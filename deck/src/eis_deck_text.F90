MODULE eis_string_deck_mod

  USE eis_header
  USE eis_utils
  USE eis_key_value_store_mod
  USE eis_string_store_mod
  IMPLICIT NONE

  TYPE eis_string_deck_block
    CHARACTER(LEN=:), ALLOCATABLE :: block_name
    INTEGER :: id = -1, parent_block = -1
    INTEGER :: line_start = -1, line_end = -1
    INTEGER, DIMENSION(:), ALLOCATABLE :: starts, ends
    INTEGER, DIMENSION(:), ALLOCATABLE :: children

    CONTAINS
    PRIVATE 
    PROCEDURE, PUBLIC :: add_child => esdb_add_child
    PROCEDURE, PUBLIC :: add_line_start => esdb_add_line_start
    PROCEDURE, PUBLIC :: add_line_end => esdb_add_line_end
  END TYPE eis_string_deck_block

  TYPE eis_string_deck
    TYPE(eis_string_deck_block), DIMENSION(:), ALLOCATABLE :: blocks
    TYPE(eis_string_store) :: strings
    INTEGER :: current_parent_block = 0

    CONTAINS
    PRIVATE
    PROCEDURE, PUBLIC :: load_deck_file => esd_parse_deck_file
    PROCEDURE, PUBLIC :: generate_blocklist => esd_generate_blocklist
  END TYPE eis_string_deck

  CONTAINS

  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add a child block to a block by index
  !> @param[in] index
  SUBROUTINE esdb_add_child(this, index)
    CLASS(eis_string_deck_block), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: index
    INTEGER :: sz
    INTEGER, DIMENSION(:), ALLOCATABLE :: temp

    IF (ALLOCATED(this%children)) THEN
      sz = SIZE(this%children)
      ALLOCATE(temp(1:sz + 1))
      temp(1:SIZE(this%children)) = this%children
      temp(sz + 1) = index
      DEALLOCATE(this%children)
      CALL MOVE_ALLOC(temp, this%children)
    ELSE
      ALLOCATE(this%children(1))
      this%children(1) = index
    END IF

  END SUBROUTINE esdb_add_child



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> @Add a line start marker
  !> @param[in] start_index
  SUBROUTINE esdb_add_line_start(this, start_index)
    CLASS(eis_string_deck_block), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: start_index
    INTEGER :: sz
    INTEGER, DIMENSION(:), ALLOCATABLE :: temp

    IF (ALLOCATED(this%starts)) THEN
      sz = SIZE(this%starts)
      ALLOCATE(temp(1:sz + 1))
      temp(1:sz) = this%starts
      temp(sz + 1) = start_index
      DEALLOCATE(this%starts)
      CALL MOVE_ALLOC(temp, this%starts)

      ALLOCATE(temp(1:sz + 1))
      temp(1:sz) = this%ends
      temp(sz+1) = -1
      DEALLOCATE(this%ends)
      CALL MOVE_ALLOC(temp, this%ends)
    ELSE
      ALLOCATE(this%starts(1))
      this%starts(1) = start_index
      ALLOCATE(this%ends(1))
      this%ends(1) = -1
    END IF

  END SUBROUTINE esdb_add_line_start



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> @Add a line end marker
  !> @param[in] end_index
  SUBROUTINE esdb_add_line_end(this, end_index)
    CLASS(eis_string_deck_block), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: end_index

    IF (.NOT. ALLOCATED(this%ends)) RETURN
    this%ends(SIZE(this%ends)) = end_index

  END SUBROUTINE esdb_add_line_end



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Parse a text file to a deck description
  !> @param[in] filename
  !> @param[in] errcode
  !> @param[out] parsed_text
  SUBROUTINE esd_parse_deck_file(this, filename, errcode, parsed_text)
    CLASS(eis_string_deck), INTENT(INOUT) :: this
    !> Filename of file to read
    CHARACTER(LEN=*), INTENT(IN) :: filename
    !> Error code
    INTEGER(eis_error), INTENT(OUT) :: errcode
    !> Serialised version of the loaded text. Should be used to transfer
    !> file input to other processors in an MPI setup etc.
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT), OPTIONAL :: parsed_text
    INTEGER, DIMENSION(2) :: ranges
    INTEGER :: iline, ln, lnm, cloc
    CHARACTER(LEN=:), ALLOCATABLE :: str, src_filename
    LOGICAL :: found

    errcode = eis_err_none
    ranges = this%strings%load_from_ascii_file(filename, errcode)
    CALL this%strings%remove_comments(slc_start='#', mlc_start='/*', &
        mlc_end='*/')
    CALL this%strings%remove_blank_lines()
    CALL this%strings%remove_whitespace()
    CALL this%strings%combine_split_lines("\")
    iline = 1
    DO WHILE (iline <= this%strings%get_size())
      found = this%strings%get(iline, str, line_number = ln, line_number_max &
          = lnm, filename = src_filename)
      iline = iline + 1
      cloc = INDEX(str, ":")
      IF (cloc < 1) CYCLE
      IF (eis_compare_string(TRIM(ADJUSTL(str(1:cloc-1))), 'import' , &
          case_sensitive = .FALSE.)) THEN
        IF (cloc < LEN(str)) THEN
          iline = iline -1 !Decrement because deleting line
          found = this%strings%delete(iline)
          ranges = this%strings%load_from_ascii_file(&
              TRIM(ADJUSTL(str(cloc+1:))), errcode, index_start = iline)
          IF (errcode /= eis_err_none) RETURN
          CALL this%strings%remove_comments(slc_start='#', mlc_start='/*', &
              mlc_end='*/')
          CALL this%strings%remove_blank_lines()
          CALL this%strings%remove_whitespace()
          CALL this%strings%combine_split_lines("\")
        ELSE
          errcode = eis_err_no_file
          RETURN
        END IF
      END IF
    END DO

    IF (PRESENT(parsed_text)) CALL this%strings%serialise(parsed_text)

    CALL this%generate_blocklist(errcode)

    DO iline = 1, this%strings%get_size()
      found = this%strings%get(iline, str, line_number = ln, line_number_max &
          = lnm, filename = src_filename)
      IF (ln == lnm) THEN
        PRINT '("Line ", I3," of file ", A, " : ",A)',ln, src_filename, str
      ELSE
        PRINT '("Lines ", I3, " - ", I3," of file ", A, " : ",A)',ln, &
            lnm, src_filename, str
      END IF
    END DO

  END SUBROUTINE esd_parse_deck_file



  SUBROUTINE esd_generate_blocklist(this, errcode)
    CLASS(eis_string_deck), INTENT(INOUT) :: this
    INTEGER(eis_error), INTENT(OUT) :: errcode
    INTEGER :: iline, cloc
    LOGICAL :: ok
    CHARACTER(LEN=:), ALLOCATABLE :: str
    INTEGER :: iblock, parent_block, current_block
    TYPE :: bholder
      CHARACTER(LEN=:), ALLOCATABLE :: blockname
      TYPE(bholder), POINTER :: next => NULL()
    END TYPE
    TYPE(bholder), POINTER :: head => NULL(), new

    errcode = eis_err_none

    DO iline = 1, this%strings%get_size()
      ok = this%strings%get(iline, str)
      cloc = INDEX(str, ":")
      IF (cloc < 1) CYCLE
      IF (eis_compare_string(TRIM(ADJUSTL(str(1:cloc-1))), 'begin' , &
          case_sensitive = .FALSE.)) THEN
        iblock = iblock + 1
        !Create a new item to hold the block name
        ALLOCATE(new)
        ALLOCATE(new%blockname, SOURCE = TRIM(ADJUSTL(str(cloc+1:))))
        !Put it at the start of the linked list
        new%next => head
        head => new
        PRINT *, "Beginning ", TRIM(ADJUSTL(str(cloc+1:)))
      END IF
      IF (eis_compare_string(TRIM(ADJUSTL(str(1:cloc-1))), 'end' , &
          case_sensitive = .FALSE.)) THEN
        !If this happens then you are closing a block that was never opened
        !This is an error
        IF (.NOT. ASSOCIATED(head)) THEN
          errcode = eis_err_mismatched_begin_end
          EXIT
        END IF
        !The block that is being closed was opened but the names don't match
        !This is also an error
        IF (.NOT. eis_compare_string(TRIM(ADJUSTL(str(cloc+1:))), &
            head%blockname)) THEN
          errcode = eis_err_mismatched_begin_end
          EXIT
        END IF

        !Unlink and deallocate the block name record
        DEALLOCATE(head%blockname)
        new => head%next
        DEALLOCATE(head)
        head => new

        PRINT *, "Ending ", TRIM(ADJUSTL(str(cloc+1:)))
      END IF
    END DO

    !If this loop ever activates then there were blocks that were opened
    !but never closed. This is also an error
    DO WHILE(ASSOCIATED(head))
      errcode = eis_err_mismatched_begin_end
      new => head%next
      DEALLOCATE(head)
      head => new
    END DO

    IF (errcode /= eis_err_none) RETURN

    !Allocate storage for the blocks
    ALLOCATE(this%blocks(0:iblock))
    parent_block = 0
    iblock = 1
    current_block = 0

    DO iline = 1, this%strings%get_size()
      ok = this%strings%get(iline, str)
      cloc = INDEX(str, ":")
      IF (cloc < 1) CYCLE
      IF (eis_compare_string(TRIM(ADJUSTL(str(1:cloc-1))), 'begin' , &
          case_sensitive = .FALSE.)) THEN
        !The parent block now has a child block and has a line end added
        CALL this%blocks(parent_block)%add_child(iblock)
        CALL this%blocks(parent_block)%add_line_end(iline)

        !Create the information on this block
        ALLOCATE(this%blocks(iblock)%block_name, &
            SOURCE = TRIM(ADJUSTL(str(cloc+1:))))
        this%blocks(iblock)%id = iblock
        this%blocks(iblock)%parent_block = parent_block
        !Mark the current line as the start line for the current block
        CALL this%blocks(iblock)%add_line_start(iline)
        this%blocks(iblock)%line_start = iline

        !The parent block for any further sub-blocks is now the current block
        !index
        parent_block = iblock
        current_block = iblock
        iblock = iblock + 1
      ELSE IF (eis_compare_string(TRIM(ADJUSTL(str(1:cloc-1))), 'end' , &
          case_sensitive = .FALSE.)) THEN
        !Mark the text for the current block as ended
        CALL this%blocks(iblock)%add_line_end(iline)
        CALL this%blocks(parent_block)%add_line_start(iline)
        this%blocks(iblock)%line_end = iline
        !Wind the parent block back up a level
        parent_block = this%blocks(parent_block)%parent_block
      END IF
    END DO

  DO iblock = 1, SIZE(this%blocks)
    PRINT *,"Starting :", this%blocks(iblock)%block_name
    PRINT *, this%blocks(iblock)%line_start + 1
    PRINT *, this%blocks(iblock)%line_end - 1
    DO iline = this%blocks(iblock)%line_start + 1, &
        this%blocks(iblock)%line_end - 1
      ok = this%strings%get(iline, str)
      PRINT *, str
    END DO
    PRINT *,"Ending :", this%blocks(iblock)%block_name
  END DO

  END SUBROUTINE esd_generate_blocklist


END MODULE eis_string_deck_mod
