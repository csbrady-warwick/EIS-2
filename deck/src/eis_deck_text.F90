MODULE eis_string_deck_mod

  USE eis_header
  USE eis_utils
  USE eis_key_value_store_mod
  USE eis_string_store_mod
  IMPLICIT NONE

  TYPE eis_string_deck_block
    CHARACTER(LEN=:), ALLOCATABLE :: block_name
    INTEGER :: id = -1, parent_block = -1
    INTEGER :: line_count = 0
    INTEGER, DIMENSION(:), ALLOCATABLE :: starts, ends, iends
    INTEGER, DIMENSION(:), ALLOCATABLE :: children
    TYPE(eis_string_deck_data), POINTER :: parser_data => NULL()

    CONTAINS
    PRIVATE 
    PROCEDURE :: add_child => esdb_add_child
    PROCEDURE :: add_line_start => esdb_add_line_start
    PROCEDURE :: add_line_end => esdb_add_line_end
    PROCEDURE :: compact_lines => esdb_compact_lines
    PROCEDURE :: get_line_full => esdb_get_line
    PROCEDURE :: get_line_split => esdb_get_line_split
    PROCEDURE, PUBLIC :: get_name => esdb_get_name
    PROCEDURE, PUBLIC :: get_parent => esdb_get_parent
    PROCEDURE, PUBLIC :: get_child_count => esdb_get_child_count
    PROCEDURE, PUBLIC :: get_child => esdb_get_child
    PROCEDURE, PUBLIC :: get_line_count => esdb_get_line_count
    GENERIC, PUBLIC :: get_line => get_line_full, get_line_split
  END TYPE eis_string_deck_block

  TYPE eis_string_deck_data
    TYPE(eis_string_deck_block), DIMENSION(:), POINTER :: blocks => NULL()
    TYPE(eis_string_store) :: strings
    CONTAINS
    FINAL :: esdd_destructor
  END TYPE eis_string_deck_data

  TYPE eis_string_deck
    TYPE(eis_string_deck_data), POINTER :: data => NULL()
    CONTAINS
    PRIVATE
    PROCEDURE, PUBLIC :: load_deck_file => esd_parse_deck_file
    PROCEDURE, PUBLIC :: generate_blocklist => esd_generate_blocklist
    PROCEDURE, PUBLIC :: get_block_count => esd_get_block_count
    PROCEDURE, PUBLIC :: get_block => esd_get_block
    FINAL :: esd_destructor
  END TYPE eis_string_deck

  CONTAINS

  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get the name of this block
  !> @param[out] name
  SUBROUTINE esdb_get_name(this, name)
    CLASS(eis_string_deck_block), INTENT(INOUT) :: this
    !> Name of this block
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: name
    IF (ALLOCATED(this%block_name)) ALLOCATE(name, SOURCE = this%block_name)
  END SUBROUTINE esdb_get_name



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get the parent of this block
  !> @result parent
  FUNCTION esdb_get_parent(this) RESULT(parent)
    CLASS(eis_string_deck_block), INTENT(IN) :: this
    !>The block that is the parent of the current block
    !>Is NULL if this block is the root block
    CLASS(eis_string_deck_block), POINTER :: parent

    IF (this%parent_block >= 0) THEN
      parent => this%parser_data%blocks(this%parent_block)
    ELSE
      parent => NULL()
    END IF
  END FUNCTION esdb_get_parent



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get the number of children for this block
  !> @result child_count
  FUNCTION esdb_get_child_count(this) RESULT(child_count)
    CLASS(eis_string_deck_block), INTENT(IN) :: this
    !> Number of child blocks of this block
    !> Will return 0 if there are no children
    INTEGER :: child_count

    IF (ALLOCATED(this%children)) THEN
      child_count = SIZE(this%children)
    ELSE
      child_count = 0
    END IF
  END FUNCTION esdb_get_child_count



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get a child block of this block
  !> @param[in] index
  !> @result child
  FUNCTION esdb_get_child(this, index) RESULT(child)
    CLASS(eis_string_deck_block), INTENT(IN) :: this
    !> Index number of child to return. Must be between 0 and
    !> this%get_child_count, returns NULL if outside this range
    INTEGER, INTENT(IN) :: index
    CLASS(eis_string_deck_block), POINTER :: child

    IF (ALLOCATED(this%children)) THEN
      IF (index >= 1 .AND. index <= this%get_child_count()) THEN
        child => this%parser_data%blocks(this%children(index))
      ELSE
        child => NULL()
      END IF
    ELSE
      child => NULL()
    END IF
  END FUNCTION esdb_get_child



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get the number of lines for this block
  !> @result count
  FUNCTION esdb_get_line_count(this) RESULT(count)
    CLASS(eis_string_deck_block), INTENT(IN) :: this
    !> Number of lines in this block
    INTEGER :: count

    count = this%line_count

  END FUNCTION esdb_get_line_count




  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get a specified line of text
  !> @param[in] index
  !> @param[out] line
  SUBROUTINE esdb_get_line(this, index, line)
    CLASS(eis_string_deck_block), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: index
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: line
    INTEGER :: i, iblock, istart
    LOGICAL :: ok

    IF (.NOT. ALLOCATED(this%iends)) RETURN
    IF (index < 1 .OR. index > this%line_count) RETURN

    DO i = 1, SIZE(this%iends)
      IF (index <= this%iends(i)) THEN
        istart = this%iends(i) - (this%ends(i) - this%starts(i))
        iblock = this%starts(i) + (index - istart) - 1
        ok = this%parser_data%strings%get(iblock,line)
        EXIT
      END IF
    END DO

  END SUBROUTINE esdb_get_line




  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get a specified line of text
  !> @param[in] index
  !> @param[out] key, value
  SUBROUTINE esdb_get_line_split(this, index, key, value)
    CLASS(eis_string_deck_block), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: index
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: key, value
    CHARACTER(LEN=:), ALLOCATABLE :: str
    INTEGER :: i

    IF (.NOT. ALLOCATED(this%iends)) RETURN
    IF (index < 1 .OR. index > this%line_count) RETURN

    CALL this%get_line(index, str)
    IF (.NOT. ALLOCATED(str)) RETURN

    i = SCAN(str, "=:")
    IF (i < 0) RETURN
    ALLOCATE(key, SOURCE = str(:i-1))
    ALLOCATE(value, SOURCE = str(i+1:))

  END SUBROUTINE esdb_get_line_split



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
  !> Add a line start marker
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
  !> Add a line end marker
  !> @param[in] end_index
  SUBROUTINE esdb_add_line_end(this, end_index)
    CLASS(eis_string_deck_block), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: end_index

    IF (.NOT. ALLOCATED(this%ends)) RETURN
    this%ends(SIZE(this%ends)) = end_index

  END SUBROUTINE esdb_add_line_end



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Compact the representation of line markers by removing empty section
  !> and
  SUBROUTINE esdb_compact_lines(this)
    CLASS(eis_string_deck_block), INTENT(INOUT) :: this
    INTEGER, DIMENSION(:), ALLOCATABLE :: t1, t2
    INTEGER :: i, ct_lines, cline

    IF (.NOT. ALLOCATED(this%starts)) RETURN

    !Find the non-empty line sections
    ct_lines = 0
    DO i = 1, SIZE(this%starts)
      IF (this%ends(i) - this%starts(i) > 1) THEN
        ct_lines = ct_lines + 1
      END IF
    END DO

    IF (ct_lines == 0) THEN
      IF (ALLOCATED(this%starts)) DEALLOCATE(this%starts)
      IF (ALLOCATED(this%ends)) DEALLOCATE(this%ends)
      this%line_count = 0
      RETURN
    END IF

    !Copy the non-empty sections into new arrays and overwrite the existing
    !arrays
    ALLOCATE(t1(ct_lines), t2(ct_lines))
    cline = 1
    DO i = 1, SIZE(this%starts)
      IF (this%ends(i) - this%starts(i) > 1) THEN
        t1(cline) = this%starts(i)
        t2(cline) = this%ends(i)
        cline = cline + 1
      END IF
    END DO
    DEALLOCATE(this%starts, this%ends)
    CALL MOVE_ALLOC(t1, this%starts)
    CALL MOVE_ALLOC(t2, this%ends)

    ALLOCATE(this%iends(ct_lines))
    this%iends(1) = this%ends(1) - this%starts(1) - 1
    this%line_count = this%ends(1) - this%starts(1) - 1
    DO i = 1, ct_lines - 1
      this%iends(i+1) = this%iends(i) + this%ends(i) - this%starts(i) - 2
      this%line_count = this%line_count + this%ends(i+1) - this%starts(i+1) - 1
    END DO

  END SUBROUTINE esdb_compact_lines



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

    IF (ASSOCIATED(this%data)) DEALLOCATE(this%data)
    ALLOCATE(this%data)

    errcode = eis_err_none
    ranges = this%data%strings%load_from_ascii_file(filename, errcode)
    CALL this%data%strings%remove_comments(slc_start='#', mlc_start='/*', &
        mlc_end='*/')
    CALL this%data%strings%remove_blank_lines()
    CALL this%data%strings%remove_whitespace()
    CALL this%data%strings%combine_split_lines("\")
    iline = 1
    DO WHILE (iline <= this%data%strings%get_size())
      found = this%data%strings%get(iline, str, line_number = ln, &
          line_number_max = lnm, filename = src_filename)
      iline = iline + 1
      cloc = INDEX(str, ":")
      IF (cloc < 1) CYCLE
      IF (eis_compare_string(TRIM(ADJUSTL(str(1:cloc-1))), 'import' , &
          case_sensitive = .FALSE.)) THEN
        IF (cloc < LEN(str)) THEN
          iline = iline -1 !Decrement because deleting line
          found = this%data%strings%delete(iline)
          ranges = this%data%strings%load_from_ascii_file(&
              TRIM(ADJUSTL(str(cloc+1:))), errcode, index_start = iline)
          IF (errcode /= eis_err_none) RETURN
          CALL this%data%strings%remove_comments(slc_start='#', &
              mlc_start='/*', mlc_end='*/')
          CALL this%data%strings%remove_blank_lines()
          CALL this%data%strings%remove_whitespace()
          CALL this%data%strings%combine_split_lines("\")
        ELSE
          errcode = eis_err_no_file
          RETURN
        END IF
      END IF
    END DO

    IF (PRESENT(parsed_text)) CALL this%data%strings%serialise(parsed_text)

    CALL this%generate_blocklist(errcode)

  END SUBROUTINE esd_parse_deck_file



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Generate a list of blocks from a parsed source
  !> @param[inout] this
  !> @param[out] errcode
  SUBROUTINE esd_generate_blocklist(this, errcode)
    CLASS(eis_string_deck), INTENT(INOUT) :: this
    INTEGER(eis_error), INTENT(OUT) :: errcode
    INTEGER :: iline, cloc, iline2
    LOGICAL :: ok
    CHARACTER(LEN=:), ALLOCATABLE :: str
    INTEGER :: iblock, current_block
    TYPE :: bholder
      CHARACTER(LEN=:), ALLOCATABLE :: blockname
      TYPE(bholder), POINTER :: next => NULL()
    END TYPE
    TYPE(bholder), POINTER :: head => NULL(), new

    errcode = eis_err_none
    iblock = 0

    DO iline = 1, this%data%strings%get_size()
      ok = this%data%strings%get(iline, str)
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
    ALLOCATE(this%data%blocks(0:iblock))
    ALLOCATE(this%data%blocks(0)%block_name, SOURCE = '{ROOT}')
    this%data%blocks(0)%id = 0
    DO iblock = 0, iblock
      this%data%blocks(iblock)%parser_data => this%data
    END DO
    iblock = 1
    current_block = 0

    DO iline = 1, this%data%strings%get_size()
      ok = this%data%strings%get(iline, str)
      cloc = INDEX(str, ":")
      IF (cloc < 1) CYCLE
      IF (eis_compare_string(TRIM(ADJUSTL(str(1:cloc-1))), 'begin' , &
          case_sensitive = .FALSE.)) THEN
        !The parent block now has a child block and has a line end added
        CALL this%data%blocks(current_block)%add_child(iblock)
        CALL this%data%blocks(current_block)%add_line_end(iline)

        !Create the information on this block
        ALLOCATE(this%data%blocks(iblock)%block_name, &
            SOURCE = TRIM(ADJUSTL(str(cloc+1:))))
        this%data%blocks(iblock)%id = iblock
        this%data%blocks(iblock)%parent_block = current_block
        !Mark the current line as the start line for the current block
        CALL this%data%blocks(iblock)%add_line_start(iline)

        !The parent block for any further sub-blocks is now the current block
        !index
        current_block = iblock
        iblock = iblock + 1
      ELSE IF (eis_compare_string(TRIM(ADJUSTL(str(1:cloc-1))), 'end' , &
          case_sensitive = .FALSE.)) THEN
        !Mark the text for the current block as ended
        CALL this%data%blocks(current_block)%add_line_end(iline)
        !Wind the current block back up a level
        current_block = this%data%blocks(current_block)%parent_block
        CALL this%data%blocks(current_block)%add_line_start(iline)
      END IF
    END DO

    DO iblock = 0, UBOUND(this%data%blocks, 1)
      CALL this%data%blocks(iblock)%compact_lines()
    END DO

  END SUBROUTINE esd_generate_blocklist


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get the total number of blocks in a parsed sequence
  !> @detail
  !> This function will return 0 if there is a parsed file
  !> but no blocks were found but -1 if no file has been parsed.
  !> The actual block indices run from 0 to the value of this function,
  !> @param[in] this
  !> @result block_count
  FUNCTION esd_get_block_count(this) RESULT(block_count)
    CLASS(eis_string_deck), INTENT(IN) :: this
    !> The number of blocks not including the 0 root block
    INTEGER :: block_count

    IF (.NOT. ASSOCIATED(this%data%blocks)) THEN
      block_count = -1
      RETURN
    END IF

    block_count = UBOUND(this%data%blocks, 1)

  END FUNCTION esd_get_block_count



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get a block by index. If no index is specified then return the root
  !> object with index 0
  !> @detail
  !> The actual block indices run from 0 to the value of get_block_count
  !> Item 0 is the root item. It is normally expected that blocks will be
  !> parsed by getting item 0 and then using the get_child functions
  !> @param[in] this
  !> @param[in] index
  !> @result block
  FUNCTION esd_get_block(this, index) RESULT(block)
    CLASS(eis_string_deck), INTENT(IN) :: this
    !> Index of block to get. Optional, default 0 (root item)
    INTEGER, INTENT(IN), OPTIONAL :: index
    CLASS(eis_string_deck_block), POINTER :: block
    INTEGER :: idx

    IF (.NOT. ASSOCIATED(this%data%blocks)) THEN
      block => NULL()
      RETURN
    END IF

    idx = 0
    IF (PRESENT(index)) idx = index
    IF (idx < 0 .OR. idx > UBOUND(this%data%blocks,1)) THEN
      block => NULL()
      RETURN
    END IF

    block => this%data%blocks(idx)

  END FUNCTION esd_get_block


  PURE ELEMENTAL SUBROUTINE esdd_destructor(this)
    TYPE(eis_string_deck_data), INTENT(INOUT) :: this
    IF (ASSOCIATED(this%blocks)) DEALLOCATE(this%blocks)
  END SUBROUTINE esdd_destructor



  PURE ELEMENTAL SUBROUTINE esd_destructor(this)
    TYPE(eis_string_deck), INTENT(INOUT) :: this
    IF (ASSOCIATED(this%data)) DEALLOCATE(this%data)
  END SUBROUTINE esd_destructor

END MODULE eis_string_deck_mod
