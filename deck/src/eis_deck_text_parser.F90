MODULE eis_string_deck_mod

  USE eis_header
  USE eis_utils
  USE eis_key_value_store_mod
  USE eis_string_store_mod
  USE eis_error_mod
  IMPLICIT NONE

  TYPE eis_string_deck_block
    CHARACTER(LEN=:), ALLOCATABLE :: block_name, filename
    INTEGER :: line_number = 0
    INTEGER :: id = -1, parent_block = -1
    INTEGER :: level = -1
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
    PROCEDURE, PUBLIC :: get_parents => esdb_get_parents
    FINAL :: esdb_destructor
  END TYPE eis_string_deck_block

  TYPE eis_string_deck_data
    TYPE(eis_string_deck_block), DIMENSION(:), POINTER :: blocks => NULL()
    TYPE(eis_string_store) :: strings
    LOGICAL :: owns_handler = .FALSE.
    TYPE(eis_error_handler), POINTER :: handler => NULL()
    CONTAINS
    FINAL :: esdd_destructor
  END TYPE eis_string_deck_data

  TYPE eis_string_deck
    TYPE(eis_string_deck_data), POINTER :: data => NULL()
    CHARACTER(LEN=:), ALLOCATABLE :: import_prefix
    LOGICAL :: is_init = .FALSE.
    PROCEDURE(file_text_processor_proto), POINTER, NOPASS :: &
        file_text_processor => NULL()
    PROCEDURE(filename_processor_proto), POINTER, NOPASS :: &
        filename_processor => NULL()
    CONTAINS
    PRIVATE
    PROCEDURE :: parse_core => esd_parse_core
    PROCEDURE, PUBLIC :: init => esd_init
    PROCEDURE, PUBLIC :: load_deck_file => esd_parse_deck_file
    PROCEDURE, PUBLIC :: read_deck_string => esd_parse_deck_string
    PROCEDURE, PUBLIC :: read_parsed_deck => esd_read_parsed_deck
    PROCEDURE, PUBLIC :: generate_blocklist => esd_generate_blocklist
    PROCEDURE, PUBLIC :: get_block_count => esd_get_block_count
    PROCEDURE, PUBLIC :: get_block => esd_get_block
    PROCEDURE, PUBLIC :: get_block_name => esd_get_block_name
    PROCEDURE, PUBLIC :: get_error_count => esd_get_error_count
    PROCEDURE, PUBLIC :: get_error_report => esd_get_error_report
    PROCEDURE, PUBLIC :: get_error_info => esd_get_error_info
    PROCEDURE, PUBLIC :: flush_errors => esd_flush_errors
    PROCEDURE, PUBLIC :: print_errors => esd_print_errors
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
  !> Get an array containing all of the parents of this block
  !> The final element is the ID of the this block
  !> @param[out] parents
  SUBROUTINE esdb_get_parents(this, parents)
    CLASS(eis_string_deck_block), INTENT(IN) :: this
    INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: parents
    CLASS(eis_string_deck_block), POINTER :: parent
    INTEGER :: iblock

    ALLOCATE(parents(this%level+1))
    parent => this%get_parent()
    iblock = this%level
    DO WHILE(ASSOCIATED(parent))
      parents(iblock) = parent%id
      parent => parent%get_parent()
      iblock = iblock - 1
    END DO
    parents(this%level+1) = this%id

  END SUBROUTINE esdb_get_parents



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
  SUBROUTINE esdb_get_line(this, index, line, line_number, filename, &
      trimmed_white_space_length)
    CLASS(eis_string_deck_block), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: index
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: line
    INTEGER, INTENT(OUT), OPTIONAL :: line_number
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT), OPTIONAL :: filename
    INTEGER, INTENT(OUT), OPTIONAL :: trimmed_white_space_length
    INTEGER :: i, iblock, istart
    LOGICAL :: ok
    CHARACTER(LEN=:), ALLOCATABLE :: temp

    IF (.NOT. ALLOCATED(this%iends)) RETURN
    IF (index < 1 .OR. index > this%line_count) RETURN

    DO i = 1, SIZE(this%iends)
      IF (index <= this%iends(i)) THEN
        istart = this%iends(i) - (this%ends(i) - this%starts(i))
        iblock = this%starts(i) + (index - istart) - 1
        ok = this%parser_data%strings%get(iblock,line, &
            line_number = line_number, filename = temp, &
            trimmed_white_space_length = trimmed_white_space_length)
        IF (PRESENT(filename)) ALLOCATE(filename, SOURCE = temp)
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
    DO i = 2, ct_lines
      this%iends(i) = this%iends(i-1) + this%ends(i) - this%starts(i) - 1
      this%line_count = this%line_count + this%ends(i) - this%starts(i) - 1
    END DO

  END SUBROUTINE esdb_compact_lines



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Routine to initialise this object
  SUBROUTINE esd_init(this, errcode, err_handler, import_prefix, &
      filename_processor, file_text_processor)
    CLASS(eis_string_deck), INTENT(INOUT) :: this
    INTEGER(eis_error), INTENT(OUT) :: errcode
    CLASS(eis_error_handler), POINTER, INTENT(IN), OPTIONAL :: err_handler
    PROCEDURE(filename_processor_proto), OPTIONAL :: filename_processor
    PROCEDURE(file_text_processor_proto), OPTIONAL :: file_text_processor
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: import_prefix

    errcode = eis_err_none

    this%is_init = .TRUE.
    !Clean up possibly already allocated from previous init
    IF (.NOT. ASSOCIATED(this%data)) ALLOCATE(this%data)
    IF (this%data%owns_handler .AND. ASSOCIATED(this%data%handler)) &
        DEALLOCATE(this%data%handler)
    IF (ALLOCATED(this%import_prefix)) DEALLOCATE(this%import_prefix)

    IF (PRESENT(filename_processor)) this%filename_processor &
        => filename_processor
    IF (PRESENT(file_text_processor)) this%file_text_processor &
        => file_text_processor

    IF (PRESENT(err_handler)) THEN
      this%data%handler => err_handler
      this%data%owns_handler = .FALSE.
    ELSE
      ALLOCATE(this%data%handler)
      this%data%owns_handler = .TRUE.
    END IF

    IF (PRESENT(import_prefix)) THEN
      ALLOCATE(this%import_prefix, SOURCE = import_prefix)
    ELSE
      ALLOCATE(this%import_prefix, SOURCE="")
    END IF

  END SUBROUTINE esd_init


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Routine containing the core parsing information that is common
  !> between all parsing routes
  !> @param[in] errcode
  SUBROUTINE esd_parse_core(this, errcode)
    CLASS(eis_string_deck), INTENT(INOUT) :: this
    !> Error code
    INTEGER(eis_error), INTENT(OUT) :: errcode
    INTEGER :: iline, cloc, ln, lnm
    CHARACTER(LEN=:), ALLOCATABLE :: str, src_filename, import_filename, raw
    LOGICAL :: found
    INTEGER, DIMENSION(2) :: ranges

    errcode = eis_err_none
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
          iline = iline - 1 !Decrement because deleting line
          found = this%data%strings%delete(iline)
          ALLOCATE(import_filename, SOURCE = this%import_prefix &
              // TRIM(ADJUSTL(str(cloc+1:))))
          errcode = eis_err_none
          ranges = this%data%strings%load_from_ascii_file(&
              this%import_prefix // TRIM(ADJUSTL(str(cloc+1:))), errcode, &
              index_start = iline, &
              filename_processor = this%filename_processor, &
              file_text_processor = this%file_text_processor)
          IF (errcode /= eis_err_none) THEN
            IF (ALLOCATED(src_filename)) THEN
              CALL this%data%handler%add_error(eis_err_deck_file, errcode, &
                  filename = src_filename, line_number = ln)
            ELSE
              CALL this%data%handler%add_error(eis_err_deck_file, errcode)
            END IF
            RETURN
          END IF
          CALL this%data%strings%remove_comments(slc_start='#', &
              mlc_start='/*', mlc_end='*/')
          CALL this%data%strings%remove_blank_lines()
          CALL this%data%strings%remove_whitespace()
          CALL this%data%strings%combine_split_lines("\")
        ELSE
          errcode = eis_err_no_file
            IF (ALLOCATED(src_filename)) THEN
              CALL this%data%handler%add_error(eis_err_deck_file, errcode, &
                  filename = src_filename, line_number = ln)
            ELSE
              CALL this%data%handler%add_error(eis_err_deck_file, errcode)
            END IF
          RETURN
        END IF
      END IF
    END DO
  END SUBROUTINE esd_parse_core



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Parse a text file to a deck description
  !> @param[in] filename
  !> @param[in] errcode
  !> @param[out] parsed_text
  !> @param[out] raw_text
  SUBROUTINE esd_parse_deck_file(this, filename, errcode, parsed_text, raw_text)
    CLASS(eis_string_deck), INTENT(INOUT) :: this
    !> Filename of file to read
    CHARACTER(LEN=*), INTENT(IN) :: filename
    !> Error code
    INTEGER(eis_error), INTENT(OUT) :: errcode
    !> Serialised version of the loaded text. Should be used to transfer
    !> file input to other processors in an MPI setup etc.
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT), OPTIONAL :: parsed_text
    !> Raw text from the file
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT), OPTIONAL :: raw_text
    INTEGER, DIMENSION(2) :: ranges
    INTEGER(eis_error) :: ierr

    errcode = eis_err_none
    CALL this%init(ierr)
    errcode = IOR(errcode, ierr)
    ranges = this%data%strings%load_from_ascii_file(filename, errcode, &
        raw_text = raw_text, filename_processor = this%filename_processor, &
        file_text_processor = this%file_text_processor)
    IF (errcode /= eis_err_none) THEN
      CALL this%data%handler%add_error(eis_err_deck_file, errcode, &
          filename = filename)
      RETURN
    END IF

    CALL this%parse_core(ierr)
    errcode = IOR(errcode, ierr)
    IF (PRESENT(parsed_text)) THEN
      CALL this%data%strings%serialise(parsed_text)
      RETURN
    END IF

  END SUBROUTINE esd_parse_deck_file



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Parse a text file to a deck description
  !> @param[in] text
  !> @param[out] errcode
  !> @param[in] filename
  SUBROUTINE esd_parse_deck_string(this, text, errcode, filename)
    CLASS(eis_string_deck), INTENT(INOUT) :: this
    !> Text of deck file
    CHARACTER(LEN=*), INTENT(IN) :: text
    !> Error code
    INTEGER(eis_error), INTENT(OUT) :: errcode
    !> Filename to associate with the strings when reporting errors
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: filename
    INTEGER, DIMENSION(2) :: ranges
    INTEGER(eis_error) :: ierr

    errcode = eis_err_none
    CALL this%init(ierr)
    errcode = IOR(errcode, ierr)
    ranges = this%data%strings%populate(text, errcode, filename = filename)
    CALL this%parse_core(ierr)

  END SUBROUTINE esd_parse_deck_string



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Parse a serialised deck representation (from read_deck_file)
  !> @param[in] text
  !> @param[in] errcode
  SUBROUTINE esd_read_parsed_deck(this, text, errcode)
    CLASS(eis_string_deck), INTENT(INOUT) :: this
    !> Serialised deck representation
    CHARACTER(LEN=*), INTENT(IN) :: text
    !> Error code
    INTEGER(eis_error), INTENT(OUT) :: errcode
    INTEGER, DIMENSION(2) :: ranges
    INTEGER(eis_error) :: ierr

    errcode = eis_err_none
    CALL this%init(ierr)
    errcode = IOR(errcode, ierr)

    ranges = this%data%strings%deserialise(text, errcode)

    CALL this%parse_core(ierr)

  END SUBROUTINE esd_read_parsed_deck



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Generate a list of blocks from a parsed source
  !> @param[inout] this
  !> @param[out] errcode
  SUBROUTINE esd_generate_blocklist(this, errcode, max_level, allow_root_keys, &
      allow_empty_blocks)
    CLASS(eis_string_deck), INTENT(INOUT) :: this
    INTEGER(eis_error), INTENT(OUT) :: errcode
    INTEGER, INTENT(IN), OPTIONAL :: max_level
    LOGICAL, INTENT(IN), OPTIONAL :: allow_root_keys, allow_empty_blocks
    INTEGER :: iline, cloc, iline2
    LOGICAL :: ok
    CHARACTER(LEN=:), ALLOCATABLE :: str, src_filename
    INTEGER :: iblock, current_block, current_level, ln
    TYPE :: bholder
      CHARACTER(LEN=:), ALLOCATABLE :: blockname
      TYPE(bholder), POINTER :: next => NULL()
    END TYPE
    TYPE(bholder), POINTER :: head => NULL(), new
    INTEGER :: mlevel
    LOGICAL :: root_keys, empty_blocks

    mlevel = HUGE(mlevel)
    IF (PRESENT(max_level)) mlevel = max_level

    root_keys = .FALSE.
    IF (PRESENT(allow_root_keys)) root_keys = allow_root_keys

    empty_blocks = .FALSE.
    IF (PRESENT(allow_empty_blocks)) empty_blocks = allow_empty_blocks

    errcode = eis_err_none
    iblock = 0

    DO iline = 1, this%data%strings%get_size()
      ok = this%data%strings%get(iline, str, filename = src_filename, &
          line_number = ln)
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
          IF (ALLOCATED(src_filename)) THEN
            CALL this%data%handler%add_error(eis_err_deck_file, errcode, &
                filename = src_filename, line_number = ln)
          ELSE
            CALL this%data%handler%add_error(eis_err_deck_file, errcode)
          END IF
          EXIT
        END IF
        !The block that is being closed was opened but the names don't match
        !This is also an error
        IF (.NOT. eis_compare_string(TRIM(ADJUSTL(str(cloc+1:))), &
            head%blockname)) THEN
          errcode = eis_err_mismatched_begin_end
          IF (ALLOCATED(src_filename)) THEN
            CALL this%data%handler%add_error(eis_err_deck_file, errcode, &
                filename = src_filename, line_number = ln)
          ELSE
            CALL this%data%handler%add_error(eis_err_deck_file, errcode)
          END IF
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
      IF (ALLOCATED(src_filename)) THEN
        CALL this%data%handler%add_error(eis_err_deck_file, errcode, &
            filename = src_filename, line_number = ln)
      ELSE
        CALL this%data%handler%add_error(eis_err_deck_file, errcode)
      END IF
      new => head%next
      DEALLOCATE(head)
      head => new
    END DO

    IF (errcode /= eis_err_none) RETURN

    !Allocate storage for the blocks
    ALLOCATE(this%data%blocks(0:iblock))
    ALLOCATE(this%data%blocks(0)%block_name, SOURCE = '{ROOT}')
    this%data%blocks(0)%id = 0
    this%data%blocks(0)%level = 0
    CALL this%data%blocks(0)%add_line_start(0)
    DO iblock = 0, iblock
      this%data%blocks(iblock)%parser_data => this%data
    END DO
    iblock = 1
    current_block = 0
    current_level = 0

    DO iline = 1, this%data%strings%get_size()
      ok = this%data%strings%get(iline, str, filename = src_filename, &
          line_number = ln)
      cloc = INDEX(str, ":")
      IF (cloc < 1) CYCLE
      IF (eis_compare_string(TRIM(ADJUSTL(str(1:cloc-1))), 'begin' , &
          case_sensitive = .FALSE.)) THEN
        current_level = current_level + 1
        IF (current_level > mlevel) THEN
          errcode = eis_err_deck_too_deep
          IF (ALLOCATED(src_filename)) THEN
            CALL this%data%handler%add_error(eis_err_deck_file, errcode, &
                filename = src_filename, line_number = ln)
          ELSE
            CALL this%data%handler%add_error(eis_err_deck_file, errcode)
          END IF
          RETURN
        END IF
        !The parent block now has a child block and has a line end added
        CALL this%data%blocks(current_block)%add_child(iblock)
        CALL this%data%blocks(current_block)%add_line_end(iline)

        !Create the information on this block
        ALLOCATE(this%data%blocks(iblock)%block_name, &
            SOURCE = TRIM(ADJUSTL(str(cloc+1:))))
        this%data%blocks(iblock)%id = iblock
        this%data%blocks(iblock)%parent_block = current_block
        this%data%blocks(iblock)%level = current_level
        ALLOCATE(this%data%blocks(iblock)%filename, SOURCE = src_filename)
        this%data%blocks(iblock)%line_number = ln
        !Mark the current line as the start line for the current block
        CALL this%data%blocks(iblock)%add_line_start(iline)

        !The parent block for any further sub-blocks is now the current block
        !index
        current_block = iblock
        iblock = iblock + 1
      ELSE IF (eis_compare_string(TRIM(ADJUSTL(str(1:cloc-1))), 'end' , &
          case_sensitive = .FALSE.)) THEN
        current_level = current_level - 1
        !Mark the text for the current block as ended
        CALL this%data%blocks(current_block)%add_line_end(iline)
        !Wind the current block back up a level
        current_block = this%data%blocks(current_block)%parent_block
        CALL this%data%blocks(current_block)%add_line_start(iline)
      END IF
    END DO
    !Finally, end the root block
    CALL this%data%blocks(0)%add_line_end(iline)

    DO iblock = 0, UBOUND(this%data%blocks, 1)
      CALL this%data%blocks(iblock)%compact_lines()
      IF (iblock > 0 .AND. .NOT. empty_blocks) THEN
        IF (this%data%blocks(iblock)%get_line_count() < 1) THEN
          errcode = eis_err_deck_empty_block
          IF (ALLOCATED(src_filename)) THEN
            CALL this%data%handler%add_error(eis_err_deck_file, errcode, &
                filename = src_filename, line_number = ln)
          ELSE
            CALL this%data%handler%add_error(eis_err_deck_file, errcode)
          END IF
          RETURN
        END IF
      END IF
    END DO

    IF (.NOT. root_keys .AND. this%data%blocks(0)%get_line_count() > 0) THEN
      errcode = eis_err_root_keys
      IF (ALLOCATED(src_filename)) THEN
        CALL this%data%handler%add_error(eis_err_deck_file, errcode, &
            filename = src_filename, line_number = ln)
      ELSE
        CALL this%data%handler%add_error(eis_err_deck_file, errcode)
      END IF
    END IF

  END SUBROUTINE esd_generate_blocklist



  !> @brief
  !> Print all of the errors to stdout
  !> @param[inout] this
  SUBROUTINE esd_print_errors(this)
    CLASS(eis_string_deck), INTENT(IN) :: this
    INTEGER :: ierr

    DO ierr = 1, this%data%handler%get_error_count()
      CALL this%data%handler%print_error_string(ierr)
    END DO
    CALL this%data%handler%flush_errors()

  END SUBROUTINE esd_print_errors



  !> @brief
  !> Flush all of the errors in the list of errors
  !> @param[inout] this
  SUBROUTINE esd_flush_errors(this)
    CLASS(eis_string_deck) :: this
    CALL this%data%handler%flush_errors()
  END SUBROUTINE esd_flush_errors



  !> @brief
  !> Get the number of errors reported on this deck
  !> @param[inout] this
  !> @return count
  FUNCTION esd_get_error_count(this) RESULT(count)
    CLASS(eis_string_deck), INTENT(IN) :: this
    INTEGER :: count !< Number of errors reported

    count = this%data%handler%get_error_count()

  END FUNCTION esd_get_error_count



  !> @brief
  !> Get the error report on a specified error
  !> @param[inout] this
  !> @param[in] index
  !> @param[out] report
  SUBROUTINE esd_get_error_report(this, index, report)
    CLASS(eis_string_deck), INTENT(IN) :: this
    !> Index of error to get report on. Must be between 1 and
    !> the result of esd_get_error_count
    INTEGER, INTENT(IN) :: index
    !> Allocatable string variable containing the error report
    !> will be reallocated to be exactly long enough to store
    !> the error report whether allocated or not
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: report

    CALL this%data%handler%get_error_report(index, report)
  END SUBROUTINE esd_get_error_report


  !> @brief
  !> Get error information for a single error. This contains the 
  !> same information as the error report but is not formatted for use
  !> @param[inout] this
  !> @param[in] index
  !> @param[out] error_cause
  !> @param[out] error_cause_location
  !> @param[out] error_phase
  !> @param[out] error_type
  !> @param[out] error_filename
  !> @param[out] error_line_number
  SUBROUTINE esd_get_error_info(this, index, error_cause, &
      error_cause_location, error_phase, error_type, error_filename, &
      error_line_number)

    CLASS(eis_string_deck), INTENT(IN) :: this
    !> Index of error to get report on. Must be between 1 and
    !> the result of esd_get_error_count
    INTEGER, INTENT(IN) :: index
    !> Reports the string representation of the part of the string that
    !> caused the error. Optional, default is not return this info
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT) :: error_cause
    !> Reports the character offset location of the cause of the error
    !> Optional, default is not report this info
    INTEGER, INTENT(OUT), OPTIONAL :: error_cause_location
    !> Reports the string representation of where in the parsing phase the
    !> error occured. Optional, default is not report this info
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT) :: error_phase
    !> Reports the string representation of the type of error that was
    !> encountered during the parsing. Optional, default is no report this info
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT) :: error_type
    !> Reports the file name of the file which contains the error
    !> Optional, default is not report this info
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT) :: error_filename
    !> Reports the line number within the file which contains the error
    !> Optional, default is not report this info
    INTEGER, INTENT(OUT), OPTIONAL :: error_line_number
    CHARACTER(LEN=:), ALLOCATABLE :: ec, eps, ets, efn
    INTEGER :: ecl, eln

    CALL this%data%handler%get_error_cause(index, ec, ecl,filename=efn, &
        line_number = eln)
    CALL this%data%handler%get_error_string(index, ets, eps)

    IF (PRESENT(error_cause)) CALL MOVE_ALLOC(ec, error_cause)
    IF (PRESENT(error_cause_location)) error_cause_location = ecl
    IF (PRESENT(error_phase)) CALL MOVE_ALLOC(eps, error_phase)
    IF (PRESENT(error_type)) CALL MOVE_ALLOC(ets, error_type)
    IF (PRESENT(error_filename)) CALL MOVE_ALLOC(efn, error_type)
    IF (PRESENT(error_line_number)) error_line_number = eln

    IF (ALLOCATED(ec)) DEALLOCATE(ec)
    IF (ALLOCATED(eps)) DEALLOCATE(eps)
    IF (ALLOCATED(ets)) DEALLOCATE(ets)

  END SUBROUTINE esd_get_error_info



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


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get a block name by index.
  !> @detail
  !> The actual block indices run from 0 to the value of get_block_count
  !> Item 0 is the root item.
  !> @param[in] this
  !> @param[in] index
  !> @param[out] name
  SUBROUTINE esd_get_block_name(this, index, name)
    CLASS(eis_string_deck), INTENT(IN) :: this
    !> Index of block to get. Optional, default 0 (root item)
    INTEGER, INTENT(IN), OPTIONAL :: index
    !> Name of the found block
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: name
    CLASS(eis_string_deck_block), POINTER :: block

    block => this%get_block(index)
    IF (ASSOCIATED(block)) ALLOCATE(name, SOURCE = block%block_name)

  END SUBROUTINE esd_get_block_name



  PURE ELEMENTAL SUBROUTINE esdb_destructor(this)
    TYPE(eis_string_deck_block), INTENT(INOUT) :: this
    !This shouldn't be needed but there appears to be a bug in gfortran
    !That means that POINTER arrays of TYPES having ALLOCATABLE components
    !Don't deallocate the ALLOCATABLES
    IF (ALLOCATED(this%block_name)) DEALLOCATE(this%block_name)
    IF (ALLOCATED(this%filename)) DEALLOCATE(this%filename)
    IF (ALLOCATED(this%starts)) DEALLOCATE(this%starts)
    IF (ALLOCATED(this%iends)) DEALLOCATE(this%iends)
    IF (ALLOCATED(this%children)) DEALLOCATE(this%children)
  END SUBROUTINE esdb_destructor



  PURE ELEMENTAL SUBROUTINE esdd_destructor(this)
    TYPE(eis_string_deck_data), INTENT(INOUT) :: this
    IF (ASSOCIATED(this%blocks)) DEALLOCATE(this%blocks)
    IF (this%owns_handler .AND. ASSOCIATED(this%handler)) &
        DEALLOCATE(this%handler)
  END SUBROUTINE esdd_destructor



  PURE ELEMENTAL SUBROUTINE esd_destructor(this)
    TYPE(eis_string_deck), INTENT(INOUT) :: this
    IF (ASSOCIATED(this%data)) DEALLOCATE(this%data)
  END SUBROUTINE esd_destructor

END MODULE eis_string_deck_mod
