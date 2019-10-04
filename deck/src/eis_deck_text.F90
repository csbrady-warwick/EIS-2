MODULE eis_deck_from_text_mod

  USE eis_deck_header
  USE eis_deck_definition_mod
  USE eis_string_deck_mod
  USE eis_parser_mod
  USE eis_error_mod
  USE eis_utils
  IMPLICIT NONE

  TYPE :: eis_text_deck_parser

    LOGICAL :: is_init = .FALSE.

    CLASS(eis_error_handler), POINTER :: err_handler => NULL()
    LOGICAL :: owns_handler = .TRUE.
    CLASS(eis_parser), POINTER :: parser => NULL()
    LOGICAL :: owns_parser = .TRUE.

    CONTAINS
    PROCEDURE :: init => tdp_init
    PROCEDURE :: parse_deck_file => tdp_parse_deck_file
    PROCEDURE :: parse_deck_object => tdp_parse_deck_object
    GENERIC :: parse_deck => parse_deck_file, parse_deck_object
    PROCEDURE :: display_blocks => tdp_display_blocks
    PROCEDURE :: call_blocks => tdp_call_blocks
    PROCEDURE :: initialise_deck => tdp_initialise_deck
    PROCEDURE :: initialize_deck => tdp_initialise_deck
    PROCEDURE :: finalise_deck => tdp_finalise_deck
    PROCEDURE :: finalize_deck => tdp_finalise_deck
    FINAL :: tdp_destructor

  END TYPE eis_text_deck_parser

  CONTAINS



  RECURSIVE SUBROUTINE tdp_display_blocks(this, block, level)
    CLASS(eis_text_deck_parser), INTENT(IN) :: this
    TYPE(eis_string_deck_block), POINTER :: block
    INTEGER, INTENT(IN) :: level
    INTEGER :: i
    CHARACTER(LEN=:), ALLOCATABLE :: key, value
    INTEGER, DIMENSION(:), ALLOCATABLE :: array

    RETURN

    IF (block%get_line_count() > 0) PRINT *,REPEAT(" ", level), "Starting ", &
        block%block_name, block%id
    CALL block%get_parents(array)
    IF (SIZE(array) > 0) THEN
      PRINT *,REPEAT(" ", level), 'Parents are ', array
    END IF
    DO i = 1, block%get_line_count()
      CALL block%get_line(i, key, value)
      PRINT *, REPEAT(" ", level), "  key:", key, "    value:", value
    END DO

    IF (block%get_child_count() > 0) PRINT *,REPEAT(" ", level), &
        "Children of ", block%block_name
    DO i = 1, block%get_child_count()
      CALL this%display_blocks(block%get_child(i), level + 1)
    END DO
    IF (block%get_child_count() > 0) PRINT *,REPEAT(" ", level), &
        "Ending children of ", block%block_name

    IF (block%get_line_count() > 0) PRINT *,REPEAT(" ", level), &
        "Ending ", block%block_name

  END SUBROUTINE tdp_display_blocks



  RECURSIVE SUBROUTINE tdp_call_blocks(this, definition, block, status, &
      host_state, errcode)
    CLASS(eis_text_deck_parser), INTENT(IN) :: this
    CLASS(eis_deck_block_definition), INTENT(INOUT) :: definition
    TYPE(eis_string_deck_block), POINTER, INTENT(IN) :: block
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER :: i
    CHARACTER(LEN=:), ALLOCATABLE :: line
    INTEGER, DIMENSION(:), ALLOCATABLE :: array
    TYPE(eis_deck_block_definition), POINTER :: cdef
    TYPE(eis_string_deck_block), POINTER :: cblock
    INTEGER(eis_status) :: this_status
    INTEGER(eis_bitmask) :: this_host
    INTEGER(eis_error) :: this_errcode
    INTEGER, DIMENSION(:), ALLOCATABLE :: block_parents
    CHARACTER(LEN=:), ALLOCATABLE :: fn
    INTEGER :: line_number

    CALL eis_default_status(this_errcode, this_status, this_host)
    CALL block%get_parents(block_parents)
    CALL definition%start_block(block_parents, this_errcode, &
        host_state = this_host, display_name=block%block_name)
    IF (this_errcode /= eis_err_none) THEN
      CALL this%err_handler%add_error(eis_err_deck_parser, this_errcode, &
          filename = block%filename, line_number = block%line_number)
    END IF
    errcode = IOR(errcode, this_errcode)
    status = IOR(status, this_status)
    host_state = IOR(host_state, this_host)

    IF (IAND(errcode, eis_err_unknown_block) /= 0 .OR. IAND(this_status, &
        eis_status_terminate) /= 0) RETURN

    DO i = 1, block%get_line_count()
      CALL block%get_line(i, line, filename = fn, line_number = line_number)
      CALL eis_default_status(this_errcode, this_status, this_host)
      CALL definition%call_key(line, block_parents, this_errcode, &
          host_state = this_host)
      errcode = IOR(errcode, this_errcode)
      status = IOR(status, this_status)
      host_state = IOR(host_state, this_host)
      IF (this_errcode /= eis_err_none) THEN
        IF (ALLOCATED(fn)) THEN
          CALL this%err_handler%add_error(eis_err_deck_parser, this_errcode, &
              filename = fn, line_number = line_number)
        ELSE
          CALL this%err_handler%add_error(eis_err_deck_parser, this_errcode)
        END IF
      END IF
      IF (IAND(this_status, eis_status_terminate) /= 0) RETURN
    END DO
    IF (ALLOCATED(line)) DEALLOCATE(line)
    IF (ALLOCATED(fn)) DEALLOCATE(fn)
    DEALLOCATE(block_parents)

    DO i = 1, block%get_child_count()
      cblock => block%get_child(i)
      CALL eis_default_status(this_errcode, this_status, this_host)
      cdef => definition%get_child(cblock%block_name, this_status, &
          this_host, this_errcode)
      IF (this_errcode /= eis_err_none) THEN
        IF (ALLOCATED(cblock%filename)) THEN
          CALL this%err_handler%add_error(eis_err_deck_parser, this_errcode, &
              filename = cblock%filename, line_number = cblock%line_number)
        ELSE
          CALL this%err_handler%add_error(eis_err_deck_parser, this_errcode)
        END IF
      END IF
      errcode = IOR(errcode, this_errcode)
      status = IOR(status, this_status)
      host_state = IOR(host_state, this_host)

      IF (this_errcode == eis_err_none) THEN
        CALL eis_default_status(this_errcode, this_status, this_host)
        CALL this%call_blocks(cdef, block%get_child(i), this_status, &
            this_host, this_errcode)
        errcode = IOR(errcode, this_errcode)
        status = IOR(status, this_status)
        host_state = IOR(host_state, this_host)
        IF (IAND(errcode, eis_err_unknown_block) /= 0 .OR. IAND(this_status, &
            eis_status_terminate) /= 0) RETURN
      END IF
    END DO

    CALL definition%end_block(block_parents, this_errcode, &
        host_state = this_host, display_name=block%block_name)

  END SUBROUTINE tdp_call_blocks


  SUBROUTINE tdp_destructor(this)
    TYPE(eis_text_deck_parser), INTENT(INOUT) :: this

    IF (this%owns_handler .AND. ASSOCIATED(this%err_handler)) &
        DEALLOCATE(this%err_handler)
    IF (this%owns_parser .AND. ASSOCIATED(this%parser)) &
        DEALLOCATE(this%parser)

  END SUBROUTINE tdp_destructor



  SUBROUTINE tdp_init(this, err_handler, parser)
    CLASS(eis_text_deck_parser), INTENT(INOUT) :: this
    TYPE(eis_error_handler), INTENT(IN), POINTER, OPTIONAL :: err_handler
    TYPE(eis_parser), INTENT(IN), POINTER, OPTIONAL :: parser

    IF (this%is_init) RETURN
    this%is_init = .TRUE.

    IF(PRESENT(err_handler)) THEN
      this%err_handler => err_handler
      this%owns_handler = .FALSE.
    ELSE
      ALLOCATE(this%err_handler)
    END IF

    IF(PRESENT(parser)) THEN
      this%parser => parser
      this%owns_parser = .FALSE.
    ELSE
      ALLOCATE(this%parser)
    END IF
  END SUBROUTINE tdp_init



  SUBROUTINE tdp_initialise_deck(this, definition, errcode, state)
    CLASS(eis_text_deck_parser), INTENT(INOUT) :: this
    TYPE(eis_deck_definition), INTENT(INOUT) :: definition
    INTEGER(eis_error), INTENT(OUT) :: errcode
    INTEGER(eis_bitmask), INTENT(INOUT), OPTIONAL :: state
    INTEGER(eis_error) :: err
    INTEGER(eis_bitmask) :: host_state

    IF (.NOT. this%is_init) CALL this%init()

    CALL eis_default_status(errcode = err, bitmask = host_state)
    !CALL definition%initialise_blocks(host_state, err)
    errcode = IOR(errcode, err)
    IF (PRESENT(state)) state = IOR(state, host_state)

  END SUBROUTINE tdp_initialise_deck



  SUBROUTINE tdp_finalise_deck(this, definition, errcode, state)
    CLASS(eis_text_deck_parser), INTENT(INOUT) :: this
    TYPE(eis_deck_definition), INTENT(INOUT) :: definition
    INTEGER(eis_error), INTENT(OUT) :: errcode
    INTEGER(eis_bitmask), INTENT(INOUT), OPTIONAL :: state
    INTEGER(eis_error) :: err
    INTEGER(eis_bitmask) :: host_state

    IF (.NOT. this%is_init) CALL this%init()

    CALL eis_default_status(errcode = err, bitmask = host_state)
    CALL definition%finalise_blocks(host_state, err)
    errcode = IOR(errcode, err)
    IF (PRESENT(state)) state = IOR(state, host_state)

  END SUBROUTINE tdp_finalise_deck



  SUBROUTINE tdp_parse_deck_object(this, string_deck, definition, errcode, &
      pass_number, max_passes, state, initialise_all_blocks)
    CLASS(eis_text_deck_parser), INTENT(INOUT) :: this
    CLASS(eis_string_deck), INTENT(IN) :: string_deck
    TYPE(eis_deck_definition), INTENT(INOUT) :: definition
    INTEGER(eis_error), INTENT(OUT) :: errcode
    INTEGER, INTENT(IN), OPTIONAL :: pass_number, max_passes
    INTEGER(eis_bitmask), INTENT(INOUT), OPTIONAL :: state
    LOGICAL, INTENT(IN), OPTIONAL :: initialise_all_blocks

    TYPE(eis_string_deck_block), POINTER :: block
    INTEGER(eis_error) :: err
    INTEGER(eis_status) :: status
    INTEGER(eis_bitmask) :: host_state
    TYPE(eis_deck_block_definition), POINTER :: bdef
    LOGICAL :: parse_over, first_pass

    first_pass = .FALSE.
    IF (PRESENT(initialise_all_blocks)) first_pass = initialise_all_blocks
    IF (PRESENT(pass_number)) first_pass = (pass_number == 1)

    IF (.NOT. this%is_init) CALL this%init()
    IF (first_pass) THEN
      CALL eis_default_status(errcode = err, bitmask = host_state)
      CALL this%initialise_deck(definition, err, state = host_state)
      errcode = IOR(errcode, err)
      IF (PRESENT(state)) state = IOR(state, host_state)
      IF (errcode /= eis_err_none) RETURN
    END IF

    block => string_deck%get_block()
    bdef => definition%get_root()
    IF (ASSOCIATED(bdef)) THEN
      CALL eis_default_status(errcode = err, bitmask = host_state, &
          status = status)
      CALL this%call_blocks(bdef, block, status, host_state, err)
      errcode = IOR(errcode, err)
      IF (PRESENT(state)) state = IOR(state, host_state)
    END IF

    parse_over = .TRUE.
    IF (PRESENT(max_passes) .AND. PRESENT(pass_number)) THEN
      parse_over = (max_passes == pass_number)
    END IF

    IF (parse_over) THEN
      CALL eis_default_status(errcode = err, bitmask = host_state)
      CALL this%finalise_deck(definition, err, state = host_state)
      IF (PRESENT(state)) state = IOR(state, host_state)
      errcode = IOR(errcode, err)
    END IF

  END SUBROUTINE tdp_parse_deck_object



  SUBROUTINE tdp_parse_deck_file(this, filename, definition, errcode, &
      pass_number, max_passes, max_level, allow_root_keys, allow_empty_blocks, &
      state, initialise_all_blocks)
    CLASS(eis_text_deck_parser), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: filename
    TYPE(eis_deck_definition), INTENT(INOUT) :: definition
    INTEGER(eis_error), INTENT(OUT) :: errcode
    INTEGER, INTENT(IN), OPTIONAL :: pass_number, max_passes
    INTEGER, INTENT(IN), OPTIONAL :: max_level
    LOGICAL, INTENT(IN), OPTIONAL :: allow_root_keys, allow_empty_blocks
    INTEGER(eis_bitmask), INTENT(INOUT), OPTIONAL :: state
    LOGICAL, INTENT(IN), OPTIONAL :: initialise_all_blocks

    TYPE(eis_string_deck) :: sdeck
    TYPE(eis_string_deck_block), POINTER :: block
    INTEGER(eis_error) :: err, host_state, status
    TYPE(eis_deck_block_definition), POINTER :: bdef
    LOGICAL :: parse_over, first_pass

    IF (.NOT. this%is_init) CALL this%init()

    CALL eis_default_status(errcode = err)
    CALL sdeck%init(err, this%err_handler)
    errcode = IOR(errcode, err)
    IF (err /= eis_err_none) RETURN

    CALL eis_default_status(errcode = err)
    CALL sdeck%load_deck_file(filename, err)
    errcode = IOR(errcode, err)
    IF (err /= eis_err_none) RETURN

    CALL eis_default_status(errcode = err)
    CALL sdeck%generate_blocklist(err, max_level, allow_root_keys, &
      allow_empty_blocks)
    errcode = IOR(errcode, err)
    IF (err /= eis_err_none) RETURN

    CALL this%parse_deck_object(sdeck, definition, err, pass_number, &
        max_passes, state = state, initialise_all_blocks &
        = initialise_all_blocks)
    errcode = IOR(errcode, err)

  END SUBROUTINE tdp_parse_deck_file

END MODULE eis_deck_from_text_mod
