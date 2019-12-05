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
    INTEGER :: interop_parser = -1
    LOGICAL :: owns_interop = .TRUE.
    TYPE(eis_string_deck), ALLOCATABLE :: sdeck

    LOGICAL :: unknown_block_is_fatal = .TRUE.
    LOGICAL :: unknown_key_is_fatal = .TRUE.
    LOGICAL :: bad_key_is_fatal = .TRUE.

    CONTAINS
    PROCEDURE :: init => tdp_init
    PROCEDURE :: get_deck_serialised => tdp_get_deck_serialised
    PROCEDURE :: parse_deck_serialised => tdp_parse_deck_serialised
    PROCEDURE :: parse_deck_file => tdp_parse_deck_file
    PROCEDURE :: parse_deck_string => tdp_parse_deck_string
    PROCEDURE :: parse_deck_object => tdp_parse_deck_object
    PROCEDURE :: reparse_deck => tdp_reparse_deck
    GENERIC :: parse_deck => parse_deck_file, parse_deck_object, reparse_deck
    PROCEDURE, PRIVATE :: call_blocks => tdp_call_blocks
    PROCEDURE, PRIVATE :: display_blocks => tdp_display_blocks
    PROCEDURE, PRIVATE :: generate_and_parse => tdp_parse_generate
    PROCEDURE :: initialise_deck => tdp_initialise_deck
    PROCEDURE :: initialize_deck => tdp_initialise_deck
    PROCEDURE :: finalise_deck => tdp_finalise_deck
    PROCEDURE :: finalize_deck => tdp_finalise_deck
    PROCEDURE :: get_error_count => tdp_get_error_count
    PROCEDURE :: get_error_report => tdp_get_error_report
    PROCEDURE :: flush_errors => tdp_flush_errors
    PROCEDURE :: get_block_name => tdp_get_block_name
    PROCEDURE :: get_block_parents => tdp_get_block_parents
    PROCEDURE :: get_block_structure => tdp_get_block_structure
    FINAL :: tdp_destructor

  END TYPE eis_text_deck_parser

  CONTAINS

  !> @brief
  !> Parse a specific deck block against a definition
  !> @param[inout] this
  !> @param[in] definition
  !> @param[in] block
  !> @param[inout] status
  !> @param[inout] host_state
  !> @param[in] pass_number
  !> @param[inout] errcode
  !> @param[in] unknown_block_is_fatal
  !> @param[in] unknown_key_is_fatal
  RECURSIVE SUBROUTINE tdp_call_blocks(this, definition, block, status, &
      host_state, pass_number, errcode, unknown_block_is_fatal, &
      unknown_key_is_fatal, bad_key_is_fatal)
    CLASS(eis_text_deck_parser), INTENT(IN) :: this
    !> Definition object used to parse the deck against
    CLASS(eis_deck_block_definition), INTENT(INOUT) :: definition
    !> Deck block to parse
    TYPE(eis_string_deck_block), POINTER, INTENT(IN) :: block
    !> EIS deck parser status code
    INTEGER(eis_status), INTENT(INOUT) :: status
    !> User code state variable
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    !> Pass number as specified by the host code
    INTEGER, INTENT(IN) :: pass_number
    !> EIS deck parser error code
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Should parsing stop if an unknown block is encountered
    LOGICAL, INTENT(IN) :: unknown_block_is_fatal
    !> Should parsing stop if an unknown key is encountered
    LOGICAL, INTENT(IN) :: unknown_key_is_fatal
    !> Should parsing stop if a bad key is encountered (key that errors during
    !> parsing)
    LOGICAL, INTENT(IN) :: bad_key_is_fatal

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
    INTEGER :: line_number, wsl

    CALL eis_default_status(this_errcode, this_status, this_host)
    CALL block%get_parents(block_parents)
    CALL definition%start_block(block_parents, pass_number, this_errcode, &
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
      CALL block%get_line(i, line, filename = fn, line_number = line_number, &
          trimmed_white_space_length = wsl)
      CALL eis_default_status(this_errcode, this_status, this_host)
      CALL definition%call_key(line, block_parents, pass_number, this_errcode, &
          host_state = this_host, filename = fn, &
          line_number = line_number, white_space_length = wsl, &
          parser = this%parser, interop_parser_id = this%interop_parser)
      IF (this_errcode /= eis_err_none) THEN
        IF (ALLOCATED(fn)) THEN
          CALL this%err_handler%add_error(eis_err_deck_parser, this_errcode, &
              filename = fn, line_number = line_number)
        ELSE
          CALL this%err_handler%add_error(eis_err_deck_parser, this_errcode)
        END IF
        IF (bad_key_is_fatal) this_status = IOR(this_status, &
            eis_status_terminate)
      END IF
      errcode = IOR(errcode, this_errcode)
      status = IOR(status, this_status)
      host_state = IOR(host_state, this_host)
      IF (IAND(errcode, eis_err_unknown_key) /= 0 &
          .AND. unknown_key_is_fatal) RETURN
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
      IF (IAND(errcode, eis_err_unknown_block) /= 0 &
          .AND. unknown_block_is_fatal) RETURN

      IF (this_errcode == eis_err_none) THEN
        CALL eis_default_status(this_errcode, this_status, this_host)
        CALL this%call_blocks(cdef, block%get_child(i), this_status, &
            this_host, pass_number, this_errcode, unknown_block_is_fatal, &
            unknown_key_is_fatal, bad_key_is_fatal)
        errcode = IOR(errcode, this_errcode)
        status = IOR(status, this_status)
        host_state = IOR(host_state, this_host)
        IF (IAND(errcode, eis_err_unknown_block) /= 0 .OR. IAND(this_status, &
            eis_status_terminate) /= 0) RETURN
      END IF
    END DO

    CALL definition%end_block(block_parents, pass_number, this_errcode, &
        host_state = this_host, display_name=block%block_name)

  END SUBROUTINE tdp_call_blocks



  !> @brief
  !> Parse a specific deck block against a definition
  !> @param[inout] this
  !> @param[in] block
  !> @param[in] include_keys
  !> @param[inout]] line_id
  !> @param[in] dot_str
  !> @param[in] max_depth
  !> @param[in] max_key_depth
  RECURSIVE SUBROUTINE tdp_display_blocks(this, block, line_id, dot_str, &
      include_keys, max_depth, max_key_depth)
    CLASS(eis_text_deck_parser), INTENT(IN) :: this
    !> Deck block to parse
    TYPE(eis_string_deck_block), POINTER, INTENT(IN) :: block
    !> ID for text lines in the dot output
    INTEGER, INTENT(INOUT) :: line_id
    !> String containing the dot output
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: dot_str
    !> Logical specifying whether to include keys
    LOGICAL, INTENT(IN), OPTIONAL :: include_keys
    !> Integer specifying the maximum depth to visualise the stack to
    INTEGER, INTENT(IN), OPTIONAL :: max_depth
    !> Integer specifyin the maximum depth to visualise keys to
    INTEGER, INTENT(IN), OPTIONAL :: max_key_depth

    CHARACTER(LEN=5) :: val1, val2
    INTEGER :: i
    CHARACTER(LEN=:), ALLOCATABLE :: line
    INTEGER, DIMENSION(:), ALLOCATABLE :: block_parents
    LOGICAL :: do_keys
    INTEGER :: cpos, epos, spos

    CALL block%get_parents(block_parents)
    IF (PRESENT(max_depth)) THEN
      IF (SIZE(block_parents) - 1 > max_depth) THEN
        DEALLOCATE(block_parents)
        RETURN
      END IF
    END IF

    do_keys = .FALSE.
    IF (PRESENT(include_keys)) THEN
      do_keys = include_keys
      IF (PRESENT(max_key_depth)) THEN
        do_keys = (SIZE(block_parents) - 1 > max_key_depth)
      END IF
    END IF

    WRITE(val1, '(I5.5)') block%id
    CALL eis_append_string(dot_str, TRIM(val1) // '[label="' &
        // block%block_name // '"] [shape= diamond];')
    CALL block%get_parents(block_parents)
    IF (SIZE(block_parents) > 1) THEN
      WRITE(val2, '(I5.5)') block_parents(SIZE(block_parents)-1)
      CALL eis_append_string(dot_str, val2 // ' -- ' // val1 // ';')
    END IF
    DO i = 1, block%get_child_count()
      CALL this%display_blocks(block%get_child(i), line_id, dot_str, &
          include_keys, max_depth, max_key_depth)
    END DO

    IF (do_keys) THEN
      WRITE(val2, '(I5.5)') block_parents(SIZE(block_parents))
      DO i = 1, block%get_line_count()
        CALL block%get_line(i, line)
        cpos = INDEX(line,':')
        epos = INDEX(line,'=')
        IF (cpos == 0 .AND. epos == 0) THEN
          spos = LEN(line) + 1
        ELSE
          IF (cpos == 0) cpos = HUGE(cpos)
          IF (epos == 0) epos = HUGE(epos)
          spos = MIN(cpos,epos)
        END IF
        WRITE(val1,'(I5.5)') line_id
        CALL eis_append_string(dot_str, TRIM(val1) // '[label="' &
            // TRIM(ADJUSTL(line(1:spos-1))) // '"] [shape= circle];')
        CALL eis_append_string(dot_str, val2 // ' -- ' // val1 // ';')
        line_id = line_id + 1
      END DO
      IF (ALLOCATED(line)) DEALLOCATE(line)
    END IF

    DEALLOCATE(block_parents)

  END SUBROUTINE tdp_display_blocks



  !> @brief
  !> Destructor
  !> @param[inout] this
  SUBROUTINE tdp_destructor(this)
    TYPE(eis_text_deck_parser), INTENT(INOUT) :: this

    IF (this%owns_handler .AND. ASSOCIATED(this%err_handler)) &
        DEALLOCATE(this%err_handler)
    IF (this%owns_parser .AND. ASSOCIATED(this%parser)) &
        DEALLOCATE(this%parser)

  END SUBROUTINE tdp_destructor



  !> @brief
  !> Initialise function for the parser
  !> @param[inout] this
  !> @param[in] err_handler
  !> @param[in] parser
  !> @param[in] interop_parser_id
  !> @param[in] unknown_block_is_fatal
  !> @param[in] unknown_key_is fatal
  SUBROUTINE tdp_init(this, err_handler, parser, interop_parser_id, &
      unknown_block_is_fatal, unknown_key_is_fatal, bad_key_is_fatal)
    CLASS(eis_text_deck_parser), INTENT(INOUT) :: this
    !> Optional error handler supplied from host code. Default is create
    !> own error handler
    TYPE(eis_error_handler), INTENT(IN), POINTER, OPTIONAL :: err_handler
    !> Optional maths parser supplied from host code. Default is create own
    !> own maths parser
    TYPE(eis_parser), INTENT(IN), POINTER, OPTIONAL :: parser
    !> Optional C maths parser interoperable parser code. Default is create
    !> own maths parser
    INTEGER, INTENT(IN), OPTIONAL :: interop_parser_id
    !> Should parsing terminate if an unknown block is found
    !> in a deck? Optional, default .TRUE.
    LOGICAL, INTENT(IN), OPTIONAL :: unknown_block_is_fatal
    !> Should parsing terminate if an unknown key is found
    !> in a deck? Optional, default .TRUE.
    LOGICAL, INTENT(IN), OPTIONAL :: unknown_key_is_fatal
    !> Should parsing terminate if an unparseable key is found
    !> in a deck? Optional, default .TRUE.
    LOGICAL, INTENT(IN), OPTIONAL :: bad_key_is_fatal
    INTEGER(eis_error) :: err

    this%is_init = .TRUE.

    IF (this%owns_handler .AND. ASSOCIATED(this%err_handler)) &
        DEALLOCATE(this%err_handler)

    IF(PRESENT(err_handler)) THEN
      this%err_handler => err_handler
      this%owns_handler = .FALSE.
    ELSE
      ALLOCATE(this%err_handler)
      this%owns_handler = .TRUE.
    END IF

    IF (this%owns_parser .AND. ASSOCIATED(this%parser)) DEALLOCATE(this%parser)

    IF (PRESENT(parser)) THEN
      this%parser => parser
      IF (PRESENT(interop_parser_id)) THEN
        this%interop_parser = interop_parser_id
      ELSE
        this%owns_interop = .TRUE.
        this%interop_parser = eis_add_interop_parser(this%parser, &
            holds = .FALSE.)
      END IF
    ELSE
      IF (PRESENT(interop_parser_id)) THEN
        this%parser => eis_get_interop_parser(interop_parser_id)
      END IF
      IF (.NOT. ASSOCIATED(this%parser)) THEN
        this%owns_parser = .TRUE.
        this%owns_interop = .TRUE.
        ALLOCATE(this%parser)
        CALL this%parser%init(err, err_handler = this%err_handler)
        this%interop_parser = eis_add_interop_parser(this%parser, &
            holds = .FALSE.)
      END IF
    END IF

    IF (PRESENT(unknown_block_is_fatal)) this%unknown_block_is_fatal &
        = unknown_block_is_fatal

    IF (PRESENT(unknown_key_is_fatal)) this%unknown_key_is_fatal &
        = unknown_key_is_fatal

    IF (PRESENT(bad_key_is_fatal)) this%bad_key_is_fatal = bad_key_is_fatal

  END SUBROUTINE tdp_init


  !> @brief
  !> Initialise blocks in a deck
  !> @param[inout] this
  !> @param[in] definition
  !> @param[out] errcode
  !> @param[inout] state
  SUBROUTINE tdp_initialise_deck(this, definition, errcode, state)
    CLASS(eis_text_deck_parser), INTENT(INOUT) :: this
    TYPE(eis_deck_definition), INTENT(INOUT) :: definition
    INTEGER(eis_error), INTENT(OUT) :: errcode
    INTEGER(eis_bitmask), INTENT(INOUT), OPTIONAL :: state
    INTEGER(eis_error) :: err
    INTEGER(eis_bitmask) :: host_state

    IF (.NOT. this%is_init) CALL this%init()

    CALL eis_default_status(errcode = err, bitmask = host_state)
    CALL definition%initialise_blocks(host_state, err)
    errcode = IOR(errcode, err)
    IF (PRESENT(state)) state = IOR(state, host_state)

  END SUBROUTINE tdp_initialise_deck



  !> @brief
  !> Finalise blocks in a deck
  !> @param[inout] this
  !> @param[in] definition
  !> @param[out] errcode
  !> @param[inout] state
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



  !> @brief
  !> Initialise function for the parser
  !> @param[inout] this
  !> @param[in] string_deck
  !> @param[inout] definition
  !> @param[out] errcode
  !> @param[in] pass_number
  !> @param[in] max_passes
  !> @param[inout] state
  !> @param[in] initialise_all_blocks
  !> @param[in] unknown_block_is_fatal
  !> @param[in] unknown_key_is_fatal
  SUBROUTINE tdp_parse_deck_object(this, string_deck, definition, errcode, &
      pass_number, max_passes, state, initialise_all_blocks, &
      unknown_block_is_fatal, unknown_key_is_fatal, bad_key_is_fatal)
    CLASS(eis_text_deck_parser), INTENT(INOUT) :: this
    !> String deck object holding a loaded deck
    CLASS(eis_string_deck), INTENT(IN) :: string_deck
    !> Deck definition object to parse against the string deck object
    TYPE(eis_deck_definition), INTENT(INOUT) :: definition
    !> Errror code
    INTEGER(eis_error), INTENT(OUT) :: errcode
    !> Pass number of this call to the parser. Blocks will only finalize if
    !> pass_number == max_passes. Optional, default 1
    INTEGER, INTENT(IN), OPTIONAL :: pass_number
    !> Maximum number of passes of the parser over this deck. Blocks will only
    !> finalize if pass_number == max_passes. Optional, default 1
    INTEGER, INTENT(IN), OPTIONAL :: max_passes
    !> Host code specified state variable. Optional, default 0
    INTEGER(eis_bitmask), INTENT(INOUT), OPTIONAL :: state
    !> Should all blocks be initialised before parsing begins?
    !> Optional. Default, .FALSE. so blocks are initialised
    !> when they first appear in a deck
    LOGICAL, INTENT(IN), OPTIONAL :: initialise_all_blocks
    !> Should parsing stop when an unknown block is found
    !> Optional, default .TRUE. or as set in INIT
    LOGICAL, INTENT(IN), OPTIONAL :: unknown_block_is_fatal
    !> Should parsing stop when an unknown key is found
    !> Optional, default .TRUE. or as set in INIT
    LOGICAL, INTENT(IN), OPTIONAL :: unknown_key_is_fatal
    !> Should parsing stop when an unparsable key is found
    !> Optional, default .TRUE. or as set in INIT
    LOGICAL, INTENT(IN), OPTIONAL :: bad_key_is_fatal


    TYPE(eis_string_deck_block), POINTER :: block
    INTEGER(eis_error) :: err
    INTEGER(eis_status) :: status
    INTEGER(eis_bitmask) :: host_state
    TYPE(eis_deck_block_definition), POINTER :: bdef
    LOGICAL :: parse_over, first_pass, ubf, ukf, bkf, should_init
    INTEGER :: gpass

    first_pass = .TRUE.
    should_init = .FALSE.
    ubf = this%unknown_block_is_fatal
    ukf = this%unknown_key_is_fatal
    bkf = this%bad_key_is_fatal
    gpass = 1
    IF (PRESENT(initialise_all_blocks)) should_init = initialise_all_blocks
    IF (PRESENT(pass_number)) THEN
      first_pass = (pass_number == 1)
      gpass = pass_number
    END IF
    IF (PRESENT(unknown_block_is_fatal)) ubf = unknown_block_is_fatal
    IF (PRESENT(unknown_key_is_fatal)) ukf = unknown_key_is_fatal
    IF (PRESENT(bad_key_is_fatal)) bkf = bad_key_is_fatal

    should_init = should_init .AND. first_pass

    IF (.NOT. this%is_init) CALL this%init()
    IF (first_pass) CALL definition%reset()
    IF (should_init) THEN
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
      CALL this%call_blocks(bdef, block, status, host_state, gpass, err, &
          ubf, ukf, bkf)
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



  SUBROUTINE tdp_parse_generate(this, sdeck, definition, errcode, &
      pass_number, max_passes, max_level, allow_root_keys, allow_empty_blocks, &
      state, initialise_all_blocks, unknown_block_is_fatal, &
      unknown_key_is_fatal)
    CLASS(eis_text_deck_parser), INTENT(INOUT) :: this
    CLASS(eis_string_deck), INTENT(INOUT) :: sdeck
    TYPE(eis_deck_definition), INTENT(INOUT) :: definition
    INTEGER(eis_error), INTENT(OUT) :: errcode
    INTEGER, INTENT(IN), OPTIONAL :: pass_number, max_passes
    INTEGER, INTENT(IN), OPTIONAL :: max_level
    LOGICAL, INTENT(IN), OPTIONAL :: allow_root_keys, allow_empty_blocks
    INTEGER(eis_bitmask), INTENT(INOUT), OPTIONAL :: state
    LOGICAL, INTENT(IN), OPTIONAL :: initialise_all_blocks
    LOGICAL, INTENT(IN), OPTIONAL :: unknown_block_is_fatal
    LOGICAL, INTENT(IN), OPTIONAL :: unknown_key_is_fatal

    TYPE(eis_string_deck_block), POINTER :: block
    INTEGER(eis_error) :: err, host_state, status
    TYPE(eis_deck_block_definition), POINTER :: bdef
    LOGICAL :: parse_over, first_pass

    IF (.NOT. this%is_init) RETURN

    CALL eis_default_status(errcode = err)
    CALL sdeck%generate_blocklist(err, max_level, allow_root_keys, &
        allow_empty_blocks)
    errcode = IOR(errcode, err)
    IF (err /= eis_err_none) RETURN

    CALL this%parse_deck_object(sdeck, definition, err, pass_number, &
        max_passes, state = state, initialise_all_blocks &
        = initialise_all_blocks, unknown_block_is_fatal &
        = unknown_block_is_fatal, unknown_key_is_fatal &
        = unknown_key_is_fatal)
    errcode = IOR(errcode, err)

  END SUBROUTINE tdp_parse_generate



  SUBROUTINE tdp_parse_deck_file(this, filename, definition, errcode, &
      pass_number, max_passes, max_level, allow_root_keys, allow_empty_blocks, &
      state, initialise_all_blocks, unknown_block_is_fatal, &
      unknown_key_is_fatal, filename_processor, file_text_processor)
    CLASS(eis_text_deck_parser), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: filename
    TYPE(eis_deck_definition), INTENT(INOUT) :: definition
    INTEGER(eis_error), INTENT(OUT) :: errcode
    INTEGER, INTENT(IN), OPTIONAL :: pass_number, max_passes
    INTEGER, INTENT(IN), OPTIONAL :: max_level
    LOGICAL, INTENT(IN), OPTIONAL :: allow_root_keys, allow_empty_blocks
    INTEGER(eis_bitmask), INTENT(INOUT), OPTIONAL :: state
    LOGICAL, INTENT(IN), OPTIONAL :: initialise_all_blocks
    LOGICAL, INTENT(IN), OPTIONAL :: unknown_block_is_fatal
    LOGICAL, INTENT(IN), OPTIONAL :: unknown_key_is_fatal
    PROCEDURE(filename_processor_proto), OPTIONAL :: filename_processor
    PROCEDURE(file_text_processor_proto), OPTIONAL :: file_text_processor

    TYPE(eis_string_deck_block), POINTER :: block
    INTEGER(eis_error) :: err, host_state, status
    TYPE(eis_deck_block_definition), POINTER :: bdef
    LOGICAL :: parse_over, first_pass

    IF (.NOT. this%is_init) CALL this%init()
    IF (ALLOCATED(this%sdeck)) DEALLOCATE(this%sdeck)
    ALLOCATE(this%sdeck)

    CALL eis_default_status(errcode = err)
    CALL this%sdeck%init(err, this%err_handler, &
        filename_processor = filename_processor, &
        file_text_processor = file_text_processor)
    errcode = IOR(errcode, err)
    IF (err /= eis_err_none) RETURN

    CALL eis_default_status(errcode = err)
    CALL this%sdeck%load_deck_file(filename, err)
    errcode = IOR(errcode, err)
    IF (err /= eis_err_none) RETURN

    CALL this%generate_and_parse(this%sdeck, definition, err, &
        pass_number = pass_number, max_passes = max_passes, state = state, &
        initialise_all_blocks = initialise_all_blocks, unknown_block_is_fatal &
        = unknown_block_is_fatal, unknown_key_is_fatal &
        = unknown_key_is_fatal, allow_empty_blocks = allow_empty_blocks, &
        allow_root_keys = allow_root_keys)
    errcode = IOR(errcode, err)

  END SUBROUTINE tdp_parse_deck_file



  SUBROUTINE tdp_parse_deck_string(this, text, definition, errcode, &
      pass_number, max_passes, max_level, allow_root_keys, allow_empty_blocks, &
      state, initialise_all_blocks, unknown_block_is_fatal, &
      unknown_key_is_fatal, filename)
    CLASS(eis_text_deck_parser), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: text
    TYPE(eis_deck_definition), INTENT(INOUT) :: definition
    INTEGER(eis_error), INTENT(OUT) :: errcode
    INTEGER, INTENT(IN), OPTIONAL :: pass_number, max_passes
    INTEGER, INTENT(IN), OPTIONAL :: max_level
    LOGICAL, INTENT(IN), OPTIONAL :: allow_root_keys, allow_empty_blocks
    INTEGER(eis_bitmask), INTENT(INOUT), OPTIONAL :: state
    LOGICAL, INTENT(IN), OPTIONAL :: initialise_all_blocks
    LOGICAL, INTENT(IN), OPTIONAL :: unknown_block_is_fatal
    LOGICAL, INTENT(IN), OPTIONAL :: unknown_key_is_fatal
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: filename

    TYPE(eis_string_deck_block), POINTER :: block
    INTEGER(eis_error) :: err, host_state, status
    TYPE(eis_deck_block_definition), POINTER :: bdef
    LOGICAL :: parse_over, first_pass

    IF (.NOT. this%is_init) CALL this%init()
    IF (ALLOCATED(this%sdeck)) DEALLOCATE(this%sdeck)
    ALLOCATE(this%sdeck)

    CALL eis_default_status(errcode = err)
    CALL this%sdeck%init(err, this%err_handler)
    errcode = IOR(errcode, err)
    IF (err /= eis_err_none) RETURN

    CALL eis_default_status(errcode = err)
    CALL this%sdeck%read_deck_string(text, err, filename)
    errcode = IOR(errcode, err)
    IF (err /= eis_err_none) RETURN

    CALL this%generate_and_parse(this%sdeck, definition, err, &
        pass_number = pass_number, max_passes = max_passes, state = state, &
        initialise_all_blocks = initialise_all_blocks, unknown_block_is_fatal &
        = unknown_block_is_fatal, unknown_key_is_fatal &
        = unknown_key_is_fatal, allow_empty_blocks = allow_empty_blocks, &
        allow_root_keys = allow_root_keys)
    errcode = IOR(errcode, err)

  END SUBROUTINE tdp_parse_deck_string



  SUBROUTINE tdp_reparse_deck(this, definition, errcode, &
      pass_number, max_passes, max_level, allow_root_keys, allow_empty_blocks, &
      state, initialise_all_blocks, unknown_block_is_fatal, &
      unknown_key_is_fatal)
    CLASS(eis_text_deck_parser), INTENT(INOUT) :: this
    TYPE(eis_deck_definition), INTENT(INOUT) :: definition
    INTEGER(eis_error), INTENT(OUT) :: errcode
    INTEGER, INTENT(IN), OPTIONAL :: pass_number, max_passes
    INTEGER, INTENT(IN), OPTIONAL :: max_level
    LOGICAL, INTENT(IN), OPTIONAL :: allow_root_keys, allow_empty_blocks
    INTEGER(eis_bitmask), INTENT(INOUT), OPTIONAL :: state
    LOGICAL, INTENT(IN), OPTIONAL :: initialise_all_blocks
    LOGICAL, INTENT(IN), OPTIONAL :: unknown_block_is_fatal
    LOGICAL, INTENT(IN), OPTIONAL :: unknown_key_is_fatal

    TYPE(eis_string_deck_block), POINTER :: block
    INTEGER(eis_error) :: err, host_state, status
    TYPE(eis_deck_block_definition), POINTER :: bdef
    LOGICAL :: parse_over, first_pass

    IF (.NOT. this%is_init) RETURN
    IF (.NOT. ALLOCATED(this%sdeck)) RETURN
    IF (.NOT. this%sdeck%is_init) RETURN

    CALL eis_default_status(errcode = err)
    CALL this%generate_and_parse(this%sdeck, definition, err, &
        pass_number = pass_number, max_passes = max_passes, state = state, &
        initialise_all_blocks = initialise_all_blocks, unknown_block_is_fatal &
        = unknown_block_is_fatal, unknown_key_is_fatal &
        = unknown_key_is_fatal, allow_empty_blocks = allow_empty_blocks, &
        allow_root_keys = allow_root_keys)
    errcode = IOR(errcode, err)

  END SUBROUTINE tdp_reparse_deck



  !> @brief
  !> Get a serialised text version of a deck file
  !> Contains all of the information about the source filename
  !> and the source line numbers in encoded form
  !> @param[inout] this
  !> @param[in] filename
  !> @param[out] serial_deck
  !> @param[inout] errcode
  !> @param[out] raw_text
  SUBROUTINE tdp_get_deck_serialised(this, filename, serial_deck, errcode, &
      raw_text)
    CLASS(eis_text_deck_parser), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: filename
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: serial_deck
    INTEGER(eis_error), INTENT(OUT) :: errcode
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT), OPTIONAL :: raw_text
    TYPE(eis_string_deck) :: sdeck
    TYPE(eis_string_deck_block), POINTER :: block
    INTEGER(eis_error) :: err, host_state, status
    TYPE(eis_deck_block_definition), POINTER :: bdef
    LOGICAL :: parse_over, first_pass

    IF (.NOT. this%is_init) CALL this%init()
    IF (ALLOCATED(this%sdeck)) DEALLOCATE(this%sdeck)
    ALLOCATE(this%sdeck)

    CALL eis_default_status(errcode = err)
    CALL sdeck%init(err, this%err_handler)
    errcode = IOR(errcode, err)
    IF (err /= eis_err_none) RETURN

    CALL eis_default_status(errcode = err)
    CALL sdeck%load_deck_file(filename, err, parsed_text = serial_deck)
    errcode = IOR(errcode, err)
    IF (err /= eis_err_none) RETURN

  END SUBROUTINE tdp_get_deck_serialised




  SUBROUTINE tdp_parse_deck_serialised(this, serial_text, definition, errcode, &
      pass_number, max_passes, max_level, allow_root_keys, allow_empty_blocks, &
      state, initialise_all_blocks, unknown_block_is_fatal, &
      unknown_key_is_fatal)
    CLASS(eis_text_deck_parser), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: serial_text
    TYPE(eis_deck_definition), INTENT(INOUT) :: definition
    INTEGER(eis_error), INTENT(OUT) :: errcode
    INTEGER, INTENT(IN), OPTIONAL :: pass_number, max_passes
    INTEGER, INTENT(IN), OPTIONAL :: max_level
    LOGICAL, INTENT(IN), OPTIONAL :: allow_root_keys, allow_empty_blocks
    INTEGER(eis_bitmask), INTENT(INOUT), OPTIONAL :: state
    LOGICAL, INTENT(IN), OPTIONAL :: initialise_all_blocks
    LOGICAL, INTENT(IN), OPTIONAL :: unknown_block_is_fatal
    LOGICAL, INTENT(IN), OPTIONAL :: unknown_key_is_fatal

    TYPE(eis_string_deck_block), POINTER :: block
    INTEGER(eis_error) :: err, host_state, status
    TYPE(eis_deck_block_definition), POINTER :: bdef
    LOGICAL :: parse_over, first_pass

    IF (.NOT. this%is_init) CALL this%init()
    IF (ALLOCATED(this%sdeck)) DEALLOCATE(this%sdeck)
    ALLOCATE(this%sdeck)

    CALL eis_default_status(errcode = err)
    CALL this%sdeck%init(err, this%err_handler)
    errcode = IOR(errcode, err)
    IF (err /= eis_err_none) RETURN

    CALL eis_default_status(errcode = err)
    CALL this%sdeck%read_parsed_deck(serial_text, err)
    errcode = IOR(errcode, err)
    IF (err /= eis_err_none) RETURN

    CALL this%generate_and_parse(this%sdeck, definition, err, &
        pass_number = pass_number, max_passes = max_passes, state = state, &
        initialise_all_blocks = initialise_all_blocks, unknown_block_is_fatal &
        = unknown_block_is_fatal, unknown_key_is_fatal &
        = unknown_key_is_fatal, allow_root_keys = allow_root_keys, &
        allow_empty_blocks = allow_empty_blocks)
    errcode = IOR(errcode, err)

  END SUBROUTINE tdp_parse_deck_serialised



  !> Get the number of errors reported on this deck parse
  !> @param[inout] this
  !> @return count
  FUNCTION tdp_get_error_count(this) RESULT(count)
    CLASS(eis_text_deck_parser), INTENT(IN) :: this
    INTEGER :: count !< Number of errors reported

    count = this%err_handler%get_error_count()

  END FUNCTION tdp_get_error_count



  !> @brief
  !> Get the error report on a specified error
  !> @param[inout] this
  !> @param[in] index
  !> @param[out] report
  SUBROUTINE tdp_get_error_report(this, index, report)
    CLASS(eis_text_deck_parser), INTENT(IN) :: this
    !> Index of error to get report on. Must be between 1 and
    !> the result of eip_get_error_count
    INTEGER, INTENT(IN) :: index
    !> Allocatable string variable containing the error report
    !> will be reallocated to be exactly long enough to store
    !> the error report whether allocated or not
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: report

    CALL this%err_handler%get_error_report(index, report)
  END SUBROUTINE tdp_get_error_report


  !> @brief
  !> Print all of the errors to stdout
  !> @param[inout] this
  SUBROUTINE tdp_flush_errors(this)
    CLASS(eis_text_deck_parser) :: this

    CALL this%err_handler%flush_errors()

  END SUBROUTINE tdp_flush_errors


  !> @brief
  !> Get the name of a block from the ID
  !> @param[inout] this
  !> @param[in] index
  !> @param[out] name
  SUBROUTINE tdp_get_block_name(this, index, name)
    CLASS(eis_text_deck_parser), INTENT(IN) :: this
    !Index of the block to get name of
    INTEGER, INTENT(IN) :: index
    !> Allocatable string variable to contain the name
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: name

    IF (ALLOCATED(this%sdeck)) THEN
      CALL this%sdeck%get_block_name(index, name)
    END IF

  END SUBROUTINE tdp_get_block_name



  !> @brief
  !> Get the parents of a block from the ID
  !> @param[inout] this
  !> @param[in] index
  !> @param[out] parents
  SUBROUTINE tdp_get_block_parents(this, index, parents)
    CLASS(eis_text_deck_parser), INTENT(IN) :: this
    !Index of the block to get name of
    INTEGER, INTENT(IN) :: index
    !> Allocatable integer variable to hold the parents
    INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: parents
    TYPE(eis_string_deck_block), POINTER :: block

    IF (ALLOCATED(this%sdeck)) THEN
      block => this%sdeck%get_block(index)
      IF (ASSOCIATED(block)) CALL block%get_parents(parents)
    END IF

  END SUBROUTINE tdp_get_block_parents



  !> @brief
  !> Get the structure of a deck's blocks and optionally keys
  !> @param[inout] this
  !> @param[out] dot_str
  !> @param[in[ include_keys
  !> @param[in] max_depth
  !> @param[in] max_key_depth
  SUBROUTINE tdp_get_block_structure(this, dot_str, include_keys, max_depth, &
      max_key_depth)
    CLASS(eis_text_deck_parser), INTENT(IN) :: this
    !> String holding the structure of the deck
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: dot_str
    !> Should the output include keys?
    LOGICAL, INTENT(IN), OPTIONAL :: include_keys
    !> Maximum depth of block to display
    INTEGER, INTENT(IN), OPTIONAL :: max_depth
    !> Maximum depth at which to show keys
    INTEGER, INTENT(IN), OPTIONAL :: max_key_depth
    TYPE(eis_string_deck_block), POINTER :: block
    INTEGER :: level

    IF (ALLOCATED(this%sdeck)) THEN
      block => this%sdeck%get_block()
      IF (ASSOCIATED(block)) THEN
        level = this%sdeck%max_block_id + 1
        CALL eis_append_string(dot_str, 'strict graph G {')
        CALL this%display_blocks(block, level, dot_str, &
            include_keys = include_keys, max_depth = max_depth, &
            max_key_depth = max_key_depth)
        CALL eis_append_string(dot_str, '}')
      END IF
    END IF

  END SUBROUTINE tdp_get_block_structure

END MODULE eis_deck_from_text_mod
