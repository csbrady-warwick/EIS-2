MODULE eis_deck_caller_mod

  USE eis_parser_constants
  USE eis_deck_definition_mod
  USE eis_ordered_store_mod
  USE eis_numbered_store_mod
  USE eis_parser_mod
  USE eis_error_mod
  USE eis_utils
  IMPLICIT NONE

  PRIVATE

  TYPE :: eis_deck_caller_key
    INTEGER(uid_kind) :: uid = -1
    CHARACTER(LEN=:), ALLOCATABLE :: key_filename
    INTEGER :: key_line_number = -1
    INTEGER :: key_offset = 0
    CHARACTER(LEN=:), ALLOCATABLE :: value_filename
    INTEGER :: value_line_number = -1
    INTEGER :: value_offset = 0
    CHARACTER(LEN=:), ALLOCATABLE :: key_text, value_text, key_comment
    PROCEDURE(parser_result_function), POINTER, NOPASS :: value_function &
        => NULL()
    CHARACTER(LEN=:), ALLOCATABLE :: function_text
    CHARACTER(LEN=:), ALLOCATABLE :: trimmed_line_text
    TYPE(eis_parser), POINTER :: parser => NULL()
    INTEGER :: interop_parser = -1
  END TYPE eis_deck_caller_key

  TYPE :: eis_deck_caller_block
    CHARACTER(LEN=:), ALLOCATABLE :: block_text, block_comment
    INTEGER :: line_number = -1
    INTEGER(uid_kind) :: uid
    CLASS(eis_deck_caller_block), POINTER :: parent => NULL()
    CLASS(eis_deck_block_definition), POINTER :: definition => NULL()
    TYPE(ordered_store) :: children
    TYPE(ordered_store) :: keys
  END TYPE eis_deck_caller_block

  !Type constants for events
  INTEGER, PARAMETER :: edce_null = 0
  INTEGER, PARAMETER :: edce_init = 1
  INTEGER, PARAMETER :: edce_start = 2
  INTEGER, PARAMETER :: edce_call = 3
  INTEGER, PARAMETER :: edce_end = 4
  INTEGER, PARAMETER :: edce_end_pass = 5
  INTEGER, PARAMETER :: edce_final = 6

  TYPE :: eis_deck_caller_event
    INTEGER :: type = edce_null
    TYPE(C_PTR) :: host_params = C_NULL_PTR
    LOGICAL :: offset_in_value = .FALSE.
    CHARACTER(LEN=:), ALLOCATABLE :: filename
    INTEGER :: line_number = -1
    INTEGER :: pass_number
    INTEGER(eis_bitmask) :: host_state = 0_eis_bitmask
    CLASS(eis_deck_caller_block), POINTER :: block => NULL()
    CLASS(eis_deck_caller_key), POINTER :: key => NULL()
  END TYPE eis_deck_caller_event

  TYPE :: eis_deck_caller
    PRIVATE
    LOGICAL :: owns_parser = .FALSE.
    CLASS(eis_parser), POINTER :: parser => NULL()
    LOGICAL :: owns_interop = .TRUE.
    INTEGER :: interop_parser = -1
    LOGICAL :: owns_err_handler = .FALSE.
    CLASS(eis_error_handler), POINTER :: err_handler => NULL()
    TYPE(eis_uid_generator) :: uid_generator, key_uid_generator

    CLASS(eis_deck_definition), POINTER :: definition => NULL()
    INTEGER :: pass_number = 1
    CHARACTER(LEN=:), ALLOCATABLE :: code_name, filename
    INTEGER :: current_level = 1
    INTEGER, DIMENSION(:), ALLOCATABLE :: current_parents
    TYPE(numbered_store) :: blocks
    TYPE(numbered_store) :: keys
    TYPE(ordered_store) :: events
    TYPE(eis_deck_caller_block), POINTER :: root => NULL()
    TYPE(eis_deck_caller_block), POINTER :: current_block => NULL()
    CONTAINS
    PROCEDURE :: add_parent => edc_add_parent
    PROCEDURE :: remove_parent => edc_remove_parent
    PROCEDURE, PUBLIC :: init => edc_init
    PROCEDURE, PUBLIC :: reset => edc_reset
    PROCEDURE, PUBLIC :: set_pass => edc_set_pass
    PROCEDURE, PUBLIC :: initialise_all_blocks => edc_initialise_all_blocks
    PROCEDURE, PUBLIC :: initialize_all_blocks => edc_initialise_all_blocks
    PROCEDURE, PUBLIC :: start_block => edc_start_block
    PROCEDURE, PUBLIC :: call_key => edc_call_key
    PROCEDURE, PUBLIC :: end_block => edc_end_block
    PROCEDURE, PUBLIC :: end_and_finalise_block => edc_end_and_final_block
    PROCEDURE, PUBLIC :: end_and_finalize_block => edc_end_and_final_block
    PROCEDURE, PUBLIC :: finalise_all_blocks => edc_finalise_all_blocks
    PROCEDURE, PUBLIC :: finalize_all_blocks => edc_finalise_all_blocks
    PROCEDURE, PUBLIC :: end_pass => edc_end_pass

    PROCEDURE, PUBLIC :: replay_deck => edc_replay_deck

    PROCEDURE, PUBLIC :: get_error_count => edc_get_error_count
    PROCEDURE, PUBLIC :: get_error_report => edc_get_error_report
    PROCEDURE, PUBLIC :: flush_errors => edc_flush_errors

    PROCEDURE, PUBLIC :: generate_deck => edc_generate_deck

    FINAL :: edc_destructor
  END TYPE eis_deck_caller

  PUBLIC :: eis_deck_caller

  CONTAINS

  SUBROUTINE edc_destructor(this)
    TYPE(eis_deck_caller), INTENT(INOUT) :: this

    IF (this%owns_parser .AND. ASSOCIATED(this%parser)) DEALLOCATE(this%parser)
    IF (this%owns_interop .AND. this%interop_parser > -1) &
        CALL eis_release_interop_parser(this%interop_parser)
    IF (this%owns_err_handler .AND. ASSOCIATED(this%err_handler)) &
        DEALLOCATE(this%err_handler)
  END SUBROUTINE edc_destructor



  SUBROUTINE edc_add_parent(this, parent_id)
    CLASS(eis_deck_caller), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: parent_id
    INTEGER, DIMENSION(:), ALLOCATABLE :: new_parents

    IF (.NOT. ALLOCATED(this%current_parents)) RETURN
    IF (this%current_level == SIZE(this%current_parents)) THEN
      ALLOCATE(new_parents(SIZE(this%current_parents)*2))
      new_parents(1:SIZE(this%current_parents)) = this%current_parents
      DEALLOCATE(this%current_parents)
      CALL MOVE_ALLOC(FROM = new_parents, TO = this%current_parents)
    END IF
    this%current_level = this%current_level + 1
    this%current_parents(this%current_level) = parent_id

  END SUBROUTINE edc_add_parent



  SUBROUTINE edc_remove_parent(this)
    CLASS(eis_deck_caller), INTENT(INOUT) :: this

    this%current_level = this%current_level - 1

  END SUBROUTINE edc_remove_parent


  !> @brief
  !> Set up the caller object and attach a definition
  !> @param[inout] this
  !> @param[in] definition
  !> @param[in] errcode
  !> @param[in] parser
  !> @param[in] err_handler
  !> @param[in] pass_number
  !> @param[in] code_name
  !> @param[in] filename
  !> @param[in] line_number
  !> @param[in] interop_parser
  SUBROUTINE edc_init(this, definition, errcode, parser, err_handler, &
      pass_number, code_name, filename, line_number, interop_parser)
    CLASS(eis_deck_caller), INTENT(INOUT) :: this
    !> Deck definition to use when calling blocks and keys
    CLASS(eis_deck_definition), POINTER, INTENT(IN) :: definition
    !> Error code from starting the caller
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Specify a maths parser to use. Optional, default is create a default
    !> eis_parser object. If explicitly passed a null pointer
    CLASS(eis_parser), INTENT(INOUT), POINTER, OPTIONAL :: parser
    !> Specify an error handler to use. Optional, default is to create a 
    !. default error handler
    CLASS(eis_error_handler), INTENT(IN), POINTER, OPTIONAL :: err_handler
    !> Default pass number when calling definition items. Can be overriden 
    !> when calling individual elements. Optional, default 1
    INTEGER, INTENT(IN), OPTIONAL :: pass_number
    !>Name of the code that is calling the deck. Not currently used but intended
    !>for future error reporting
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: code_name
    !> Filename of the file that is calling the init function. Used for error
    !> reporting. Optional, default do not use when reporting errors
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: filename
    !> Line number in the file that is calling the init function. Used for error
    !> reporting. Optional, default do not use when reporting errors
    INTEGER, INTENT(IN), OPTIONAL :: line_number
    !> The ID number of an interoperable parser to use
    INTEGER, INTENT(IN), OPTIONAL :: interop_parser

    TYPE(eis_deck_caller_block), POINTER :: root_block
    CLASS(*), POINTER :: gptr

    errcode = eis_err_none

    this%pass_number = 1
    IF (PRESENT(pass_number)) this%pass_number = pass_number
    this%definition => definition

    IF (PRESENT(code_name)) THEN
      IF (ALLOCATED(this%code_name)) DEALLOCATE(this%code_name)
      ALLOCATE(this%code_name, SOURCE = code_name)
    END IF
    IF (.NOT. ALLOCATED(this%code_name)) ALLOCATE(this%code_name, &
        SOURCE = 'unknown_source')

    IF (PRESENT(filename)) THEN
      IF (ALLOCATED(this%filename)) DEALLOCATE(this%filename)
      ALLOCATE(this%filename, SOURCE =filename)
    END IF
    IF (.NOT. ALLOCATED(this%filename)) ALLOCATE(this%filename, &
        SOURCE = 'unknown file')

    !IF specifying a new error_handler deallocate the old one if needed
    IF (PRESENT(err_handler)) THEN
      IF (ASSOCIATED(this%err_handler) .AND. this%owns_err_handler) &
          DEALLOCATE(this%err_handler)
      this%err_handler => NULL()
    END IF

    !If there is no error handler already allocated at this point then
    !either use the one that the user provided, or create one if they user
    !didn't specify one or specified a NULL handler
    IF (.NOT. ASSOCIATED(this%err_handler)) THEN
      IF (PRESENT(err_handler)) THEN
        IF (ASSOCIATED(err_handler)) THEN
          this%err_handler => err_handler
          this%owns_err_handler = .FALSE.
        ELSE
          ALLOCATE(this%err_handler)
          this%owns_err_handler = .TRUE.
        END IF
      ELSE
        ALLOCATE(this%err_handler)
        this%owns_err_handler = .TRUE.
      END IF
    END IF

    !IF specifying a new parser deallocate the old one if needed
    IF (PRESENT(parser) .OR. PRESENT(interop_parser)) THEN
      IF (ASSOCIATED(this%parser) .AND. this%owns_parser) &
          DEALLOCATE(this%parser)
      this%parser => NULL()
      IF (this%owns_interop) &
          CALL eis_release_interop_parser(this%interop_parser)
      this%interop_parser = -1
    END IF

    IF (PRESENT(parser) .AND. PRESENT(interop_parser)) THEN
      this%parser => parser
      this%interop_parser = interop_parser
      this%owns_parser = .FALSE.
      this%owns_interop = .FALSE.
    ELSE IF (PRESENT(parser)) THEN
      this%parser => parser
      this%owns_parser = .FALSE.
      IF (ASSOCIATED(this%parser)) THEN
        IF (this%parser%interop_id > -1) THEN
          this%interop_parser = this%parser%interop_id
          this%owns_interop = .FALSE.
        ELSE
          this%interop_parser = eis_add_interop_parser(parser, owns = .FALSE.)
          this%owns_interop = .TRUE.
        END IF
      ELSE
        this%interop_parser = -1
      END IF
    ELSE IF (PRESENT(interop_parser)) THEN
      this%interop_parser = interop_parser
      this%owns_interop = .FALSE.
      this%parser => eis_get_interop_parser(interop_parser)
      this%owns_parser = .FALSE.
    END IF

    this%definition => definition
    IF (.NOT. ASSOCIATED(this%definition)) THEN
      errcode = eis_err_bad_deck_definition
      CALL this%err_handler%add_error(eis_err_parser, errcode, &
          filename = filename, line_number = line_number)
      RETURN
    END IF

    IF (.NOT. this%definition%is_init) THEN
      errcode = eis_err_bad_deck_definition
      CALL this%err_handler%add_error(eis_err_parser, errcode, &
          filename = filename, line_number = line_number)
      RETURN
    END IF

    IF (.NOT. ALLOCATED(this%current_parents)) &
        ALLOCATE(this%current_parents(10))
    ALLOCATE(root_block)
    ALLOCATE(root_block%block_text, SOURCE = '{ROOT}')
    this%root => root_block
    this%current_block => root_block
    root_block%definition => this%definition%get_root()
    gptr => root_block
    !Root block must have ID 0
    root_block%uid = 0
    CALL this%blocks%hold(0, gptr, owns = .TRUE.)
    CALL this%uid_generator%reset(1)
    CALL this%key_uid_generator%reset(1)
    this%current_level = 1
    this%current_parents(1) = root_block%uid

  END SUBROUTINE edc_init


  !> @brief
  !> Reset the caller so that you can work with a new definition
  !> @param[inout] this
  SUBROUTINE edc_reset(this, errcode)
    CLASS(eis_deck_caller), INTENT(INOUT) :: this
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    LOGICAL :: success

    errcode = eis_err_none
    this%definition => NULL()
    CALL this%blocks%clear()
    success = this%events%clear()

  END SUBROUTINE edc_reset



  !> @brief
  !> Set a general pass number to use for all future calls
  !> @param[inout] this
  !> @param[in] pass_number
  SUBROUTINE edc_set_pass(this, pass_number)
    CLASS(eis_deck_caller), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: pass_number

    this%pass_number = pass_number

  END SUBROUTINE edc_set_pass



  !> @brief
  !> Initialise all of the blocks in the deck
  !> @param[inout] this
  !> @param[inout] errcode
  !> @param[in] pass_number
  !> @param[inout] host_state
  !> @param[in] line_number
  !> @param[in] filename
  SUBROUTINE edc_initialise_all_blocks(this, errcode, pass_number, &
      host_state, line_number, filename)
    CLASS(eis_deck_caller), INTENT(INOUT) :: this
    !> Error code from the initialise operation
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Pass number through the deck. Optional, default 1
    INTEGER, INTENT(IN), OPTIONAL :: pass_number
    !> Optional host state parameter
    INTEGER(eis_bitmask), INTENT(INOUT), OPTIONAL :: host_state
    !> Line number for error reporting
    INTEGER, INTENT(IN), OPTIONAL :: line_number
    !> Filename for error reporting
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: filename

    INTEGER :: pass_l, dummy
    CHARACTER(LEN=:), ALLOCATABLE :: fname_l
    TYPE(eis_deck_caller_event), POINTER :: event
    CLASS(*), POINTER :: gptr

    IF (PRESENT(filename)) THEN
      ALLOCATE(fname_l, SOURCE = filename)
    ELSE
      ALLOCATE(fname_l, SOURCE = this%filename)
    END IF

    IF (.NOT. ASSOCIATED(this%definition)) THEN
      errcode = eis_err_bad_deck_definition
      CALL this%err_handler%add_error(eis_err_deck_parser, errcode, &
          filename = fname_l, line_number = line_number)
      RETURN
    END IF

    pass_l = this%pass_number
    IF (PRESENT(pass_number)) pass_l = pass_number

    errcode = eis_err_none
    CALL this%definition%initialise_blocks(host_state, errcode, &
        pass_number = pass_l)

    ALLOCATE(event)
    gptr => event
    event%type = edce_init
    IF (PRESENT(host_state)) event%host_state = host_state
    ALLOCATE(event%filename, SOURCE = fname_l)
    IF (PRESENT(line_number)) event%line_number = line_number
    event%pass_number = pass_l
    dummy = this%events%hold(gptr, owns = .TRUE.)

    IF (errcode /= eis_err_none) THEN
      CALL this%err_handler%add_error(eis_err_deck_parser, errcode, &
          filename = fname_l, line_number = line_number)
      RETURN
    END IF

  END SUBROUTINE edc_initialise_all_blocks


  !> @brief
  !> Start a new block in the currently started block.
  !> @detail
  !> This routine starts a block by name in the currently started block
  !> By default you start in the root block when you initialise the caller
  !> If the block name is not found then an error will be returned and
  !> the current block is not changed. If the block is found you enter the
  !> context for this block even if starting the block returns an error.
  !> You will have to end the block to return to the parent block context
  !> @param[inout] this
  !> @param[in] name
  !> @param[inout] errcode
  !> @param[in] pass_number
  !> @param[inout] host_state
  !> @param[in] line_number
  !> @param[in] filename
  !> @param[in] comment
  !> @return uid
  FUNCTION edc_start_block(this, name, errcode, pass_number, host_state, &
      line_number, filename, comment) RESULT(uid)
    CLASS(eis_deck_caller), INTENT(INOUT) :: this
    !> Name of the block to start
    CHARACTER(LEN=*), INTENT(IN) :: name
    !> Error code from the start operation
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Pass number through the deck. Optional, default 1
    INTEGER, INTENT(IN), OPTIONAL :: pass_number
    !> Optional host state parameter
    INTEGER(eis_bitmask), INTENT(INOUT), OPTIONAL :: host_state
    !> Line number for error reporting
    INTEGER, INTENT(IN), OPTIONAL :: line_number
    !> Filename for error reporting
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: filename
    !> Optional comment for the block. Will be added to the produced deck
    !> file
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: comment
    !> UID of started block
    INTEGER(uid_kind) :: uid

    TYPE(eis_deck_block_definition), POINTER :: defn
    INTEGER :: pass_l, dummy
    CHARACTER(LEN=:), ALLOCATABLE :: fname_l
    INTEGER(eis_status) :: status
    TYPE(eis_deck_caller_block), POINTER :: new_block
    TYPE(eis_deck_caller_event), POINTER :: event
    CLASS(*), POINTER :: gptr

    IF (PRESENT(filename)) THEN
      ALLOCATE(fname_l, SOURCE = filename)
    ELSE
      ALLOCATE(fname_l, SOURCE = this%filename)
    END IF

    IF (.NOT. ASSOCIATED(this%definition)) THEN
      errcode = eis_err_bad_deck_definition
      CALL this%err_handler%add_error(eis_err_deck_parser, errcode, &
          filename = fname_l, line_number = line_number)
      uid = -1
      RETURN
    END IF

    pass_l = this%pass_number
    IF (PRESENT(pass_number)) pass_l = pass_number

    errcode = eis_err_none
    defn => this%current_block%definition%get_child(name, &
        pass_number = pass_l, errcode = errcode)
    IF (errcode /= eis_err_none) THEN
      CALL this%err_handler%add_error(eis_err_deck_parser, errcode, &
          filename = fname_l, line_number = line_number)
      uid = -1
      RETURN
    END IF

    !Create the new block and link it in to the list of blocks
    ALLOCATE(new_block)
    new_block%parent => this%current_block
    gptr => new_block
    new_block%uid = this%uid_generator%get_next_uid()
    uid = new_block%uid
    CALL this%blocks%hold(new_block%uid, gptr, owns = .TRUE.)
    CALL this%add_parent(new_block%uid)
    new_block%definition => defn
    !Now call the start block functions
    errcode = eis_err_none
    CALL defn%start_block(this%current_parents(1:this%current_level), pass_l, &
        status, errcode, host_state, display_name = name)
    IF (errcode /= eis_err_none) THEN
      !Error starting block but block exists. Report it but you *have* still
      !started the block. You will have to end it to go back up
      CALL this%err_handler%add_error(eis_err_deck_parser, errcode, &
          filename = fname_l, line_number = line_number)
    END IF

    dummy = this%current_block%children%hold(gptr, owns = .FALSE.)
    this%current_block => new_block

    ALLOCATE(new_block%block_text, SOURCE = name)

    !Create the event
    ALLOCATE(event)
    gptr => event
    event%type = edce_start
    IF (PRESENT(host_state)) event%host_state = host_state
    ALLOCATE(event%filename, SOURCE = fname_l)
    IF (PRESENT(line_number)) event%line_number = line_number
    event%pass_number = pass_l
    event%block => new_block
    IF (PRESENT(comment)) THEN
      IF (LEN(TRIM(ADJUSTL(comment))) > 0) THEN
        ALLOCATE(event%block%block_comment, SOURCE = TRIM(ADJUSTL(comment)))
      END IF
    END IF
    dummy = this%events%hold(gptr, owns = .TRUE.)

  END FUNCTION edc_start_block



  !> @brief
  !> End the currently started block
  !> @param[inout] this
  !> @param[inout] errcode
  !> @param[in] pass_number
  !> @param[inout] host_state
  !> @param[in] line_number
  !> @param[in] filename
  SUBROUTINE edc_end_block(this, errcode, pass_number, host_state, &
      line_number, filename)
    CLASS(eis_deck_caller), INTENT(INOUT) :: this
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER, INTENT(IN), OPTIONAL :: pass_number
    INTEGER(eis_bitmask), INTENT(INOUT), OPTIONAL :: host_state
    INTEGER, INTENT(IN), OPTIONAL :: line_number
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: filename

    INTEGER :: pass_l, dummy
    CHARACTER(LEN=:), ALLOCATABLE :: fname_l
    INTEGER(eis_status) :: status
    TYPE(eis_deck_caller_event), POINTER :: event
    CLASS(*), POINTER :: gptr

    IF (PRESENT(filename)) THEN
      ALLOCATE(fname_l, SOURCE = filename)
    ELSE
      ALLOCATE(fname_l, SOURCE = this%filename)
    END IF

    IF (.NOT. ASSOCIATED(this%definition)) THEN
      errcode = eis_err_bad_deck_definition
      CALL this%err_handler%add_error(eis_err_deck_parser, errcode, &
          filename = fname_l, line_number = line_number)
      RETURN
    END IF

    pass_l = this%pass_number
    IF (PRESENT(pass_number)) pass_l = pass_number

    errcode = eis_err_none

    !Call the end block function
    CALL this%current_block%definition%end_block(&
        this%current_parents(1:this%current_level), pass_l, status, errcode, &
        host_state, display_name = this%current_block%block_text)

    !Create the event
    ALLOCATE(event)
    gptr => event
    event%type = edce_end
    IF (PRESENT(host_state)) event%host_state = host_state
    ALLOCATE(event%filename, SOURCE = fname_l)
    IF (PRESENT(line_number)) event%line_number = line_number
    event%pass_number = pass_l
    event%block => this%current_block
    dummy = this%events%hold(gptr, owns = .TRUE.)

    CALL this%remove_parent()
    this%current_block => this%current_block%parent

    IF (errcode /= eis_err_none) THEN
      CALL this%err_handler%add_error(eis_err_deck_parser, errcode, &
          filename = fname_l, line_number = line_number)
      RETURN
    END IF

  END SUBROUTINE edc_end_block



  !> Call a key on the currently selected block
  !> @param[inout] this
  !> @param[in] key_text
  !> @param[inout] errcode
  !> @param[in] pass_number
  !> @param[inout] host_state
  !> @param[in] key_line_number
  !> @param[in] key_filename
  !> @param[in] key_offset
  !> @param[in] value_text
  !> @param[in] value_line_number
  !> @param[in] value_filename
  !> @param[in] value_offset
  !> @param[in] value_function
  !> @param[in] value_function_text
  !> @param[in] parser
  !> @param[in] interop_parser
  !> @param[in] comment
  !> @param[in] host_params
  !> @param[in] trimmed_line
  !> @return uid
  FUNCTION edc_call_key(this, key_text, errcode, pass_number, host_state, &
      key_line_number, key_filename, key_offset, value_text, &
      value_line_number, value_filename, value_offset, value_function, &
      value_function_text, parser, interop_parser, comment, host_params, &
      trimmed_line_text) RESULT(uid)
    CLASS(eis_deck_caller), INTENT(INOUT) :: this
    !> Text of the key to be called
    CHARACTER(LEN=*), INTENT(IN) :: key_text
    !> Error code from the function
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Pass through the deck to call the key on. Optional, default use object
    !> pass number, set with "set_pass"
    INTEGER, INTENT(IN), OPTIONAL :: pass_number
    !> Optional host_state parameter, used to pass a single eis_bitmask in or
    !> out of the deck parser code. Optional and set to zero if not used.
    !> Typical use cases would be reporting errors that are not related to EIS
    INTEGER(eis_bitmask), INTENT(INOUT), OPTIONAL :: host_state
    !> Optional line number to be used when reporting errors in the key
    INTEGER, INTENT(IN), OPTIONAL :: key_line_number
    !> Optional filename to be used when reporting errors in the key
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: key_filename
    !> Optional parameter for the offset from the start of the line for the key
    !> When reporting errors
    INTEGER, INTENT(IN), OPTIONAL :: key_offset
    !> Optional text to specify the value of a key
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: value_text
    !> Optional line number to be used when reporting errors in the value
    INTEGER, INTENT(IN), OPTIONAL :: value_line_number
    !> Optional filename to be used when reporting errors in the value
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: value_filename
    !> Optional parameter for the offset from the start of the line for the
    !> value when reporting errors
    INTEGER, INTENT(IN), OPTIONAL :: value_offset
    !> Optional callback function for the value of a stack.If not present
    !> then "key_text" must evaluate to a valid parser expression
    PROCEDURE(parser_result_function), OPTIONAL :: value_function
    !> Optional string of the source of the function specified in 
    !> "value_function". Used when reporting the state of the deck
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: value_function_text
    !> Optional eis_parser object to use to evaluate the stack.
    !> If not present then the parser object specified when the
    !> eis_deck_caller object was initialised
    CLASS(eis_parser), INTENT(IN), POINTER, OPTIONAL :: parser
    !> Optional interop_parser code. Used to evaluate the stack as needed
    INTEGER, INTENT(IN), OPTIONAL :: interop_parser
    !> Optional comment. Will be used in a generated deck file from the calling
    !> sequence
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: comment
    !> Optional host parameter object to use when parsing text
    TYPE(C_PTR), INTENT(IN), OPTIONAL :: host_params
    !> Optional string of the whole line including both key and value.
    !> Is used with eis_key_text callbacks. key=value is generated automatically
    !> if this is not specified. Any leading spaces included in offset should be
    !> trimmed off
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: trimmed_line_text
    !> Unique ID of the key
    INTEGER(uid_kind) :: uid

    INTEGER :: pass_l, dummy
    CLASS(eis_parser), POINTER :: parser_l
    INTEGER :: interop_parser_l
    TYPE(eis_deck_caller_key), POINTER :: key_info
    LOGICAL :: release_interop, has_value
    CLASS(*), POINTER :: gptr
    INTEGER(eis_bitmask) :: host_state_l
    TYPE(eis_deck_caller_event), POINTER :: event

    pass_l = this%pass_number
    IF (PRESENT(pass_number)) pass_l = pass_number

    host_state_l = 0_eis_bitmask
    IF (PRESENT(host_state)) host_state_l = host_state

    release_interop = .FALSE.
    parser_l => this%parser
    IF (PRESENT(parser)) parser_l => parser
    interop_parser_l = this%interop_parser
    IF (PRESENT(interop_parser)) interop_parser_l = interop_parser

    IF (.NOT. ASSOCIATED(parser_l) .AND. interop_parser_l > -1) THEN
      parser_l => eis_get_interop_parser(interop_parser_l)
    END IF

    IF (interop_parser_l < 0) THEN
      IF (ASSOCIATED(parser_l)) THEN
        IF (parser_l%interop_id > -1) THEN
          interop_parser_l = parser_l%interop_id
        ELSE
          interop_parser_l = eis_add_interop_parser(parser_l, owns = .FALSE.)
          release_interop = .TRUE.
        END IF
      END IF
    END IF

    errcode = eis_err_none

    ALLOCATE(key_info)
    key_info%uid = this%key_uid_generator%get_next_uid()
    uid = key_info%uid
    ALLOCATE(key_info%key_text, SOURCE = key_text)
    IF (PRESENT(value_text)) THEN
      ALLOCATE(key_info%value_text, SOURCE = value_text)
    END IF
    IF (PRESENT(key_filename)) THEN
      ALLOCATE(key_info%key_filename, SOURCE = key_filename)
    ELSE
      ALLOCATE(key_info%key_filename, SOURCE = this%filename)
    END IF
    IF (PRESENT(value_filename)) THEN
      ALLOCATE(key_info%value_filename, SOURCE = value_filename)
    ELSE
      ALLOCATE(key_info%value_filename, SOURCE = this%filename)
    END IF

    IF (PRESENT(key_line_number)) &
        key_info%key_line_number = key_line_number
    IF (PRESENT(value_line_number)) &
        key_info%value_line_number = value_line_number

    IF (PRESENT(key_offset)) &
        key_info%key_offset = key_offset
    IF (PRESENT(value_offset)) &
        key_info%value_offset = value_offset

    IF (PRESENT(value_function)) key_info%value_function => value_function
    IF (PRESENT(value_function_text)) ALLOCATE(key_info%function_text, &
        SOURCE =  value_function_text)
    key_info%parser => parser_l
    IF (.NOT. release_interop) key_info%interop_parser = interop_parser_l

    IF (PRESENT(trimmed_line_text)) ALLOCATE(key_info%trimmed_line_text, &
        SOURCE = trimmed_line_text)

    IF (.NOT. ASSOCIATED(this%definition)) THEN
      errcode = eis_err_bad_deck_definition
      CALL this%err_handler%add_error(eis_err_deck_parser, errcode, &
          filename = key_info%key_filename, &
          line_number = key_info%key_line_number)
      RETURN
    END IF

    gptr => key_info
    dummy = this%current_block%keys%hold(gptr, owns = .FALSE.)
    CALL this%keys%hold(uid, gptr, owns = .TRUE.)

    has_value = PRESENT(value_text)
    IF (has_value) has_value = has_value .AND. LEN(value_text) > 0

    IF (.NOT. has_value) THEN
      CALL this%current_block%definition%call_key(key_text, &
          this%current_parents(1:this%current_level), pass_l, host_state_l, &
          errcode, key_filename = key_info%key_filename, &
          key_line_number = key_info%key_line_number, &
          key_offset = key_info%key_offset, &
          value_function = value_function, parser = parser_l, &
          interop_parser_id = interop_parser_l, &
          host_params = host_params, trimmed_line_text = trimmed_line_text)
    ELSE
      CALL this%current_block%definition%call_key_text(key_text, &
          this%current_parents(1:this%current_level), pass_l, host_state_l, &
          errcode, key_filename = key_info%key_filename, &
          key_line_number = key_info%key_line_number, &
          key_offset = key_info%key_offset, &
          key_value = value_text, &
          value_line_number = key_info%value_line_number, &
          value_filename = key_info%value_filename, &
          value_offset = key_info%value_offset, &
          value_function = value_function, parser = parser_l, &
          interop_parser_id = interop_parser_l, &
          host_params = host_params, trimmed_line_text = trimmed_line_text)
    END IF

    IF (release_interop) CALL eis_release_interop_parser(interop_parser_l)

    !Create the event
    ALLOCATE(event)
    gptr => event
    event%type = edce_call
    IF (PRESENT(host_state)) event%host_state = host_state
    IF (PRESENT(host_params)) event%host_params = host_params
    ALLOCATE(event%filename, SOURCE = key_info%key_filename)
    IF (PRESENT(key_line_number)) event%line_number = key_info%key_line_number
    event%pass_number = pass_l
    event%key => key_info
    IF (PRESENT(comment)) THEN
      IF (LEN(TRIM(ADJUSTL(comment))) > 0) THEN
        ALLOCATE(event%key%key_comment, SOURCE = TRIM(ADJUSTL(comment)))
      END IF
    END IF
    dummy = this%events%hold(gptr, owns = .TRUE.)

    IF (errcode /= eis_err_none) THEN
      CALL this%err_handler%add_error(eis_err_deck_parser, errcode, &
          filename = key_info%key_filename, &
          line_number = key_info%key_line_number)
      RETURN
    END IF
  
  END FUNCTION edc_call_key



  !> @brief
  !> End and finalise the currently started block
  !> @param[inout] this
  !> @param[inout] errcode
  !> @param[in] pass_number
  !> @param[inout] host_state
  !> @param[in] line_number
  !> @param[in] filename
  SUBROUTINE edc_end_and_final_block(this, errcode, pass_number, host_state, &
      line_number, filename)
    CLASS(eis_deck_caller), INTENT(INOUT) :: this
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER, INTENT(IN), OPTIONAL :: pass_number
    INTEGER(eis_bitmask), INTENT(INOUT), OPTIONAL :: host_state
    INTEGER, INTENT(IN), OPTIONAL :: line_number
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: filename

    INTEGER :: pass_l, dummy
    CHARACTER(LEN=:), ALLOCATABLE :: fname_l
    INTEGER(eis_status) :: status
    INTEGER(eis_error) :: errcode_l
    TYPE(eis_deck_caller_event), POINTER :: event
    CLASS(*), POINTER :: gptr

    IF (PRESENT(filename)) THEN
      ALLOCATE(fname_l, SOURCE = filename)
    ELSE
      ALLOCATE(fname_l, SOURCE = this%filename)
    END IF

    IF (.NOT. ASSOCIATED(this%definition)) THEN
      errcode = eis_err_bad_deck_definition
      CALL this%err_handler%add_error(eis_err_deck_parser, errcode, &
          filename = fname_l, line_number = line_number)
      RETURN
    END IF

    pass_l = this%pass_number
    IF (PRESENT(pass_number)) pass_l = pass_number

    errcode_l = eis_err_none

    !Call the end block function
    CALL this%current_block%definition%end_block(&
        this%current_parents(1:this%current_level), pass_l, status, errcode_l, &
        host_state, display_name = this%current_block%block_text)
    IF (errcode_l /= eis_err_none) THEN
      CALL this%err_handler%add_error(eis_err_deck_parser, errcode_l, &
          filename = fname_l, line_number = line_number)
    END IF
    !Create the event
    ALLOCATE(event)
    gptr => event
    event%type = edce_end
    IF (PRESENT(host_state)) event%host_state = host_state
    ALLOCATE(event%filename, SOURCE = fname_l)
    IF (PRESENT(line_number)) event%line_number = line_number
    event%pass_number = pass_l
    event%block => this%current_block
    dummy = this%events%hold(gptr, owns = .TRUE.)

    errcode = IOR(errcode, errcode_l)
    errcode_l = eis_err_none
    CALL this%current_block%definition%finalise_block(pass_l, .TRUE., status, &
        errcode_l, host_state)

    !Create the event
    ALLOCATE(event)
    gptr => event
    event%type = edce_final
    IF (PRESENT(host_state)) event%host_state = host_state
    ALLOCATE(event%filename, SOURCE = fname_l)
    IF (PRESENT(line_number)) event%line_number = line_number
    event%pass_number = pass_l
    event%block => this%current_block
    dummy = this%events%hold(gptr, owns = .TRUE.)

    CALL this%remove_parent()
    this%current_block => this%current_block%parent

    IF (errcode_l /= eis_err_none) THEN
      CALL this%err_handler%add_error(eis_err_deck_parser, errcode_l, &
          filename = fname_l, line_number = line_number)
    END IF
    errcode = IOR(errcode, errcode_l)

  END SUBROUTINE edc_end_and_final_block



  !> @brief
  !> Finalise all of the blocks in the deck
  !> @param[inout] this
  !> @param[inout] errcode
  !> @param[in] pass_number
  !> @param[inout] host_state
  !> @param[in] line_number
  !> @param[in] filename
  SUBROUTINE edc_finalise_all_blocks(this, errcode, pass_number, &
      host_state, line_number, filename)
    CLASS(eis_deck_caller), INTENT(INOUT) :: this
    !> Error code from the finalise operation
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Pass number through the deck. Optional, default 1
    INTEGER, INTENT(IN), OPTIONAL :: pass_number
    !> Optional host state parameter
    INTEGER(eis_bitmask), INTENT(INOUT), OPTIONAL :: host_state
    !> Line number for error reporting
    INTEGER, INTENT(IN), OPTIONAL :: line_number
    !> Filename for error reporting
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: filename

    INTEGER :: pass_l, dummy
    CHARACTER(LEN=:), ALLOCATABLE :: fname_l
    TYPE(eis_deck_caller_event), POINTER :: event
    CLASS(*), POINTER :: gptr

    IF (PRESENT(filename)) THEN
      ALLOCATE(fname_l, SOURCE = filename)
    ELSE
      ALLOCATE(fname_l, SOURCE = this%filename)
    END IF

    IF (.NOT. ASSOCIATED(this%definition)) THEN
      errcode = eis_err_bad_deck_definition
      CALL this%err_handler%add_error(eis_err_deck_parser, errcode, &
          filename = fname_l, line_number = line_number)
      RETURN
    END IF

    pass_l = this%pass_number
    IF (PRESENT(pass_number)) pass_l = pass_number

    errcode = eis_err_none
    CALL this%definition%finalise_blocks(host_state, errcode, &
        pass_number = pass_l)

    ALLOCATE(event)
    gptr => event
    event%type = edce_final
    IF (PRESENT(host_state)) event%host_state = host_state
    ALLOCATE(event%filename, SOURCE = fname_l)
    IF (PRESENT(line_number)) event%line_number = line_number
    event%pass_number = pass_l
    dummy = this%events%hold(gptr, owns = .TRUE.)

    IF (errcode /= eis_err_none) THEN
      CALL this%err_handler%add_error(eis_err_deck_parser, errcode, &
          filename = fname_l, line_number = line_number)
      RETURN
    END IF

  END SUBROUTINE edc_finalise_all_blocks



  !> @brief
  !> End pass for all of the blocks in the deck
  !> @param[inout] this
  !> @param[inout] errcode
  !> @param[in] pass_number
  !> @param[inout] host_state
  !> @param[in] line_number
  !> @param[in] filename
  SUBROUTINE edc_end_pass(this, errcode, pass_number, &
      host_state, line_number, filename)
    CLASS(eis_deck_caller), INTENT(INOUT) :: this
    !> Error code from the end pass operation
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Pass number through the deck. Optional, default 1
    INTEGER, INTENT(IN), OPTIONAL :: pass_number
    !> Optional host state parameter
    INTEGER(eis_bitmask), INTENT(INOUT), OPTIONAL :: host_state
    !> Line number for error reporting
    INTEGER, INTENT(IN), OPTIONAL :: line_number
    !> Filename for error reporting
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: filename

    INTEGER :: pass_l, dummy
    CHARACTER(LEN=:), ALLOCATABLE :: fname_l
    TYPE(eis_deck_caller_event), POINTER :: event
    CLASS(*), POINTER :: gptr

    IF (PRESENT(filename)) THEN
      ALLOCATE(fname_l, SOURCE = filename)
    ELSE
      ALLOCATE(fname_l, SOURCE = this%filename)
    END IF

    IF (.NOT. ASSOCIATED(this%definition)) THEN
      errcode = eis_err_bad_deck_definition
      CALL this%err_handler%add_error(eis_err_deck_parser, errcode, &
          filename = fname_l, line_number = line_number)
      RETURN
    END IF

    pass_l = this%pass_number
    IF (PRESENT(pass_number)) pass_l = pass_number

    errcode = eis_err_none
    CALL this%definition%end_pass(host_state, errcode, &
        pass_number = pass_l)

    ALLOCATE(event)
    gptr => event
    event%type = edce_end_pass
    IF (PRESENT(host_state)) event%host_state = host_state
    ALLOCATE(event%filename, SOURCE = fname_l)
    IF (PRESENT(line_number)) event%line_number = line_number
    event%pass_number = pass_l
    dummy = this%events%hold(gptr, owns = .TRUE.)

    IF (errcode /= eis_err_none) THEN
      CALL this%err_handler%add_error(eis_err_deck_parser, errcode, &
          filename = fname_l, line_number = line_number)
      RETURN
    END IF

  END SUBROUTINE edc_end_pass



  !> Get the number of errors reported on this deck parse
  !> @param[inout] this
  !> @return count
  FUNCTION edc_get_error_count(this) RESULT(count)
    CLASS(eis_deck_caller), INTENT(IN) :: this
    INTEGER :: count !< Number of errors reported

    count = this%err_handler%get_error_count()

  END FUNCTION edc_get_error_count



  !> @brief
  !> Get the error report on a specified error
  !> @param[inout] this
  !> @param[in] index
  !> @param[out] report
  SUBROUTINE edc_get_error_report(this, index, report)
    CLASS(eis_deck_caller), INTENT(IN) :: this
    !> Index of error to get report on. Must be between 1 and
    !> the result of eip_get_error_count
    INTEGER, INTENT(IN) :: index
    !> Allocatable string variable containing the error report
    !> will be reallocated to be exactly long enough to store
    !> the error report whether allocated or not
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: report

    CALL this%err_handler%get_error_report(index, report)
  END SUBROUTINE edc_get_error_report



  !> @brief
  !> Print all of the errors to stdout
  !> @param[inout] this
  SUBROUTINE edc_flush_errors(this)
    CLASS(eis_deck_caller) :: this

    CALL this%err_handler%flush_errors()

  END SUBROUTINE edc_flush_errors


  !> @brief
  !> Replay a deck again with a different pass number
  !> @param[inout] this
  !> @param[inout] errcode
  !> @param[in] pass_number
  !> @param[in] host_state
  !> @param[in] replay_control_blocks
  SUBROUTINE edc_replay_deck(this, errcode, pass_number, host_state, &
      replay_control_blocks)
    CLASS(eis_deck_caller), INTENT(INOUT) :: this
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER, INTENT(IN), OPTIONAL :: pass_number
    INTEGER(eis_bitmask), INTENT(INOUT), OPTIONAL :: host_state
    LOGICAL, INTENT(IN), OPTIONAL :: replay_control_blocks
    INTEGER :: ievent, pass_l, interop_parser_l
    INTEGER(eis_status) :: status
    CLASS(*), POINTER :: gptr
    CLASS(eis_deck_caller_event), POINTER :: sptr
    TYPE(eis_deck_block_definition), POINTER :: defn
    CLASS(eis_parser), POINTER :: parser_l
    LOGICAL :: release_interop, include_control
    INTEGER(eis_bitmask) :: host_state_l

    errcode = eis_err_none

    IF (.NOT. ASSOCIATED(this%definition)) THEN
      errcode = eis_err_bad_deck_definition
      CALL this%err_handler%add_error(eis_err_deck_parser, errcode)
      RETURN
    END IF

    include_control = .FALSE.
    IF (PRESENT(replay_control_blocks)) include_control = replay_control_blocks

    this%current_level = 1
    this%current_block => this%root

    DO ievent = 1, this%events%get_size()
      gptr => this%events%get(ievent)
      SELECT TYPE (gptr)
        CLASS IS (eis_deck_caller_event)
          sptr => gptr
        CLASS DEFAULT
          sptr => NULL()
      END SELECT
      IF (.NOT. ASSOCIATED(sptr)) CYCLE
      pass_l = sptr%pass_number
      IF (PRESENT(pass_number)) pass_l = pass_number
      host_state_l = sptr%host_state
      IF (PRESENT(host_state)) host_state_l = host_state

      errcode = eis_err_none

      SELECT CASE (sptr%type)
        CASE (edce_init)
          IF (.NOT. ASSOCIATED(sptr%block)) THEN
            CALL this%definition%initialise_blocks(host_state_l, errcode, &
              pass_number = pass_l)
          ELSE
            !Currently no initialise for single blocks
          END IF
        CASE (edce_start)
          defn => this%current_block%definition%get_child(&
            sptr%block%block_text, pass_number = pass_l, errcode = errcode)
          CALL this%add_parent(sptr%block%uid)
          CALL defn%start_block(this%current_parents(1:this%current_level), &
              pass_l, status, errcode, host_state_l, &
              display_name = sptr%block%block_text)
          this%current_block => sptr%block
        CASE (edce_call)

          release_interop = .FALSE.
          IF (sptr%key%interop_parser > -1) THEN
            interop_parser_l = sptr%key%interop_parser
          ELSE
            IF (ASSOCIATED(sptr%key%parser)) THEN
              IF (sptr%key%parser%interop_id > -1) THEN
                interop_parser_l = sptr%key%parser%interop_id
              ELSE
                interop_parser_l = eis_add_interop_parser(parser_l, &
                    owns = .FALSE.)
                release_interop = .TRUE.
              END IF
            ELSE
              interop_parser_l = -1
            END IF
          END IF

          IF (ASSOCIATED(sptr%key%parser)) THEN
            parser_l => sptr%key%parser
          ELSE IF (interop_parser_l > -1) THEN
            parser_l => eis_get_interop_parser(interop_parser_l)
          ELSE
            parser_l => NULL()
          END IF

          IF (.NOT. ALLOCATED(sptr%key%value_text)) THEN
            IF (ALLOCATED(sptr%key%trimmed_line_text)) THEN
              CALL this%current_block%definition%call_key(sptr%key%key_text, &
                  this%current_parents(1:this%current_level), pass_l, &
                  host_state_l, errcode, key_filename = sptr%filename, &
                  key_line_number = sptr%line_number, &
                  key_offset = sptr%key%key_offset, &
                  value_function = sptr%key%value_function, &
                  parser = parser_l, &
                  interop_parser_id = interop_parser_l, &
                  host_params = sptr%host_params, &
                  trimmed_line_text = sptr%key%trimmed_line_text)
            ELSE
              CALL this%current_block%definition%call_key(sptr%key%key_text, &
                  this%current_parents(1:this%current_level), pass_l, &
                  host_state_l, errcode, key_filename = sptr%filename, &
                  key_line_number = sptr%line_number, &
                  key_offset = sptr%key%key_offset, &
                  value_function = sptr%key%value_function, &
                  parser = parser_l, &
                  interop_parser_id = interop_parser_l, &
                  host_params = sptr%host_params)
            END IF
          ELSE
            IF (ALLOCATED(sptr%key%trimmed_line_text)) THEN
              CALL this%current_block%definition%call_key(sptr%key%key_text, &
                  this%current_parents(1:this%current_level), pass_l, &
                  host_state_l, errcode, key_filename = sptr%filename, &
                  key_line_number = sptr%line_number, &
                  key_offset = sptr%key%key_offset, &
                  key_value = sptr%key%value_text, &
                  value_filename = sptr%key%value_filename, &
                  value_line_number = sptr%key%value_line_number, &
                  value_offset = sptr%key%key_offset, &
                  value_function = sptr%key%value_function, &
                  parser = parser_l, &
                  interop_parser_id = interop_parser_l, &
                  host_params = sptr%host_params, &
                  trimmed_line_text = sptr%key%trimmed_line_text)
            ELSE
              CALL this%current_block%definition%call_key(sptr%key%key_text, &
                  this%current_parents(1:this%current_level), pass_l, &
                  host_state_l, errcode, key_filename = sptr%filename, &
                  key_line_number = sptr%line_number, &
                  key_offset = sptr%key%key_offset, &
                  key_value = sptr%key%value_text, &
                  value_filename = sptr%key%value_filename, &
                  value_line_number = sptr%key%value_line_number, &
                  value_offset = sptr%key%key_offset, &
                  value_function = sptr%key%value_function, &
                  parser = parser_l, &
                  interop_parser_id = interop_parser_l, &
                  host_params = sptr%host_params)
            END IF
          END IF

          IF (release_interop) CALL eis_release_interop_parser(interop_parser_l)
        CASE (edce_end)

          CALL defn%end_block(this%current_parents(1:this%current_level), &
              pass_l, status, errcode, host_state_l, &
              display_name = sptr%block%block_text)
          CALL this%remove_parent()
          this%current_block => this%current_block%parent

        CASE (edce_end_pass)
          IF (include_control) THEN
            IF (ASSOCIATED(sptr%block)) THEN
              CALL this%current_block%definition%end_pass_block(pass_l, &
                  errcode, host_state_l)
            ELSE
              CALL this%definition%end_pass(host_state_l, errcode, &
                  pass_number = pass_l)
            END IF
          END IF

        CASE (edce_final)
          IF (include_control) THEN
            IF (ASSOCIATED(sptr%block)) THEN
              CALL this%current_block%definition%finalise_block(pass_l, &
                  .TRUE., status, errcode, host_state_l)
            ELSE
              CALL this%definition%finalise_blocks(host_state_l, errcode, &
                  pass_number = pass_l)
            END IF
          END IF
      END SELECT

      IF (errcode /= eis_err_none) THEN
        CALL this%err_handler%add_error(eis_err_deck_parser, errcode, &
            filename = sptr%filename, line_number = sptr%line_number)
        RETURN
      END IF

    END DO

  END SUBROUTINE edc_replay_deck



  SUBROUTINE edc_generate_deck(this, deck_string, errcode)
    CLASS(eis_deck_caller), INTENT(INOUT) :: this
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: deck_string
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER :: ievent
    CLASS(*), POINTER :: gptr
    CLASS(eis_deck_caller_event), POINTER :: sptr

    errcode = eis_err_none

    CALL eis_append_string(deck_string, '/*Generated by EIS-2 from a deck &
        &caller sequence */')
    CALL eis_append_string(deck_string, '')

    this%current_level = 1
    this%current_block => this%root

    DO ievent = 1, this%events%get_size()
      gptr => this%events%get(ievent)
      SELECT TYPE (gptr)
        CLASS IS (eis_deck_caller_event)
          sptr => gptr
        CLASS DEFAULT
          sptr => NULL()
      END SELECT
      SELECT CASE (sptr%type)
        CASE (edce_start)
          IF (ALLOCATED(sptr%block%block_comment)) &
              CALL eis_append_string(deck_string, &
              REPEAT(" ", 2 * this%current_level) // '/*' &
              // sptr%block%block_comment // '*/')
          CALL eis_append_string(deck_string, &
              REPEAT(" ", 2 * this%current_level) // "begin : " &
              // sptr%block%block_text)
          CALL this%add_parent(sptr%block%uid)
          this%current_block => sptr%block
        CASE (edce_call)
          IF (ALLOCATED(sptr%key%key_comment)) &
              CALL eis_append_string(deck_string, &
              REPEAT(" ", 2 * this%current_level) // '/*' &
              // sptr%key%key_comment // '*/')
          IF (.NOT. ASSOCIATED(sptr%key%value_function)) THEN
            CALL eis_append_string(deck_string, &
                REPEAT(" ", 2 * this%current_level) // sptr%key%key_text &
                // ' = ' // sptr%key%value_text)
          ELSE
            CALL eis_append_string(deck_string, '/*')
            CALL eis_append_string(deck_string, &
                REPEAT(" ", 2 * this%current_level) // 'External function')
            IF (ALLOCATED(sptr%key%function_text)) THEN
              CALL eis_append_string(deck_string, &
                  sptr%key%function_text // '*/')
            END IF
            CALL eis_append_string(deck_string, &
                REPEAT(" ", 2 * this%current_level) // sptr%key%key_text  &
                // ' = ' // sptr%key%value_text)
          END IF
        CASE (edce_end)
          CALL this%remove_parent()
          this%current_block => this%current_block%parent
          CALL eis_append_string(deck_string, &
              REPEAT(" ", 2 * this%current_level) // "end : " &
              // sptr%block%block_text)
          IF (ievent /= this%events%get_size()) &
              CALL eis_append_string(deck_string, "")
      END SELECT
    END DO
  

  END SUBROUTINE edc_generate_deck

END MODULE eis_deck_caller_mod
