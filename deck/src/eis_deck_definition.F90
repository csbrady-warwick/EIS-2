MODULE eis_deck_definition_mod

  USE :: eis_header
  USE :: eis_parser_constants
  USE :: eis_parser_mod
  USE :: eis_deck_function_mod
  USE :: eis_named_store_mod
  USE :: eis_numbered_store_mod
  IMPLICIT NONE

  PRIVATE
  INTEGER, PARAMETER :: npass_global = 1
  

  TYPE :: eis_deck_definition_info
    INTEGER :: id_max = -1 !Want live UID to start from 0
    INTEGER :: longest_block_name = 10 !Minimum of 10
    TYPE(numbered_store) :: all_blocks
    LOGICAL :: owns_parser = .FALSE.
    CLASS(eis_parser), POINTER :: parser => NULL()
    LOGICAL :: owns_interop = .FALSE.
    INTEGER :: interop_parser = -1
    PROCEDURE(event_callback), POINTER, NOPASS :: on_key_success_fn => NULL()
    PROCEDURE(event_callback_c), POINTER, NOPASS :: c_on_key_success_fn &
        => NULL()
    PROCEDURE(event_callback), POINTER, NOPASS :: on_key_failure_fn => NULL()
    PROCEDURE(event_callback_c), POINTER, NOPASS :: c_on_key_failure_fn &
        => NULL()
    PROCEDURE(event_callback), POINTER, NOPASS :: on_key_no_trigger_fn => NULL()
    PROCEDURE(event_callback_c), POINTER, NOPASS :: c_on_key_no_trigger_fn &
        => NULL()

    PROCEDURE(event_callback), POINTER, NOPASS :: on_block_start_fn => NULL()
    PROCEDURE(event_callback_c), POINTER, NOPASS :: c_on_block_start_fn &
        => NULL()
    PROCEDURE(event_callback), POINTER, NOPASS :: on_block_end_fn => NULL()
    PROCEDURE(event_callback_c), POINTER, NOPASS :: c_on_block_end_fn &
        => NULL()
    PROCEDURE(event_callback), POINTER, NOPASS :: on_block_failure_fn => NULL()
    PROCEDURE(event_callback_c), POINTER, NOPASS :: c_on_block_failure_fn &
        => NULL()
    PROCEDURE(event_callback), POINTER, NOPASS :: on_block_no_trigger_fn &
        => NULL()
    PROCEDURE(event_callback_c), POINTER, NOPASS :: c_on_block_no_trigger_fn &
        => NULL()

    PROCEDURE(generic_event_callback), POINTER, NOPASS :: &
        on_generic_init_fn => NULL()
    PROCEDURE(generic_event_callback_c), POINTER, NOPASS :: &
        c_on_generic_init_fn => NULL()

    PROCEDURE(generic_event_callback), POINTER, NOPASS :: &
        on_generic_start_pass_fn => NULL()
    PROCEDURE(generic_event_callback_c), POINTER, NOPASS :: &
        c_on_generic_start_pass_fn => NULL()

    PROCEDURE(generic_event_callback), POINTER, NOPASS :: &
        on_generic_end_pass_fn => NULL()
    PROCEDURE(generic_event_callback_c), POINTER, NOPASS :: &
        c_on_generic_end_pass_fn => NULL()

    PROCEDURE(generic_event_callback), POINTER, NOPASS :: &
        on_generic_final_fn => NULL()
    PROCEDURE(generic_event_callback_c), POINTER, NOPASS :: &
        c_on_generic_final_fn => NULL()

    PROCEDURE(generic_event_callback), POINTER, NOPASS :: &
        on_generic_failure_fn => NULL()
    PROCEDURE(generic_event_callback_c), POINTER, NOPASS :: &
        c_on_generic_failure_fn => NULL()

    CONTAINS

    PROCEDURE :: get_id => ddi_get_next_id
    PROCEDURE :: add_block => ddi_add_block
    PROCEDURE :: get_block_count => ddi_get_block_count
    PROCEDURE :: get_block => ddi_get_block
  END TYPE eis_deck_definition_info

  TYPE :: deck_key_definition

    CHARACTER(LEN=:), ALLOCATABLE :: description
    LOGICAL :: is_hidden = .FALSE.

    INTEGER(INT32), POINTER :: i32data => NULL()
    INTEGER(INT64), POINTER :: i64data => NULL()
    REAL(REAL32), POINTER :: r32data => NULL()
    REAL(REAL64), POINTER :: r64data => NULL()
    LOGICAL, POINTER :: logicaldata => NULL()

    INTEGER(INT32), DIMENSION(:), POINTER :: i32arraydata => NULL()
    INTEGER(INT64), DIMENSION(:), POINTER :: i64arraydata => NULL()
    REAL(REAL32), DIMENSION(:), POINTER :: r32arraydata => NULL()
    REAL(REAL64), DIMENSION(:), POINTER :: r64arraydata => NULL()
    LOGICAL, DIMENSION(:), POINTER :: logicalarraydata => NULL()

    LOGICAL, POINTER :: set_variable => NULL()
    INTEGER(INT32), POINTER :: i32count_variable => NULL()
    INTEGER(INT64), POINTER :: i64count_variable => NULL()

    TYPE(C_PTR) :: c_i32data = C_NULL_PTR
    INTEGER :: c_i32len = 1
    TYPE(C_PTR) :: c_i64data = C_NULL_PTR
    INTEGER :: c_i64len = 1
    TYPE(C_PTR) :: c_r32data = C_NULL_PTR
    INTEGER :: c_r32len = 1
    TYPE(C_PTR) :: c_r64data = C_NULL_PTR
    INTEGER :: c_r64len = 1

    CHARACTER(LEN=:), ALLOCATABLE :: name
    INTEGER :: expected_params = -1
    INTEGER :: pass_eq = -1, pass_le = -1, pass_ge = -1
    LOGICAL :: use_eq = .FALSE., use_le = .FALSE., use_ge = .FALSE.
    LOGICAL :: text_fallback = .FALSE.
    PROCEDURE(key_text_callback), POINTER, NOPASS  :: key_text_fn  => NULL()
    PROCEDURE(key_text_callback_c), POINTER, NOPASS  :: c_key_text_fn  => NULL()
    PROCEDURE(key_value_callback), POINTER, NOPASS :: key_value_fn => NULL()
    PROCEDURE(key_value_callback_c), POINTER, NOPASS :: c_key_value_fn => NULL()
    PROCEDURE(key_numeric_value_callback), POINTER, NOPASS :: &
        key_numeric_value_fn => NULL()
    PROCEDURE(key_numeric_value_callback_c), POINTER, NOPASS :: &
        c_key_numeric_value_fn => NULL()
    PROCEDURE(key_stack_callback), POINTER, NOPASS :: key_stack_fn => NULL()
    PROCEDURE(key_stack_callback_c), POINTER, NOPASS :: c_key_stack_fn => NULL()
    PROCEDURE(should_key_trigger_callback), POINTER, NOPASS :: &
        should_key_trigger_fn => NULL()
    PROCEDURE(should_key_trigger_callback_c), POINTER, NOPASS :: &
        c_should_key_trigger_fn => NULL()

    CONTAINS
    PROCEDURE :: init => dkd_init
  END TYPE deck_key_definition

  TYPE :: eis_deck_block_definition
    TYPE(named_store) :: sub_blocks
    TYPE(named_store) :: keys

    CHARACTER(LEN=:), ALLOCATABLE :: description
    LOGICAL :: is_hidden = .FALSE.

    LOGICAL, POINTER :: set_variable => NULL()
    INTEGER(INT32), POINTER :: i32count_variable => NULL()
    INTEGER(INT64), POINTER :: i64count_variable => NULL()

    INTEGER :: pass_eq = -1, pass_le = -1, pass_ge = -1
    LOGICAL :: use_eq = .FALSE., use_le = .FALSE., use_ge = .FALSE.
    LOGICAL :: text_fallback = .FALSE.

    CHARACTER(LEN=:), ALLOCATABLE :: name
    CLASS(eis_deck_definition_info), POINTER :: info => NULL()
    INTEGER :: id = -1, parent = -1, depth = -1
    INTEGER :: lastinit = 0
    PROCEDURE(block_generic_callback), POINTER, NOPASS :: init_block_fn &
        => NULL()
    PROCEDURE(block_generic_callback_c), POINTER, NOPASS :: c_init_block_fn &
        => NULL()
    PROCEDURE(block_generic_callback), POINTER, NOPASS :: start_pass_block_fn &
        => NULL()
    PROCEDURE(block_generic_callback_c), POINTER, NOPASS :: &
        c_start_pass_block_fn => NULL()
    PROCEDURE(block_callback), POINTER, NOPASS :: start_block_fn => NULL()
    PROCEDURE(block_callback_c), POINTER, NOPASS :: c_start_block_fn => NULL()
    PROCEDURE(block_callback), POINTER, NOPASS :: end_block_fn => NULL()
    PROCEDURE(block_callback_c), POINTER, NOPASS :: c_end_block_fn => NULL()
    PROCEDURE(block_generic_callback), POINTER, NOPASS :: end_pass_block_fn &
        => NULL()
    PROCEDURE(block_generic_callback_c), POINTER, NOPASS :: &
        c_end_pass_block_fn => NULL()
    PROCEDURE(block_generic_callback), POINTER, NOPASS :: final_block_fn &
        => NULL()
    PROCEDURE(block_generic_callback_c), POINTER, NOPASS :: c_final_block_fn &
        => NULL()
    PROCEDURE(block_remap_callback), POINTER, NOPASS :: block_remap_fn &
        => NULL()
    PROCEDURE(block_remap_callback_c), POINTER, NOPASS :: c_block_remap_fn &
        => NULL()
    PROCEDURE(key_text_callback), POINTER, NOPASS :: any_key_text_fn => NULL()
    PROCEDURE(key_text_callback_c), POINTER, NOPASS :: c_any_key_text_fn &
        => NULL()
    PROCEDURE(key_value_callback), POINTER, NOPASS :: any_key_value_fn => NULL()
    PROCEDURE(key_value_callback_c), POINTER, NOPASS :: c_any_key_value_fn &
        => NULL()
    PROCEDURE(key_numeric_value_callback), POINTER, NOPASS :: &
        any_key_numeric_value_fn => NULL()
    PROCEDURE(key_numeric_value_callback_c), POINTER, NOPASS :: &
        c_any_key_numeric_value_fn => NULL()
    PROCEDURE(key_stack_callback), POINTER, NOPASS :: any_key_stack_fn => NULL()
    PROCEDURE(key_stack_callback_c), POINTER, NOPASS :: c_any_key_stack_fn &
        => NULL()

    CONTAINS

    PROCEDURE, PRIVATE :: get_parents => dbd_get_parents

    PROCEDURE :: init => dbd_init
    PROCEDURE, PRIVATE :: add_new_block => dbd_add_new_block
    PROCEDURE, PRIVATE :: add_old_block => dbd_add_old_block
    PROCEDURE :: get_child => dbd_get_block
    PROCEDURE :: get_parent => dbd_get_parent
    PROCEDURE :: initialise_block => dbd_initialise_block
    PROCEDURE :: initialize_block => dbd_initialise_block
    PROCEDURE :: start_block => dbd_start_block
    PROCEDURE :: end_block => dbd_end_block
    PROCEDURE :: finalise_block => dbd_finalise_block
    PROCEDURE :: finalize_block => dbd_finalise_block
    PROCEDURE :: end_pass_block => dbd_end_pass_block
    PROCEDURE :: get_id => dbd_get_id
    PROCEDURE :: optimise => dbd_optimise
    PROCEDURE :: visualise => dbd_visualise
    PROCEDURE :: markdown => dbd_markdown
    PROCEDURE :: get_description => dbd_get_description

    PROCEDURE :: add_key => dbd_add_key
    PROCEDURE, PRIVATE :: call_key_text => dbd_call_key_text
    GENERIC, PUBLIC :: call_key => call_key_text
    GENERIC :: add_block => add_new_block
  END TYPE eis_deck_block_definition


  TYPE :: eis_deck_definition
    LOGICAL :: is_init = .FALSE.
    LOGICAL :: finalise_absent = .FALSE.
    CLASS(eis_deck_definition_info), POINTER :: info => NULL()
    CLASS(eis_deck_block_definition), POINTER :: root_definition => NULL()

    CONTAINS
    PROCEDURE :: init => dd_init
    PROCEDURE :: reset => dd_reset
    PROCEDURE :: get_root => dd_get_root
    PROCEDURE :: get_block => dd_get_block
    PROCEDURE :: get_block_name => dd_get_block_name
    PROCEDURE :: initialize_blocks => dd_init_blocks
    PROCEDURE :: initialise_blocks => dd_init_blocks
    PROCEDURE :: finalize_blocks => dd_finalise_blocks
    PROCEDURE :: finalise_blocks => dd_finalise_blocks
    PROCEDURE :: end_pass => dd_end_pass
    PROCEDURE :: optimise => dd_optimise
    PROCEDURE :: optimize => dd_optimise
    PROCEDURE :: visualise => dd_visualise
    PROCEDURE :: get_block_structure => dd_visualise
    PROCEDURE :: get_block_structure_as_markdown => dd_markdown
    FINAL :: dd_destructor
  END TYPE eis_deck_definition

  PUBLIC :: eis_deck_definition, eis_deck_block_definition

  CONTAINS

  SUBROUTINE eis_call_generic_triplet(fn, fn_c, succ_fn, succ_fn_c, err_fn, &
      err_fn_c, name, pass_number, parent_kind, status, errcode, host_state)

    PROCEDURE(block_generic_callback), POINTER :: fn
    PROCEDURE(block_generic_callback_c), POINTER :: fn_c
    PROCEDURE(generic_event_callback), POINTER :: succ_fn, err_fn
    PROCEDURE(generic_event_callback_c), POINTER :: succ_fn_c, err_fn_c
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER(eis_bitmask), INTENT(INOUT), OPTIONAL :: host_state

    CHARACTER(LEN=1, KIND = C_CHAR), DIMENSION(:), ALLOCATABLE, TARGET :: c_name
    INTEGER(eis_status) :: this_status
    INTEGER(eis_error) :: this_err
    INTEGER(eis_bitmask) :: this_bitmask
    LOGICAL :: called_dual

    called_dual = .FALSE.
    this_bitmask = 0_eis_bitmask
    IF (PRESENT(host_state)) this_bitmask = host_state

    ALLOCATE(c_name(LEN(name)))
    CALL eis_f_c_string(name, LEN(name), c_name)
    IF (ASSOCIATED(fn)) THEN
      this_status = eis_status_none
      this_err = eis_err_none
      CALL fn(name, pass_number, parent_kind, this_status, this_bitmask, &
          this_err)
      called_dual = .TRUE.
      CALL eis_call_generic_dual(succ_fn, succ_fn_c, err_fn, err_fn_c, &
          name, c_name, pass_number, parent_kind, this_status, this_bitmask, &
          this_err)
      errcode = IOR(errcode, this_err)
      status = IOR(status, this_status)
    END IF

    IF (ASSOCIATED(fn_c)) THEN
      this_status = eis_status_none
      this_err = eis_err_none
      CALL fn_c(C_LOC(c_name), INT(pass_number, C_INT), &
          SIZE(parent_kind, KIND=C_INT), INT(parent_kind, C_INT), &
          this_status, this_bitmask, this_err)
      called_dual = .TRUE.
      CALL eis_call_generic_dual(succ_fn, succ_fn_c, err_fn, err_fn_c, &
          name, c_name, pass_number, parent_kind, this_status, this_bitmask, &
          this_err)
      errcode = IOR(errcode, this_err)
      status = IOR(status, this_status)
    END IF

    IF (.NOT. called_dual) THEN
      this_err = eis_err_none
      CALL eis_call_generic_dual(succ_fn, succ_fn_c, err_fn, err_fn_c, &
          name, c_name, pass_number, parent_kind, this_status, this_bitmask, &
          this_err)
    END IF
    IF (PRESENT(host_state)) host_state = IOR(host_state, this_bitmask)
    DEALLOCATE(c_name)

  END SUBROUTINE eis_call_generic_triplet



  SUBROUTINE eis_call_generic_dual(succ_fn, succ_fn_c, err_fn, err_fn_c, name, &
      c_name, pass_number, parent_kind, this_status, this_bitmask, errcode)

    PROCEDURE(generic_event_callback), POINTER :: succ_fn, err_fn
    PROCEDURE(generic_event_callback_c), POINTER :: succ_fn_c, err_fn_c
    CHARACTER(LEN=*), INTENT(IN) :: name
    CHARACTER(LEN=1, KIND = C_CHAR), DIMENSION(*), TARGET :: c_name
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: this_status
    INTEGER(eis_bitmask), INTENT(INOUT) :: this_bitmask
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    IF (errcode == eis_err_none) THEN
      IF (ASSOCIATED(succ_fn)) THEN
        CALL succ_fn(name, pass_number, parent_kind, this_status, &
            this_bitmask, errcode)
      END IF

      IF (ASSOCIATED(succ_fn_c)) THEN
        CALL succ_fn_c(C_LOC(c_name), INT(pass_number, C_INT), &
            SIZE(parent_kind, KIND = C_INT), INT(parent_kind, C_INT), &
            this_status, this_bitmask, errcode)
      END IF
    ELSE
      IF (ASSOCIATED(err_fn)) THEN
        CALL err_fn(name, pass_number, parent_kind, this_status, &
            this_bitmask, errcode)
      END IF

      IF (ASSOCIATED(err_fn_c)) THEN
        CALL err_fn_c(C_LOC(c_name), INT(pass_number, C_INT), &
            SIZE(parent_kind, KIND = C_INT), INT(parent_kind, C_INT), &
            this_status, this_bitmask, errcode)
      END IF
    END IF
  END SUBROUTINE eis_call_generic_dual



  SUBROUTINE eis_call_specific_triplet(fn, fn_c, succ_fn, succ_fn_c, err_fn, &
      err_fn_c, name, pass_number, parents, parent_kind, status, errcode, &
      host_state)

    PROCEDURE(block_callback), POINTER :: fn
    PROCEDURE(block_callback_c), POINTER :: fn_c
    PROCEDURE(event_callback), POINTER :: succ_fn, err_fn
    PROCEDURE(event_callback_c), POINTER :: succ_fn_c, err_fn_c
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER(eis_bitmask), INTENT(INOUT), OPTIONAL :: host_state

    CHARACTER(LEN=1, KIND = C_CHAR), DIMENSION(:), ALLOCATABLE, TARGET :: c_name
    INTEGER(eis_status) :: this_status
    INTEGER(eis_error) :: this_err
    INTEGER(eis_bitmask) :: this_bitmask
    LOGICAL :: called_dual

    called_dual = .FALSE.
    this_bitmask = 0_eis_bitmask
    IF (PRESENT(host_state)) this_bitmask = host_state

    ALLOCATE(c_name(LEN(name)))
    CALL eis_f_c_string(name, LEN(name), c_name)
    IF (ASSOCIATED(fn)) THEN
      this_status = eis_status_none
      this_err = eis_err_none
      CALL fn(name, pass_number, parents, parent_kind, this_status, &
          this_bitmask, this_err)
      called_dual = .TRUE.
      CALL eis_call_specific_dual(succ_fn, succ_fn_c, err_fn, err_fn_c, &
          name, c_name, pass_number, parents, parent_kind, this_status, &
          this_bitmask, this_err)
      errcode = IOR(errcode, this_err)
      status = IOR(status, this_status)
    END IF

    IF (ASSOCIATED(fn_c)) THEN
      this_status = eis_status_none
      this_err = eis_err_none
      CALL fn_c(C_LOC(c_name), INT(pass_number, C_INT), &
          SIZE(parent_kind, KIND=C_INT), INT(parents, C_INT), &
          INT(parent_kind, C_INT), this_status, this_bitmask, this_err)
      called_dual = .TRUE.
      CALL eis_call_specific_dual(succ_fn, succ_fn_c, err_fn, err_fn_c, &
          name, c_name, pass_number, parents, parent_kind, this_status, &
          this_bitmask, this_err)
      errcode = IOR(errcode, this_err)
      status = IOR(status, this_status)
    END IF

    IF (.NOT. called_dual) THEN
      this_err = eis_err_none
      CALL eis_call_specific_dual(succ_fn, succ_fn_c, err_fn, err_fn_c, &
          name, c_name, pass_number, parents, parent_kind, this_status, &
          this_bitmask, this_err)
    END IF
    IF (PRESENT(host_state)) host_state = IOR(host_state, this_bitmask)
    DEALLOCATE(c_name)

  END SUBROUTINE eis_call_specific_triplet



  SUBROUTINE eis_call_specific_dual(succ_fn, succ_fn_c, err_fn, err_fn_c, &
      name, c_name, pass_number, parents, parent_kind, this_status, &
      this_bitmask, errcode)

    PROCEDURE(event_callback), POINTER :: succ_fn, err_fn
    PROCEDURE(event_callback_c), POINTER :: succ_fn_c, err_fn_c
    CHARACTER(LEN=*), INTENT(IN) :: name
    CHARACTER(LEN=1, KIND = C_CHAR), DIMENSION(*), TARGET :: c_name
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents, parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: this_status
    INTEGER(eis_bitmask), INTENT(INOUT) :: this_bitmask
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    IF (errcode == eis_err_none) THEN
      IF (ASSOCIATED(succ_fn)) THEN
        CALL succ_fn(name, pass_number, parents, parent_kind, this_status, &
            this_bitmask, errcode)
      END IF

      IF (ASSOCIATED(succ_fn_c)) THEN
        CALL succ_fn_c(C_LOC(c_name), INT(pass_number, C_INT), &
            SIZE(parent_kind, KIND = C_INT), INT(parents, C_INT), &
            INT(parent_kind, C_INT), this_status, this_bitmask, errcode)
      END IF
    ELSE
      IF (ASSOCIATED(err_fn)) THEN
        CALL err_fn(name, pass_number, parents, parent_kind, this_status, &
            this_bitmask, errcode)
      END IF

      IF (ASSOCIATED(err_fn_c)) THEN
        CALL err_fn_c(C_LOC(c_name), INT(pass_number, C_INT), &
            SIZE(parent_kind, KIND = C_INT), INT(parents, C_INT), &
            INT(parent_kind, C_INT), this_status, this_bitmask, errcode)
      END IF
    END IF
  END SUBROUTINE eis_call_specific_dual



  FUNCTION ddi_get_next_id(this)
    CLASS(eis_deck_definition_info), INTENT(INOUT) :: this
    INTEGER :: ddi_get_next_id

    ddi_get_next_id = this%id_max + 1
    this%id_max = this%id_max + 1

  END FUNCTION ddi_get_next_id



  SUBROUTINE ddi_add_block(this, block_in)
    CLASS(eis_deck_definition_info), INTENT(INOUT) :: this
    CLASS(eis_deck_block_definition), POINTER, INTENT(IN) :: block_in
    CLASS(*), POINTER :: ptr

    !Needed in F2003 standard
    ptr => block_in

    CALL this%all_blocks%hold(block_in%id, ptr, owns = .TRUE.)

  END SUBROUTINE ddi_add_block



  FUNCTION ddi_get_block_count(this) RESULT(count)
    CLASS(eis_deck_definition_info), INTENT(IN) :: this
    INTEGER :: count

    count = this%id_max

  END FUNCTION ddi_get_block_count



  FUNCTION ddi_get_block(this, id) RESULT(bptr)
    CLASS(eis_deck_definition_info), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: id
    CLASS(eis_deck_block_definition), POINTER :: bptr
    CLASS(*), POINTER :: gptr

    gptr => this%all_blocks%get(id)
    IF (.NOT. ASSOCIATED(gptr)) THEN
      bptr => NULL()
      RETURN
    END IF

    SELECT TYPE(gptr)
      CLASS IS (eis_deck_block_definition)
        bptr => gptr
      CLASS DEFAULT
        bptr => NULL()
    END SELECT

  END FUNCTION ddi_get_block



  FUNCTION dd_init(this, init_deck, start_pass, start_deck, end_deck, &
      end_pass, final_deck, any_key_text, any_key_value, &
      any_key_numeric_value, any_key_stack, block_remapper, c_init_deck, &
      c_start_pass, c_start_deck, c_end_deck, c_end_pass, c_final_deck, &
      c_any_key_text, c_any_key_value, c_any_key_numeric_value, &
      c_any_key_stack, c_block_remapper, on_key_success, on_key_failure, &
      on_key_no_trigger, on_block_start, on_block_end, on_block_no_trigger, &
      on_block_failure, c_on_key_success, c_on_key_failure, &
      c_on_key_no_trigger, c_on_block_start, c_on_block_end, &
      c_on_block_no_trigger, c_on_block_failure, on_init, on_start_pass, &
      on_end_pass, on_final, on_generic_block_failure, c_on_init, &
      c_on_start_pass, c_on_end_pass, c_on_final, c_on_generic_block_failure, &
      parser, interop_parser, text_fallback, no_interop) RESULT(root)
    CLASS(eis_deck_definition), INTENT(INOUT) :: this
    PROCEDURE(block_generic_callback), OPTIONAL :: init_deck, final_deck
    PROCEDURE(block_generic_callback), OPTIONAL :: start_pass, end_pass
    PROCEDURE(block_callback), OPTIONAL :: start_deck, end_deck
    PROCEDURE(key_text_callback), OPTIONAL :: any_key_text
    PROCEDURE(key_value_callback), OPTIONAL :: any_key_value
    PROCEDURE(key_numeric_value_callback), OPTIONAL :: any_key_numeric_value
    PROCEDURE(key_stack_callback), OPTIONAL :: any_key_stack
    PROCEDURE(block_remap_callback), OPTIONAL :: block_remapper

    PROCEDURE(block_generic_callback_c), OPTIONAL :: c_init_deck, c_final_deck
    PROCEDURE(block_generic_callback_c), OPTIONAL :: c_start_pass, c_end_pass
    PROCEDURE(block_callback_c), OPTIONAL :: c_start_deck, c_end_deck
    PROCEDURE(key_text_callback_c), OPTIONAL :: c_any_key_text
    PROCEDURE(key_value_callback_c), OPTIONAL :: c_any_key_value
    PROCEDURE(key_numeric_value_callback_c), OPTIONAL :: c_any_key_numeric_value
    PROCEDURE(key_stack_callback_c), OPTIONAL :: c_any_key_stack
    PROCEDURE(block_remap_callback_c), OPTIONAL :: c_block_remapper

    PROCEDURE(event_callback), OPTIONAL :: on_key_success
    PROCEDURE(event_callback), OPTIONAL :: on_key_failure
    PROCEDURE(event_callback), OPTIONAL :: on_key_no_trigger
    PROCEDURE(event_callback), OPTIONAL :: on_block_start
    PROCEDURE(event_callback), OPTIONAL :: on_block_end
    PROCEDURE(event_callback), OPTIONAL :: on_block_no_trigger
    PROCEDURE(event_callback), OPTIONAL :: on_block_failure
    PROCEDURE(generic_event_callback), OPTIONAL :: on_init
    PROCEDURE(generic_event_callback), OPTIONAL :: on_start_pass
    PROCEDURE(generic_event_callback), OPTIONAL :: on_end_pass
    PROCEDURE(generic_event_callback), OPTIONAL :: on_final
    PROCEDURE(generic_event_callback), OPTIONAL :: on_generic_block_failure

    PROCEDURE(event_callback_c), OPTIONAL :: c_on_key_success
    PROCEDURE(event_callback_c), OPTIONAL :: c_on_key_failure
    PROCEDURE(event_callback_c), OPTIONAL :: c_on_key_no_trigger
    PROCEDURE(event_callback_c), OPTIONAL :: c_on_block_start
    PROCEDURE(event_callback_c), OPTIONAL :: c_on_block_end
    PROCEDURE(event_callback_c), OPTIONAL :: c_on_block_no_trigger
    PROCEDURE(event_callback_c), OPTIONAL :: c_on_block_failure
    PROCEDURE(generic_event_callback_c), OPTIONAL :: c_on_init
    PROCEDURE(generic_event_callback_c), OPTIONAL :: c_on_start_pass
    PROCEDURE(generic_event_callback_c), OPTIONAL :: c_on_end_pass
    PROCEDURE(generic_event_callback_c), OPTIONAL :: c_on_final
    PROCEDURE(generic_event_callback_c), OPTIONAL :: c_on_generic_block_failure

    CLASS(eis_parser), INTENT(IN), POINTER, OPTIONAL :: parser
    INTEGER, INTENT(IN), OPTIONAL :: interop_parser
    LOGICAL, INTENT(IN), OPTIONAL :: text_fallback
    LOGICAL, INTENT(IN), OPTIONAL :: no_interop

    CLASS(eis_deck_block_definition), POINTER :: root
    INTEGER :: dummy
    LOGICAL :: stop_interop

    root => this%root_definition
    IF (this%is_init) RETURN
    this%is_init = .TRUE.

    ALLOCATE(this%info)
    IF (PRESENT(on_key_success)) this%info%on_key_success_fn => on_key_success
    IF (PRESENT(on_key_failure)) this%info%on_key_failure_fn => on_key_failure
    IF (PRESENT(on_key_no_trigger)) this%info%on_key_no_trigger_fn &
        => on_key_no_trigger
    IF (PRESENT(on_block_start)) this%info%on_block_start_fn => on_block_start
    IF (PRESENT(on_block_end)) this%info%on_block_end_fn => on_block_end
    IF (PRESENT(on_block_no_trigger)) this%info%on_block_no_trigger_fn &
        => on_block_start
    IF (PRESENT(on_block_failure)) this%info%on_block_failure_fn &
        => on_block_failure

    IF (PRESENT(c_on_key_success)) this%info%c_on_key_success_fn &
        => c_on_key_success
    IF (PRESENT(c_on_key_failure)) this%info%c_on_key_failure_fn &
        => c_on_key_failure
    IF (PRESENT(c_on_key_no_trigger)) this%info%c_on_key_no_trigger_fn &
        => c_on_key_no_trigger
    IF (PRESENT(c_on_block_start)) this%info%c_on_block_start_fn &
        => c_on_block_start
    IF (PRESENT(c_on_block_end)) this%info%c_on_block_end_fn => c_on_block_end
    IF (PRESENT(c_on_block_no_trigger)) this%info%c_on_block_no_trigger_fn &
        => c_on_block_start
    IF (PRESENT(c_on_block_failure)) this%info%c_on_block_failure_fn &
        => c_on_block_failure

    IF (PRESENT(on_init)) this%info%on_generic_init_fn => on_init
    IF (PRESENT(on_start_pass)) this%info%on_generic_start_pass_fn &
        => on_start_pass
    IF (PRESENT(on_end_pass)) this%info%on_generic_end_pass_fn => on_end_pass
    IF (PRESENT(on_final)) this%info%on_generic_final_fn => on_final

    IF (PRESENT(c_on_init)) this%info%c_on_generic_init_fn => c_on_init
    IF (PRESENT(c_on_start_pass)) this%info%c_on_generic_start_pass_fn &
        => c_on_start_pass
    IF (PRESENT(c_on_end_pass)) this%info%c_on_generic_end_pass_fn &
        => c_on_end_pass
    IF (PRESENT(c_on_final)) this%info%c_on_generic_final_fn => c_on_final

    ALLOCATE(this%root_definition)
    dummy = this%root_definition%init(this%info, '{ROOT}', 0, -1, init_deck, &
        start_pass, start_deck, end_deck, end_pass, final_deck, any_key_text, &
        any_key_value, any_key_numeric_value, any_key_stack, block_remapper, &
        c_init_deck, c_start_pass, c_start_deck, c_end_deck, c_end_pass, &
        c_final_deck, c_any_key_text, c_any_key_value, &
        c_any_key_numeric_value, c_any_key_stack, c_block_remapper, &
        description = 'Root block holding all other blocks', &
        text_fallback = text_fallback)
    CALL this%info%add_block(this%root_definition)
    root => this%root_definition

    stop_interop = .FALSE.
    IF (PRESENT(no_interop)) stop_interop = no_interop
    IF (PRESENT(parser)) THEN
      this%info%parser => parser
      IF (PRESENT(interop_parser)) THEN
        this%info%interop_parser = interop_parser
      ELSE
        IF (.NOT. stop_interop) THEN
          this%info%owns_interop = .TRUE.
          this%info%interop_parser = eis_add_interop_parser(this%info%parser, &
              owns = .FALSE.)
        END IF
      END IF
    ELSE
      IF (PRESENT(interop_parser)) THEN
        this%info%parser => eis_get_interop_parser(interop_parser)
      END IF
      IF (.NOT. ASSOCIATED(this%info%parser)) THEN
        this%info%owns_parser = .TRUE.
        this%info%owns_interop = .TRUE.
        ALLOCATE(this%info%parser)
        IF (.NOT. stop_interop) THEN
          this%info%interop_parser = eis_add_interop_parser(this%info%parser, &
              owns = .FALSE.)
        END IF
      END IF
    END IF

  END FUNCTION dd_init



  SUBROUTINE dd_reset(this)
    CLASS(eis_deck_definition), INTENT(INOUT) :: this
    INTEGER :: i
    TYPE(eis_deck_block_definition), POINTER :: block

    DO i = 1, this%info%get_block_count()
      block => this%info%get_block(i)
      block%lastinit = 0
    END DO

  END SUBROUTINE dd_reset



  FUNCTION dd_get_root(this) RESULT(root)
    CLASS(eis_deck_definition), INTENT(IN) :: this
    CLASS(eis_deck_block_definition), POINTER :: root

    root => this%root_definition

  END FUNCTION dd_get_root



  FUNCTION dd_get_block(this, id) RESULT(block)
    CLASS(eis_deck_definition), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: id
    CLASS(eis_deck_block_definition), POINTER :: block

    block => this%info%get_block(id)

  END FUNCTION dd_get_block



  SUBROUTINE dd_get_block_name(this, id, name, description, is_hidden)
    CLASS(eis_deck_definition), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: id
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: name
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT), OPTIONAL :: description
    LOGICAL, INTENT(OUT), OPTIONAL :: is_hidden
    CLASS(eis_deck_block_definition), POINTER :: block

    block => this%info%get_block(id)
    IF (ASSOCIATED(block)) ALLOCATE(name, SOURCE = block%name)
    IF (ASSOCIATED(block) .AND. PRESENT(description)) ALLOCATE(description, &
        SOURCE = block%description)
    IF (ASSOCIATED(block) .AND. PRESENT(is_hidden)) is_hidden = block%is_hidden

  END SUBROUTINE dd_get_block_name



  SUBROUTINE dd_init_blocks(this, host_state, errcode, pass_number)
    CLASS(eis_deck_definition), INTENT(INOUT) :: this
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER, INTENT(IN), OPTIONAL :: pass_number
    INTEGER :: nblocks, iblock
    CLASS(eis_deck_block_definition), POINTER :: p
    INTEGER :: npass

    npass = npass_global
    IF (PRESENT(pass_number)) npass = pass_number

    nblocks = this%info%get_block_count()
    DO iblock = 0, nblocks
      p => this%info%get_block(iblock)
      CALL p%initialise_block(npass, errcode, host_state)
    END DO

  END SUBROUTINE dd_init_blocks



  SUBROUTINE dd_finalise_blocks(this, host_state, errcode, pass_number, &
      last_pass)
    CLASS(eis_deck_definition), INTENT(INOUT) :: this
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER, INTENT(IN), OPTIONAL :: pass_number
    LOGICAL, INTENT(IN), OPTIONAL :: last_pass
    INTEGER :: nblocks, iblock
    CLASS(eis_deck_block_definition), POINTER :: p
    INTEGER :: npass
    LOGICAL :: lpass
    INTEGER(eis_status) :: status

    npass = npass_global
    IF (PRESENT(pass_number)) npass = pass_number

    lpass = .TRUE.
    IF (PRESENT(last_pass)) lpass = last_pass

    nblocks = this%info%get_block_count()
    DO iblock = 0, nblocks
      p => this%info%get_block(iblock)
      IF (p%lastinit /= 0 .OR. this%finalise_absent) THEN
        CALL p%finalise_block(npass, lpass, status, errcode, host_state)
      END IF
    END DO

  END SUBROUTINE dd_finalise_blocks


  SUBROUTINE dd_end_pass(this, host_state, errcode, pass_number)
    CLASS(eis_deck_definition), INTENT(INOUT) :: this
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER, INTENT(IN), OPTIONAL :: pass_number
    INTEGER :: nblocks, iblock
    CLASS(eis_deck_block_definition), POINTER :: p
    INTEGER :: npass

    npass = npass_global
    IF (PRESENT(pass_number)) npass = pass_number

    nblocks = this%info%get_block_count()
    DO iblock = 0, nblocks
      p => this%info%get_block(iblock)
      IF (p%lastinit /= 0) THEN
        CALL p%end_pass_block(npass, errcode, host_state)
      END IF
    END DO

  END SUBROUTINE dd_end_pass


  SUBROUTINE dd_optimise(this)
    CLASS(eis_deck_definition), INTENT(INOUT) :: this

    IF (ASSOCIATED(this%root_definition)) CALL this%root_definition%optimise()

  END SUBROUTINE dd_optimise



  SUBROUTINE dd_visualise(this, str)
    CLASS(eis_deck_definition), INTENT(INOUT) :: this
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: str
    INTEGER :: level

    IF (.NOT. ASSOCIATED(this%root_definition)) RETURN

    level = this%info%id_max + 1
    CALL eis_append_string(str,'strict graph G {')
    CALL this%root_definition%visualise(str, level)
    CALL eis_append_string(str,'}')

  END SUBROUTINE dd_visualise



  SUBROUTINE dd_markdown(this, str, title)
    CLASS(eis_deck_definition), INTENT(INOUT) :: this
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: str
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: title
    INTEGER :: level

    IF (.NOT. ASSOCIATED(this%root_definition)) RETURN

    IF (PRESENT(title)) CALL eis_append_string(str, '# ' // title)
    CALL eis_append_string(str,'')

    level = this%info%id_max + 1
    CALL this%root_definition%markdown(this, str, level)

  END SUBROUTINE dd_markdown



  SUBROUTINE dd_destructor(this)
    TYPE(eis_deck_definition), INTENT(INOUT) :: this
    IF (ASSOCIATED(this%info%parser) .AND. this%info%owns_parser) &
        DEALLOCATE(this%info%parser)
    IF (this%info%owns_interop) &
        CALL eis_release_interop_parser(this%info%interop_parser, gone = .TRUE.)
    IF (ASSOCIATED(this%info)) DEALLOCATE(this%info)
  END SUBROUTINE dd_destructor



  SUBROUTINE dbd_get_parents(this, parents)
    CLASS(eis_deck_block_definition), INTENT(IN) :: this
    INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: parents
    INTEGER :: ipar
    CLASS(eis_deck_block_definition), POINTER :: par

    ALLOCATE(parents(this%depth+1))
    par => this%get_parent()
    DO ipar = this%depth, 1, -1
      parents(ipar) = par%id
      par => par%get_parent()
    END DO
    parents(this%depth+1) = this%id

  END SUBROUTINE dbd_get_parents



  FUNCTION dbd_init(this, info, block_name, depth, parent, init_block, &
      start_pass, start_block, end_block, end_pass, final_block, &
      any_key_text, any_key_value, any_key_numeric_value, any_key_stack, &
      block_remapper, c_init_block, c_start_pass, c_start_block, &
      c_end_block, c_end_pass, c_final_block, c_any_key_text, &
      c_any_key_value, c_any_key_numeric_value, c_any_key_stack, &
      c_block_remapper, parent_block, pass_eq, pass_le, pass_ge, &
      init_flag, i32count, i64count, description, hidden, text_fallback)

    CLASS(eis_deck_block_definition), INTENT(INOUT) :: this
    CLASS(eis_deck_definition_info), POINTER, INTENT(IN) :: info
    CHARACTER(LEN=*), INTENT(IN) :: block_name
    INTEGER, INTENT(IN) :: depth
    INTEGER, INTENT(IN) :: parent
    PROCEDURE(block_generic_callback), OPTIONAL :: init_block, final_block
    PROCEDURE(block_generic_callback), OPTIONAL :: start_pass, end_pass
    PROCEDURE(block_callback), OPTIONAL :: start_block, end_block
    PROCEDURE(key_text_callback), OPTIONAL :: any_key_text
    PROCEDURE(key_value_callback), OPTIONAL :: any_key_value
    PROCEDURE(key_numeric_value_callback), OPTIONAL :: any_key_numeric_value
    PROCEDURE(key_stack_callback), OPTIONAL :: any_key_stack
    PROCEDURE(block_remap_callback), OPTIONAL :: block_remapper

    PROCEDURE(block_generic_callback_c), OPTIONAL :: c_init_block, c_final_block
    PROCEDURE(block_generic_callback_c), OPTIONAL :: c_start_pass, c_end_pass
    PROCEDURE(block_callback_c), OPTIONAL :: c_start_block, c_end_block
    PROCEDURE(key_text_callback_c), OPTIONAL :: c_any_key_text
    PROCEDURE(key_value_callback_c), OPTIONAL :: c_any_key_value
    PROCEDURE(key_numeric_value_callback_c), OPTIONAL :: c_any_key_numeric_value
    PROCEDURE(key_stack_callback_c), OPTIONAL :: c_any_key_stack
    PROCEDURE(block_remap_callback_c), OPTIONAL :: c_block_remapper
    INTEGER, INTENT(IN), OPTIONAL :: pass_eq
    INTEGER, INTENT(IN), OPTIONAL :: pass_le
    INTEGER, INTENT(IN), OPTIONAL :: pass_ge
#ifdef F2008
    LOGICAL, TARGET, OPTIONAL :: init_flag
    INTEGER(INT32), TARGET, OPTIONAL :: i32count
    INTEGER(INT64), TARGET, OPTIONAL :: i64count
#else
    LOGICAL, POINTER, OPTIONAL :: init_flag
    INTEGER(INT32), POINTER, OPTIONAL :: i32count
    INTEGER(INT64), POINTER, OPTIONAL :: i64count
#endif
    CLASS(eis_deck_block_definition), INTENT(IN), OPTIONAL :: parent_block
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    LOGICAL, INTENT(IN), OPTIONAL :: hidden
    LOGICAL, INTENT(IN), OPTIONAL :: text_fallback

    INTEGER :: dbd_init
    LOGICAL :: inherit

    IF (ALLOCATED(this%name)) DEALLOCATE(this%name)
    ALLOCATE(this%name, SOURCE = block_name)
    info%longest_block_name = MAX(info%longest_block_name, LEN(block_name))
    this%info => info
    this%id = info%get_id()
    this%depth = depth
    this%parent = parent
    IF (PRESENT(init_block)) this%init_block_fn => init_block
    IF (PRESENT(start_pass)) this%start_pass_block_fn => start_pass
    IF (PRESENT(start_block)) this%start_block_fn => start_block
    IF (PRESENT(end_block)) this%end_block_fn => end_block
    IF (PRESENT(end_pass)) this%end_pass_block_fn => end_pass
    IF (PRESENT(final_block)) this%final_block_fn => final_block
    IF (PRESENT(any_key_text)) this%any_key_text_fn => any_key_text
    IF (PRESENT(any_key_value)) this%any_key_value_fn => any_key_value
    IF (PRESENT(any_key_numeric_value)) this%any_key_numeric_value_fn &
        => any_key_numeric_value
    IF (PRESENT(any_key_stack)) this%any_key_stack_fn => any_key_stack
    IF (PRESENT(block_remapper)) this%block_remap_fn => block_remapper

    IF (PRESENT(c_init_block)) this%c_init_block_fn => c_init_block
    IF (PRESENT(c_start_pass)) this%c_start_pass_block_fn => c_start_pass
    IF (PRESENT(c_start_block)) this%c_start_block_fn => c_start_block
    IF (PRESENT(c_end_block)) this%c_end_block_fn => c_end_block
    IF (PRESENT(c_end_pass)) this%c_end_pass_block_fn => c_end_pass
    IF (PRESENT(c_final_block)) this%c_final_block_fn => c_final_block
    IF (PRESENT(c_any_key_text)) this%c_any_key_text_fn => c_any_key_text
    IF (PRESENT(c_any_key_value)) this%c_any_key_value_fn => c_any_key_value
    IF (PRESENT(c_any_key_numeric_value)) this%c_any_key_numeric_value_fn &
        => c_any_key_numeric_value
    IF (PRESENT(c_any_key_stack)) this%c_any_key_stack_fn => c_any_key_stack
    IF (PRESENT(c_block_remapper)) this%c_block_remap_fn => c_block_remapper
    IF (PRESENT(description)) ALLOCATE(this%description, SOURCE = description)
    IF (PRESENT(hidden)) this%is_hidden = hidden

    IF (PRESENT(init_flag)) this%set_variable => init_flag
    IF (PRESENT(i32count)) this%i32count_variable => i32count
    IF (PRESENT(i64count)) this%i64count_variable => i64count

    inherit = PRESENT(parent_block)
    IF (PRESENT(pass_eq)) THEN
      this%pass_eq = pass_eq
      this%use_eq = .TRUE.
      inherit = .FALSE.
    END IF
    IF (PRESENT(pass_le)) THEN
      this%pass_le = pass_le
      this%use_le = .TRUE.
      inherit = .FALSE.
    END IF
    IF (PRESENT(pass_ge)) THEN
      this%pass_ge = pass_ge
      this%use_ge = .TRUE.
      inherit = .FALSE.
    END IF

    IF (inherit) THEN
      this%pass_eq = parent_block%pass_eq
      this%use_eq = parent_block%use_eq
      this%pass_le = parent_block%pass_le
      this%use_le = parent_block%use_le
      this%pass_ge = parent_block%pass_ge
      this%pass_eq = parent_block%pass_eq
      this%use_eq = parent_block%use_eq
      this%pass_le = parent_block%pass_le
    END IF

    IF (PRESENT(text_fallback)) THEN
      this%text_fallback = text_fallback
    ELSE
      IF (PRESENT(parent_block)) this%text_fallback = parent_block%text_fallback
    END IF

    dbd_init = this%id

  END FUNCTION dbd_init



  FUNCTION dbd_add_new_block(this, block_name, init_block, start_pass, &
      start_block, end_block, end_pass, final_block, any_key_text, &
      any_key_value, any_key_numeric_value, any_key_stack, block_remapper, &
      c_init_block, c_start_pass, c_start_block, c_end_block, c_end_pass, &
      c_final_block, c_any_key_text, c_any_key_value, c_any_key_numeric_value, &
      c_any_key_stack, c_block_remapper, pass_eq, pass_le, pass_ge, init_flag, &
      i32count, i64count, description, hidden, text_fallback)
    CLASS(eis_deck_block_definition), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: block_name

    PROCEDURE(block_generic_callback), OPTIONAL :: init_block, final_block
    PROCEDURE(block_generic_callback), OPTIONAL :: start_pass, end_pass
    PROCEDURE(block_callback), OPTIONAL :: start_block, end_block
    PROCEDURE(key_text_callback), OPTIONAL :: any_key_text
    PROCEDURE(key_value_callback), OPTIONAL :: any_key_value
    PROCEDURE(key_numeric_value_callback), OPTIONAL :: any_key_numeric_value
    PROCEDURE(key_stack_callback), OPTIONAL :: any_key_stack
    PROCEDURE(block_remap_callback), OPTIONAL :: block_remapper

    PROCEDURE(block_generic_callback_c), OPTIONAL :: c_init_block, c_final_block
    PROCEDURE(block_generic_callback_c), OPTIONAL :: c_start_pass, c_end_pass
    PROCEDURE(block_callback_c), OPTIONAL :: c_start_block, c_end_block
    PROCEDURE(key_text_callback_c), OPTIONAL :: c_any_key_text
    PROCEDURE(key_value_callback_c), OPTIONAL :: c_any_key_value
    PROCEDURE(key_numeric_value_callback_c), OPTIONAL :: c_any_key_numeric_value
    PROCEDURE(key_stack_callback_c), OPTIONAL :: c_any_key_stack
    PROCEDURE(block_remap_callback_c), OPTIONAL :: c_block_remapper
    INTEGER, INTENT(IN), OPTIONAL :: pass_eq
    INTEGER, INTENT(IN), OPTIONAL :: pass_le
    INTEGER, INTENT(IN), OPTIONAL :: pass_ge
#ifdef F2008
    LOGICAL, TARGET, OPTIONAL :: init_flag
    INTEGER(INT32), TARGET, OPTIONAL :: i32count
    INTEGER(INT64), TARGET, OPTIONAL :: i64count
#else
    LOGICAL, POINTER, OPTIONAL :: init_flag
    INTEGER(INT32), POINTER, OPTIONAL :: i32count
    INTEGER(INT64), POINTER, OPTIONAL :: i64count
#endif
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    LOGICAL, INTENT(IN), OPTIONAL :: hidden
    LOGICAL, INTENT(IN), OPTIONAL :: text_fallback

    CLASS(eis_deck_block_definition), POINTER :: dbd_add_new_block
    CLASS(*), POINTER :: ptr
    INTEGER :: id

    ALLOCATE(dbd_add_new_block)
    id = &
        dbd_add_new_block%init(this%info, block_name, this%depth + 1, this%id, &
        init_block, start_pass, start_block, end_block, end_pass, final_block, &
        any_key_text, any_key_value, any_key_numeric_value, any_key_stack, &
        block_remapper, c_init_block, c_start_pass, c_start_block, &
        c_end_block, c_end_pass, c_final_block, c_any_key_text, &
        c_any_key_value, c_any_key_numeric_value, c_any_key_stack, &
        c_block_remapper, parent_block = this, pass_eq = pass_eq, &
        pass_le = pass_le, pass_ge = pass_ge, description = description, &
        hidden = hidden, init_flag = init_flag, i32count = i32count, &
        i64count = i64count, text_fallback = text_fallback)

    ptr => dbd_add_new_block
    CALL this%sub_blocks%hold(block_name, ptr, owns = .FALSE.)
    CALL this%info%add_block(dbd_add_new_block)
    
  END FUNCTION dbd_add_new_block



  FUNCTION dbd_add_old_block(this, old_block)
    CLASS(eis_deck_block_definition), INTENT(INOUT) :: this
    CLASS(eis_deck_block_definition), POINTER, INTENT(INOUT) :: old_block
    CLASS(eis_deck_block_definition), POINTER :: dbd_add_old_block
    CLASS(*), POINTER :: ptr
    INTEGER :: id

    ALLOCATE(dbd_add_old_block, SOURCE = old_block)
    id = dbd_add_old_block%init(this%info, old_block%name, this%depth + 1, &
        this%id)
    ptr => dbd_add_old_block
    CALL this%sub_blocks%hold(old_block%name, ptr, owns = .FALSE.)
    CALL this%info%add_block(dbd_add_old_block)
    
  END FUNCTION dbd_add_old_block



  FUNCTION dbd_get_block(this, block_name, status_code, host_state, errcode, &
      pass_number)
    CLASS(eis_deck_block_definition), INTENT(IN) :: this
    CHARACTER(LEN=*), INTENT(IN) :: block_name
    INTEGER(eis_status), INTENT(INOUT), OPTIONAL :: status_code
    INTEGER(eis_bitmask), INTENT(INOUT), OPTIONAL :: host_state
    INTEGER(eis_error), INTENT(INOUT), OPTIONAL :: errcode
    INTEGER, INTENT(IN), OPTIONAL :: pass_number
    CLASS(eis_deck_block_definition), POINTER :: dbd_get_block
    CLASS(*), POINTER :: ptr
    CHARACTER(LEN=:), ALLOCATABLE :: remap_name
    CHARACTER(LEN=1), DIMENSION(:), ALLOCATABLE, TARGET :: c_block_name, &
        c_remap_name
    TYPE(C_PTR) :: c_block_ptr, c_remap_ptr
    INTEGER(eis_status) :: outer_status, this_status
    INTEGER(eis_bitmask) :: outer_state, this_state
    INTEGER(eis_error) :: outer_errcode, this_errcode
    INTEGER :: npass
    INTEGER, DIMENSION(0) :: dummy

    ptr => this%sub_blocks%get(block_name)

    npass = npass_global
    IF (PRESENT(pass_number)) npass = pass_number
    outer_status = eis_status_none
    IF (PRESENT(status_code)) outer_status = status_code
    outer_state = 0_eis_bitmask
    IF (PRESENT(host_state)) outer_state = host_state
    outer_errcode = eis_err_none
    IF (PRESENT(errcode)) outer_errcode = errcode

    !Try to remap using the Fortran remapper function first
    IF (.NOT. ASSOCIATED(ptr) .AND. ASSOCIATED(this%block_remap_fn)) THEN
      ALLOCATE(CHARACTER(LEN=this%info%longest_block_name)::remap_name)
      this_status = eis_status_none
      this_state = 0_eis_bitmask
      this_errcode = eis_err_none
      CALL this%block_remap_fn(block_name, npass, remap_name, this_status, &
          this_state, this_errcode)
      outer_status = IOR(outer_status, this_status)
      outer_state = IOR(outer_state, this_state)
      outer_errcode = IOR(outer_errcode, this_errcode)
      IF (IAND(this_status, eis_status_not_handled)/=0) THEN
        outer_errcode = IOR(outer_errcode, eis_err_unknown_block)
      END IF
      IF (this_errcode == eis_err_none) THEN
        ptr => this%sub_blocks%get(TRIM(ADJUSTL(remap_name)))
      END IF
      DEALLOCATE(remap_name)
    END IF

    !If that fails, try the C function next
    IF (.NOT. ASSOCIATED(ptr) .AND. ASSOCIATED(this%c_block_remap_fn)) THEN 
      ALLOCATE(CHARACTER(LEN=1, KIND=C_CHAR)::c_block_name(LEN(block_name)))
      ALLOCATE(CHARACTER(LEN=1, KIND=C_CHAR):: &
          c_remap_name(this%info%longest_block_name))
      CALL eis_f_c_string(block_name, LEN(block_name), c_block_name)
      c_block_ptr = C_LOC(c_block_name)
      c_remap_ptr = C_LOC(c_remap_name)
      this_status = eis_status_none
      this_state = 0
      this_errcode = eis_err_none
      CALL this%c_block_remap_fn(c_block_ptr, INT(npass, C_INT), &
          INT(this%info%longest_block_name, C_INT), c_remap_ptr, &
          this_status, this_state, this_errcode)
      CALL eis_c_f_string(c_remap_ptr, remap_name)
      DEALLOCATE(c_remap_name, c_block_name)
      outer_status = IOR(outer_status, this_status)
      outer_state = IOR(outer_state, this_state)
      outer_errcode = IOR(outer_errcode, this_errcode)
      IF (IAND(this_status, eis_status_not_handled)/=0) THEN
        outer_errcode = IOR(outer_errcode, eis_err_unknown_block)
      END IF
      IF (this_errcode == eis_err_none) THEN
        ptr => this%sub_blocks%get(TRIM(ADJUSTL(remap_name)))
      END IF
      DEALLOCATE(remap_name)
    END IF

    dbd_get_block => NULL()
    IF (ASSOCIATED(ptr)) THEN
      SELECT TYPE(ptr)
        CLASS IS (eis_deck_block_definition)
          dbd_get_block => ptr
        CLASS DEFAULT
          outer_errcode = IOR(outer_errcode, eis_err_unknown_block)
      END SELECT
    ELSE
      outer_errcode = IOR(outer_errcode, eis_err_unknown_block)
    END IF

    IF (outer_errcode /= eis_err_none) THEN
      IF (ASSOCIATED(this%info%on_block_failure_fn)) THEN
        CALL this%info%on_block_failure_fn(block_name, npass, dummy, &
            dummy, outer_status, outer_state, outer_errcode)
      END IF

      IF (ASSOCIATED(this%info%c_on_block_failure_fn)) THEN
        ALLOCATE(CHARACTER(LEN=1, KIND=C_CHAR)::c_block_name(LEN(block_name)))
        CALL eis_f_c_string(block_name, LEN(block_name), c_block_name)
        CALL this%info%c_on_block_failure_fn(C_LOC(c_block_name), &
            INT(npass, C_INT), SIZE(dummy, KIND=C_INT), &
            INT(dummy, C_INT), INT(dummy, C_INT), outer_status, &
            outer_state, outer_errcode)
        DEALLOCATE(c_block_name)
      END IF
    END IF

    IF (PRESENT(status_code)) status_code = outer_status
    IF (PRESENT(host_state)) host_state = outer_state
    IF (PRESENT(errcode)) errcode = outer_errcode

  END FUNCTION dbd_get_block



  FUNCTION dbd_get_parent(this)
    CLASS(eis_deck_block_definition), INTENT(IN) :: this
    CLASS(eis_deck_block_definition), POINTER :: dbd_get_parent

    dbd_get_parent => this%info%get_block(this%parent)

  END FUNCTION dbd_get_parent



  SUBROUTINE dbd_initialise_block(this, pass_number, status, errcode, &
      host_state)
    CLASS(eis_deck_block_definition), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: pass_number
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER(eis_bitmask), INTENT(INOUT), OPTIONAL :: host_state
    INTEGER, DIMENSION(:), ALLOCATABLE :: parent_kind
    INTEGER(eis_bitmask) :: this_bitmask
    LOGICAL :: run

    CALL this%get_parents(parent_kind)

    IF (PRESENT(host_state)) THEN
      this_bitmask = host_state
    ELSE
      this_bitmask = 0_eis_bitmask
    END IF

    run = .NOT. ANY([this%use_eq, this%use_le, this%use_ge])
    IF (this%use_eq) run = run .OR. (pass_number == this%pass_eq)
    IF (this%use_le) run = run .OR. (pass_number <= this%pass_le)
    IF (this%use_ge) run = run .OR. (pass_number >= this%pass_ge)

    IF (.NOT. run) RETURN

    IF (this%lastinit /= pass_number) THEN
      IF (this%lastinit == 0) THEN
        CALL eis_call_generic_triplet(this%init_block_fn, &
            this%c_init_block_fn, this%info%on_generic_init_fn, &
            this%info%c_on_generic_init_fn, &
            this%info%on_generic_failure_fn, &
            this%info%c_on_generic_failure_fn, &
            this%name, pass_number, parent_kind, status, errcode, host_state)
      END IF
      CALL eis_call_generic_triplet(this%start_pass_block_fn, &
          this%c_start_pass_block_fn, this%info%on_generic_start_pass_fn, &
          this%info%c_on_generic_start_pass_fn, &
          this%info%on_generic_failure_fn, &
          this%info%c_on_generic_failure_fn, &
          this%name, pass_number, parent_kind, status, errcode, host_state)
       this%lastinit = pass_number
    END IF

    DEALLOCATE(parent_kind)

  END SUBROUTINE dbd_initialise_block



  SUBROUTINE dbd_start_block(this, parents, pass_number, status, errcode, &
      host_state, display_name)
    CLASS(eis_deck_block_definition), INTENT(INOUT) :: this
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, INTENT(IN) :: pass_number
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER(eis_bitmask), INTENT(INOUT), OPTIONAL :: host_state
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: display_name
    INTEGER, DIMENSION(:), ALLOCATABLE :: parent_kind
    INTEGER(eis_status) :: this_status
    INTEGER(eis_error) :: this_err
    INTEGER(eis_bitmask) :: this_bitmask
    CHARACTER(LEN=:), ALLOCATABLE :: this_name
    CHARACTER(LEN=1), DIMENSION(:), ALLOCATABLE, TARGET :: c_this_name
    LOGICAL :: run

    CALL this%get_parents(parent_kind)
    this_status = eis_status_none

    run = .NOT. ANY([this%use_eq, this%use_le, this%use_ge])
    IF (this%use_eq) run = run .OR. (pass_number == this%pass_eq)
    IF (this%use_le) run = run .OR. (pass_number <= this%pass_le)
    IF (this%use_ge) run = run .OR. (pass_number >= this%pass_ge)
    IF (.NOT. run) THEN
      IF (PRESENT(display_name)) THEN
        ALLOCATE(this_name, SOURCE = display_name)
      ELSE
        ALLOCATE(this_name, SOURCE = this%name)
      END IF
      IF (ASSOCIATED(this%info%on_block_no_trigger_fn)) THEN
        CALL this%info%on_block_no_trigger_fn(this_name, pass_number, parents, &
            parent_kind, this_status, this_bitmask, this_err)
        errcode = IOR(errcode, this_err)
        status = IOR(status, this_status)
        IF (PRESENT(host_state)) host_state = IOR(host_state, this_bitmask)
      END IF

      IF (ASSOCIATED(this%info%c_on_block_no_trigger_fn)) THEN
        ALLOCATE(CHARACTER(LEN=1, KIND=C_CHAR)::c_this_name(LEN(this_name)))
        CALL eis_f_c_string(this_name, LEN(this_name), c_this_name)
        CALL this%info%c_on_block_no_trigger_fn(C_LOC(c_this_name), &
            INT(pass_number, C_INT), SIZE(parents, KIND=C_INT), &
            INT(parents, C_INT), INT(parent_kind, C_INT), this_status, &
            this_bitmask, this_err)
        errcode = IOR(errcode, this_err)
        status = IOR(status, this_status)
        IF (PRESENT(host_state)) host_state = IOR(host_state, this_bitmask)
        DEALLOCATE(c_this_name)
      END IF
      DEALLOCATE(this_name)
      DEALLOCATE(parent_kind)
      RETURN
    END IF

    IF (PRESENT(host_state)) THEN
      this_bitmask = host_state
    ELSE
      this_bitmask = 0_eis_bitmask
    END IF
    this_err = eis_err_none
    !If you are starting a block you may also be initialising the block
    !so try this
    CALL this%initialise_block(pass_number, this_status, this_err, &
        this_bitmask)
    IF (PRESENT(host_state)) host_state = IOR(host_state, this_bitmask)
    status = IOR(status, this_status)
    errcode = IOR(errcode, this_err)

    IF (PRESENT(display_name)) THEN
      ALLOCATE(this_name, SOURCE = display_name)
    ELSE
      ALLOCATE(this_name, SOURCE = this%name)
    END IF

    IF (ASSOCIATED(this%set_variable)) this%set_variable = .TRUE.
    IF (ASSOCIATED(this%i32count_variable)) this%i32count_variable &
        = this%i32count_variable + 1_INT32
    IF (ASSOCIATED(this%i64count_variable)) this%i64count_variable &
        = this%i64count_variable + 1_INT64

    CALL eis_call_specific_triplet(this%start_block_fn, &
        this%c_start_block_fn, this%info%on_block_start_fn, &
        this%info%c_on_block_start_fn, &
        this%info%on_block_failure_fn, &
        this%info%c_on_block_failure_fn, this_name, pass_number, parents, &
        parent_kind, status, errcode, host_state)

    DEALLOCATE(parent_kind)
    DEALLOCATE(this_name)

  END SUBROUTINE dbd_start_block



  SUBROUTINE dbd_end_block(this, parents, pass_number, status, errcode, &
      host_state, display_name)
    CLASS(eis_deck_block_definition), INTENT(IN) :: this
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, INTENT(IN) :: pass_number
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER(eis_bitmask), INTENT(INOUT), OPTIONAL :: host_state
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: display_name
    INTEGER, DIMENSION(:), ALLOCATABLE :: parent_kind
    CHARACTER(LEN=:), ALLOCATABLE :: this_name
    LOGICAL :: run

    run = .NOT. ANY([this%use_eq, this%use_le, this%use_ge])
    IF (this%use_eq) run = run .OR. (pass_number == this%pass_eq)
    IF (this%use_le) run = run .OR. (pass_number <= this%pass_le)
    IF (this%use_ge) run = run .OR. (pass_number >= this%pass_ge)
    !You only trigger block_no_trigger on block starts, not block ends
    IF (.NOT. run) RETURN

    CALL this%get_parents(parent_kind)

    IF (PRESENT(display_name)) THEN
      ALLOCATE(this_name, SOURCE = display_name)
    ELSE
      ALLOCATE(this_name, SOURCE = this%name)
    END IF

    CALL eis_call_specific_triplet(this%end_block_fn, &
        this%c_end_block_fn, this%info%on_block_end_fn, &
        this%info%c_on_block_end_fn, &
        this%info%on_block_failure_fn, &
        this%info%c_on_block_failure_fn, this_name, pass_number, parents, &
        parent_kind, status, errcode, host_state)

    DEALLOCATE(parent_kind)
    DEALLOCATE(this_name)

  END SUBROUTINE dbd_end_block



  SUBROUTINE dbd_end_pass_block(this, pass_number, errcode, host_state)
    CLASS(eis_deck_block_definition), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: pass_number
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER(eis_bitmask), INTENT(INOUT), OPTIONAL :: host_state
    INTEGER, DIMENSION(:), ALLOCATABLE :: parent_kind
    INTEGER(eis_status) :: this_status
    LOGICAL :: run

    run = .NOT. ANY([this%use_eq, this%use_le, this%use_ge])
    IF (this%use_eq) run = run .OR. (pass_number == this%pass_eq)
    IF (this%use_le) run = run .OR. (pass_number <= this%pass_le)
    IF (this%use_ge) run = run .OR. (pass_number >= this%pass_ge)
    !You only trigger block_no_trigger on block starts, not block ends
    IF (.NOT. run) RETURN

    CALL this%get_parents(parent_kind)

    CALL eis_call_generic_triplet(this%end_pass_block_fn, &
        this%c_end_pass_block_fn, this%info%on_generic_end_pass_fn, &
        this%info%c_on_generic_end_pass_fn, &
        this%info%on_generic_failure_fn, &
        this%info%c_on_generic_failure_fn, &
        this%name, pass_number, parent_kind, this_status, errcode, host_state)

    DEALLOCATE(parent_kind)

  END SUBROUTINE dbd_end_pass_block


  SUBROUTINE dbd_finalise_block(this, pass_number, last_pass, status, errcode, &
      host_state)
    CLASS(eis_deck_block_definition), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: pass_number
    LOGICAL, INTENT(IN) :: last_pass
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER(eis_bitmask), INTENT(INOUT), OPTIONAL :: host_state
    INTEGER, DIMENSION(:), ALLOCATABLE :: parent_kind
    LOGICAL :: run
    INTEGER :: crit_pass

    run = last_pass
    crit_pass = -1
    !Finalise triggers on the last valid pass or the last pass if not
    !otherwise triggered
    IF (.NOT. run) THEN
      IF (this%use_eq) crit_pass = MAX(crit_pass, this%pass_eq)
      IF (this%use_le) crit_pass = MAX(crit_pass, this%pass_le)
      run = (pass_number == crit_pass)
    END IF

    IF (.NOT. run) RETURN

    CALL this%get_parents(parent_kind)

    CALL eis_call_generic_triplet(this%final_block_fn, &
        this%c_final_block_fn, this%info%on_generic_final_fn, &
        this%info%c_on_generic_final_fn, &
        this%info%on_generic_failure_fn, &
        this%info%c_on_generic_failure_fn, &
        this%name, pass_number, parent_kind, status, errcode, &
        host_state = host_state)

    DEALLOCATE(parent_kind)

    this%lastinit = 0

  END SUBROUTINE dbd_finalise_block



  FUNCTION dbd_get_id(this)
    CLASS(eis_deck_block_definition), INTENT(IN) :: this
    INTEGER :: dbd_get_id

    dbd_get_id = this%id

  END FUNCTION dbd_get_id



  SUBROUTINE dbd_optimise(this)
    CLASS(eis_deck_block_definition), INTENT(INOUT) :: this
    INTEGER :: i
    CLASS(*), POINTER :: gptr

    CALL this%keys%optimise()
    CALL this%sub_blocks%optimise()
    DO i = 1, this%sub_blocks%get_name_count()
      gptr => this%sub_blocks%get(i)
      SELECT TYPE (gptr)
        CLASS IS (eis_deck_block_definition)
          CALL gptr%optimise()
      END SELECT
    END DO

  END SUBROUTINE dbd_optimise



  SUBROUTINE dbd_visualise(this, str, key_id)
    CLASS(eis_deck_block_definition), INTENT(INOUT) :: this
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: str
    INTEGER, INTENT(INOUT) :: key_id
    INTEGER, DIMENSION(:), ALLOCATABLE :: parents
    CHARACTER(LEN=5) :: val1, val2
    INTEGER :: i
    CLASS(*), POINTER :: gptr

    CALL this%get_parents(parents)
    WRITE(val1,'(I5.5)') this%id
    CALL eis_append_string(str,  TRIM(val1) // '[label="' &
        // this%name // '"] [shape= diamond];')
    IF (SIZE(parents) > 1) THEN
      WRITE(val2,'(I5.5)') parents(SIZE(parents)-1)
      CALL eis_append_string(str, val2 // ' -- ' // val1 // ';')
    END IF

    DO i = 1, this%sub_blocks%get_name_count()
      gptr => this%sub_blocks%get(i)
      SELECT TYPE(gptr)
        CLASS IS (eis_deck_block_definition)
          CALL gptr%visualise(str, key_id)
      END SELECT
    END DO

    DO i = 1, this%keys%get_name_count()
      gptr => this%keys%get(i)
      SELECT TYPE(gptr)
        CLASS IS (deck_key_definition)
        WRITE(val2,'(I5.5)') key_id
        CALL eis_append_string(str,  TRIM(val2) // '[label="' &
            // gptr%name // '"] [shape= circle];')
        CALL eis_append_string(str, val1 // ' -- ' // val2 // ';') 
        key_id = key_id + 1
      END SELECT
    END DO

  END SUBROUTINE dbd_visualise



  SUBROUTINE dbd_markdown(this, definition, str, key_id)
    CLASS(eis_deck_block_definition), INTENT(INOUT) :: this
    CLASS(eis_deck_definition), INTENT(INOUT) :: definition
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: str
    INTEGER, INTENT(INOUT) :: key_id
    INTEGER, DIMENSION(:), ALLOCATABLE :: parents
    CHARACTER(LEN=:), ALLOCATABLE :: pname, composite_name, composite_pname
    INTEGER :: i
    CLASS(*), POINTER :: gptr

    CALL this%get_parents(parents)
    IF (SIZE(parents) > 1 .AND. .NOT. this%is_hidden) THEN
      CALL eis_append_string(str,'---')
      CALL eis_append_string(str,'')
      IF (SIZE(parents) > 2) THEN
        DO i = 2, SIZE(parents)
          CALL definition%get_block_name(parents(i), pname)
          CALL eis_append_string(composite_name,pname, newline = .FALSE.)
          IF (i == SIZE(parents) - 1) ALLOCATE(composite_pname, &
              SOURCE = composite_name)
          IF (i/=SIZE(parents)) CALL eis_append_string(composite_name, '.', &
              newline = .FALSE.)
        END DO
      ELSE
        ALLOCATE(composite_name, SOURCE = this%name)
      END IF
      CALL eis_append_string(str, '## ' // composite_name)
      CALL eis_append_string(str,'')
      IF (SIZE(parents) > 2) THEN
        CALL eis_append_string(str, 'Child of : ' // composite_pname)
        CALL eis_append_string(str, '')
      END IF
      IF (ALLOCATED(this%description)) THEN
        CALL eis_append_string(str, this%description)
        CALL eis_append_string(str, '')
      END IF
      IF (this%keys%get_name_count() > 0) THEN
        CALL eis_append_string(str, '### Keys')
        CALL eis_append_string(str, '')
        DO i = 1, this%keys%get_name_count()
          gptr => this%keys%get(i)
          SELECT TYPE(gptr)
            CLASS IS (deck_key_definition)
            IF (gptr%is_hidden) CYCLE
            IF (ALLOCATED(gptr%description)) THEN
              CALL eis_append_string(str, '* `' // gptr%name // '` - ' &
                  // gptr%description)
            ELSE
              CALL eis_append_string(str, '* `' // gptr%name //'`')
            END IF
            CALL eis_append_string(str, '')
            key_id = key_id + 1
          END SELECT
        END DO
      END IF
      IF (this%sub_blocks%get_name_count() > 0) THEN
        CALL eis_append_string(str, '### Sub blocks')
        CALL eis_append_string(str, '')
        DO i = 1, this%sub_blocks%get_name_count()
          gptr => this%sub_blocks%get(i)
          SELECT TYPE(gptr)
            CLASS IS (eis_deck_block_definition)
              CALL eis_append_string(str, '* `' // composite_name //'.' &
                  // gptr%name // '`')
          END SELECT
        END DO
        CALL eis_append_string(str, '')
      END IF
      DEALLOCATE(composite_name)
      IF (ALLOCATED(composite_pname)) DEALLOCATE(composite_pname)
    END IF

    DO i = 1, this%sub_blocks%get_name_count()
      gptr => this%sub_blocks%get(i)
      SELECT TYPE(gptr)
        CLASS IS (eis_deck_block_definition)
          CALL gptr%markdown(definition, str, key_id)
      END SELECT
    END DO

  END SUBROUTINE dbd_markdown



  SUBROUTINE dbd_get_description(this, description)
    CLASS(eis_deck_block_definition), INTENT(INOUT) :: this
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: description

    IF (ALLOCATED(this%description)) &
        ALLOCATE(description, SOURCE = this%description)

  END SUBROUTINE dbd_get_description



  SUBROUTINE dbd_add_key(this, key_name, key_text_fn, key_value_fn, &
      should_key_trigger_fn,key_numeric_value_fn, key_stack_fn, c_key_text_fn, &
      c_key_value_fn, c_key_numeric_value_fn, c_key_stack_fn, &
      c_should_key_trigger_fn, i32value, i64value, r32value, &
      r64value, logicalvalue, i32array, i64array, r32array, r64array, &
      logicalarray, c_i32value, c_i64value, c_r32value, c_r64value, &
      c_i32len, c_i64len, c_r32len, c_r64len, init_flag, i32count, i64count, &
      expected_params, pass_eq, pass_le, pass_ge, description, hidden, &
      text_fallback)
    CLASS(eis_deck_block_definition), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: key_name
    PROCEDURE(key_text_callback), OPTIONAL :: key_text_fn
    PROCEDURE(key_value_callback), OPTIONAL :: key_value_fn
    PROCEDURE(key_numeric_value_callback), OPTIONAL :: key_numeric_value_fn
    PROCEDURE(key_stack_callback), OPTIONAL :: key_stack_fn
    PROCEDURE(should_key_trigger_callback), OPTIONAL :: should_key_trigger_fn
    PROCEDURE(key_text_callback_c), OPTIONAL :: c_key_text_fn
    PROCEDURE(key_value_callback_c), OPTIONAL :: c_key_value_fn
    PROCEDURE(key_numeric_value_callback_c), OPTIONAL :: c_key_numeric_value_fn
    PROCEDURE(key_stack_callback_c), OPTIONAL :: c_key_stack_fn
    PROCEDURE(should_key_trigger_callback_c), OPTIONAL :: &
        c_should_key_trigger_fn
#ifdef F2008
    INTEGER(INT32), TARGET, OPTIONAL :: i32value
    INTEGER(INT64), TARGET, OPTIONAL :: i64value
    REAL(REAL32), TARGET, OPTIONAL :: r32value
    REAL(REAL64), TARGET, OPTIONAL :: r64value
    LOGICAL, TARGET, OPTIONAL :: logicalvalue
    INTEGER(INT32), DIMENSION(:), TARGET, OPTIONAL :: i32array
    INTEGER(INT64), DIMENSION(:), TARGET, OPTIONAL :: i64array
    REAL(REAL32), DIMENSION(:), TARGET, OPTIONAL :: r32array
    REAL(REAL64), DIMENSION(:), TARGET, OPTIONAL :: r64array
    LOGICAL, DIMENSION(:), TARGET, OPTIONAL :: logicalarray

    LOGICAL, POINTER, OPTIONAL :: init_flag
    INTEGER(INT32), POINTER, OPTIONAL :: i32count
    INTEGER(INT64), POINTER, OPTIONAL :: i64count
#else
    INTEGER(INT32), POINTER, OPTIONAL :: i32value
    INTEGER(INT64), POINTER, OPTIONAL :: i64value
    REAL(REAL32), POINTER, OPTIONAL :: r32value
    REAL(REAL64), POINTER, OPTIONAL :: r64value
    LOGICAL, POINTER, OPTIONAL :: logicalvalue
    INTEGER(INT32), DIMENSION(:), POINTER, OPTIONAL :: i32array
    INTEGER(INT64), DIMENSION(:), POINTER, OPTIONAL :: i64array
    REAL(REAL32), DIMENSION(:), POINTER, OPTIONAL :: r32array
    REAL(REAL64), DIMENSION(:), POINTER, OPTIONAL :: r64array
    LOGICAL, DIMENSION(:), POINTER, OPTIONAL :: logicalarray

    LOGICAL, POINTER, OPTIONAL :: init_flag
    INTEGER(INT32), POINTER, OPTIONAL :: i32count
    INTEGER(INT64), POINTER, OPTIONAL :: i64count
#endif
    TYPE(C_PTR), INTENT(IN), OPTIONAL :: c_i32value
    TYPE(C_PTR), INTENT(IN), OPTIONAL :: c_i64value
    TYPE(C_PTR), INTENT(IN), OPTIONAL :: c_r32value
    TYPE(C_PTR), INTENT(IN), OPTIONAL :: c_r64value
    INTEGER, INTENT(IN), OPTIONAL :: c_i32len
    INTEGER, INTENT(IN), OPTIONAL :: c_i64len
    INTEGER, INTENT(IN), OPTIONAL :: c_r32len
    INTEGER, INTENT(IN), OPTIONAL :: c_r64len

    INTEGER, INTENT(IN), OPTIONAL :: expected_params
    INTEGER, INTENT(IN), OPTIONAL :: pass_eq
    INTEGER, INTENT(IN), OPTIONAL :: pass_le
    INTEGER, INTENT(IN), OPTIONAL :: pass_ge
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    LOGICAL, INTENT(IN), OPTIONAL :: hidden
    LOGICAL, INTENT(IN), OPTIONAL :: text_fallback
    TYPE(deck_key_definition), POINTER :: new
    CLASS(*), POINTER :: ptr

    ALLOCATE(new)
    CALL new%init(this, key_name, key_text_fn, key_value_fn, &
        key_numeric_value_fn, key_stack_fn, &
        should_key_trigger_fn = should_key_trigger_fn, &
        expected_params = expected_params, pass_eq = pass_eq, &
        pass_le = pass_le, pass_ge = pass_ge, c_key_text_fn = c_key_text_fn, &
        c_key_value_fn = c_key_value_fn, &
        c_key_numeric_value_fn = c_key_numeric_value_fn, &
        c_key_stack_fn = c_key_stack_fn, &
        c_should_key_trigger_fn = c_should_key_trigger_fn, &
        i32value = i32value, &
        i64value = i64value, r32value = r32value, r64value = r64value, &
        logicalvalue = logicalvalue, i32array = i32array, i64array = i64array, &
        r32array = r32array, r64array = r64array, logicalarray = logicalarray, &
        c_i32value = c_i32value, c_i64value = c_i64value, &
        c_r32value = c_r32value, c_r64value = c_r64value, c_i32len = c_i32len, &
        c_i64len = c_i64len, c_r32len = c_r32len, c_r64len = c_r64len, &
        init_flag = init_flag, i32count = i32count, i64count = i64count, &
        description = description, hidden = hidden, &
        text_fallback = text_fallback)

    ptr => new
    CALL this%keys%hold(key_name, ptr, owns = .TRUE.)

  END SUBROUTINE dbd_add_key



  SUBROUTINE dbd_call_key_text(this, key_text, parents, pass_number, status, &
      errcode, host_state, filename, line_number, white_space_length, &
      value_function, parser, interop_parser_id, non_value_line_is_blank, &
      err_handler)
    CLASS(eis_deck_block_definition), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: key_text
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, INTENT(IN) :: pass_number
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER(eis_bitmask), INTENT(INOUT), OPTIONAL :: host_state
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: filename
    INTEGER, INTENT(IN), OPTIONAL :: line_number
    INTEGER, INTENT(IN), OPTIONAL :: white_space_length
    PROCEDURE(parser_result_function), OPTIONAL :: value_function
    CLASS(eis_parser), INTENT(IN), POINTER, OPTIONAL :: parser
    INTEGER, INTENT(IN), OPTIONAL :: interop_parser_id
    LOGICAL, INTENT(IN), OPTIONAL :: non_value_line_is_blank
    CLASS(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler

    CLASS(*), POINTER :: ptr
    CLASS(deck_key_definition), POINTER :: dkd
    INTEGER, DIMENSION(:), ALLOCATABLE :: parent_kind
    INTEGER(eis_status) :: base_stat, this_stat
    INTEGER(eis_error) :: this_err, stack_err
    INTEGER(eis_bitmask) :: this_bitmask
    INTEGER :: eindex, cindex, sindex, ct, stack_id
    LOGICAL :: handled, is_key_value, is_directive, is_key_stack
    CHARACTER(LEN=:), ALLOCATABLE :: key, value
    TYPE(eis_stack), TARGET :: stack
    TYPE(eis_stack), POINTER :: stack_ptr
    REAL(eis_num), DIMENSION(:), ALLOCATABLE :: value_array
    CHARACTER(LEN=1), DIMENSION(:), ALLOCATABLE, TARGET :: c_key_text, c_key, &
        c_value
    TYPE(eis_parser), POINTER :: ps
    INTEGER :: interop_parser, wsl, char_offset
    LOGICAL :: run, any_candidates, isscalarvar, isarrayvar, non_value_blank
    INTEGER(INT32), DIMENSION(:), POINTER :: c_i32
    INTEGER(INT64), DIMENSION(:), POINTER :: c_i64
    REAL(REAL32), DIMENSION(:), POINTER :: c_r32
    REAL(REAL64), DIMENSION(:), POINTER :: c_r64
    INTEGER(eis_bitmask) :: cbits
    LOGICAL :: parser_error

    !Is this error triggered by the parser? If not then I should report it here
    parser_error = .FALSE.

    IF (PRESENT(parser)) THEN
      IF (ASSOCIATED(this%info%parser)) THEN
        ps => this%info%parser
      ELSE
        ps => parser
      END IF
    ELSE
      ps => this%info%parser
    END IF

    IF (PRESENT(interop_parser_id)) THEN
      IF (this%info%interop_parser > 0) THEN
        interop_parser = this%info%interop_parser
      ELSE
        interop_parser = interop_parser_id
      END IF
    ELSE
      interop_parser = this%info%interop_parser
    END IF

    IF (PRESENT(white_space_length)) THEN
      wsl = white_space_length
    ELSE
      wsl = 0
    END IF

    IF (PRESENT(non_value_line_is_blank)) THEN
      non_value_blank = non_value_line_is_blank
    ELSE
      non_value_blank = .FALSE.
    END IF

    ptr => NULL()
    dkd => NULL()

    this_err = eis_err_none
    this_stat = 0_eis_status

    IF (PRESENT(host_state)) THEN
      this_bitmask = host_state
    ELSE
      this_bitmask = 0_eis_bitmask
    END IF

    ALLOCATE(c_key_text(LEN(TRIM(ADJUSTL(key_text)))))
    CALL eis_f_c_string(TRIM(ADJUSTL(key_text)), LEN(TRIM(ADJUSTL(key_text))), &
        c_key_text)
    eindex = INDEX(key_text, '=')
    cindex = INDEX(key_text, ':')
    sindex = 0
    is_directive = .FALSE.
    IF (eindex > 0 .OR. cindex > 0) THEN
      IF (cindex == 0) THEN
        sindex = eindex
      ELSE IF (eindex == 0) THEN
        sindex = cindex
        is_directive = .TRUE.
      ELSE IF (eindex > cindex) THEN
        sindex = cindex
        is_directive = .TRUE.
      ELSE
        sindex = eindex
      END IF
    END IF

    char_offset = 0

    is_key_value = (sindex > 1 .AND. sindex < LEN(key_text))
    is_key_stack = is_key_value .OR. PRESENT(value_function)
    IF (is_key_value) THEN
      ALLOCATE(key, SOURCE = TRIM(ADJUSTL(key_text(1:sindex-1))))
      ALLOCATE(value, SOURCE = TRIM(ADJUSTL(key_text(sindex+1:))))
      char_offset = sindex + wsl + LEN(key_text(sindex+1:)) - LEN(value)
    ELSE
      ALLOCATE(key, SOURCE = TRIM(ADJUSTL(key_text)))
      ALLOCATE(value, SOURCE = "{Unknown value}")
      is_key_value = PRESENT(value_function)
    END IF
    IF (.NOT. is_key_value .AND. non_value_blank) RETURN

    ALLOCATE(c_key(LEN(key)))
    CALL eis_f_c_string(key, LEN(key), c_key)
    ALLOCATE(c_value(LEN(value)))
    CALL eis_f_c_string(value, LEN(value), c_value)

    IF (is_directive) THEN
      base_stat = eis_status_directive
    ELSE
      base_stat = eis_status_none
    END IF

    ptr => this%keys%get(key)
    SELECT TYPE(ptr)
      CLASS IS (deck_key_definition)
        dkd => ptr
      CLASS DEFAULT
        dkd => NULL()
    END SELECT

    CALL this%get_parents(parent_kind)
    handled = .FALSE.
    any_candidates = .FALSE.
    isscalarvar = .FALSE.
    isarrayvar = .FALSE.
    IF (ASSOCIATED(dkd)) THEN
      isscalarvar = ANY([ASSOCIATED(dkd%i32data), ASSOCIATED(dkd%i64data), &
          ASSOCIATED(dkd%r32data), ASSOCIATED(dkd%r64data), &
          ASSOCIATED(dkd%logicaldata), (C_ASSOCIATED(dkd%c_i32data) &
          .AND. dkd%c_i32len == 1), (C_ASSOCIATED(dkd%c_i64data) &
          .AND. dkd%c_i64len == 1), (C_ASSOCIATED(dkd%c_r32data) &
          .AND. dkd%c_r32len == 1), (C_ASSOCIATED(dkd%c_r64data) &
          .AND. dkd%c_r64len == 1)])
      isarrayvar = ANY([ASSOCIATED(dkd%i32arraydata), &
          ASSOCIATED(dkd%i64arraydata), ASSOCIATED(dkd%r32arraydata), &
          ASSOCIATED(dkd%r64arraydata), ASSOCIATED(dkd%logicalarraydata), &
           (C_ASSOCIATED(dkd%c_i32data) .AND. dkd%c_i32len > 1), &
           (C_ASSOCIATED(dkd%c_i64data) .AND. dkd%c_i64len > 1), &
           (C_ASSOCIATED(dkd%c_r32data) .AND. dkd%c_r32len > 1), &
           (C_ASSOCIATED(dkd%c_r64data) .AND. dkd%c_r64len > 1)])
      any_candidates = ANY([ASSOCIATED(dkd%key_text_fn), &
          ASSOCIATED(dkd%c_key_text_fn), &
          ASSOCIATED(dkd%key_value_fn), &
          ASSOCIATED(dkd%c_key_value_fn), &
          ASSOCIATED(dkd%key_numeric_value_fn), &
          ASSOCIATED(dkd%c_key_numeric_value_fn), &
          ASSOCIATED(dkd%key_stack_fn), &
          ASSOCIATED(dkd%c_key_stack_fn)])
       any_candidates = any_candidates .OR. isscalarvar .OR. isarrayvar
     ELSE
       any_candidates = .FALSE.
     END IF

    IF (any_candidates) THEN
      run = .NOT. ANY([dkd%use_eq, dkd%use_le, dkd%use_ge])
      IF (dkd%use_eq) run = run .OR. (pass_number == dkd%pass_eq)
      IF (dkd%use_le) run = run .OR. (pass_number <= dkd%pass_le)
      IF (dkd%use_ge) run = run .OR. (pass_number >= dkd%pass_ge)
      IF (ASSOCIATED(dkd%should_key_trigger_fn)) THEN
        run = run .AND. dkd%should_key_trigger_fn(key, pass_number, &
              parents, parent_kind, this_stat, this_bitmask, this_err)
        errcode = IOR(errcode, this_err)
        status = IOR(status, this_stat)
        IF (PRESENT(host_state)) host_state = IOR(host_state, this_bitmask)
      END IF
      IF (ASSOCIATED(dkd%c_should_key_trigger_fn)) THEN
        run = run .AND. (dkd%c_should_key_trigger_fn(C_LOC(c_key), &
            INT(pass_number, C_INT), SIZE(parents, KIND=C_INT), &
            INT(parents, C_INT), INT(parent_kind, C_INT), this_stat, &
            this_bitmask, this_err) /= 0_C_INT)
        errcode = IOR(errcode, this_err)
        status = IOR(status, this_stat)
        IF (PRESENT(host_state)) host_state = IOR(host_state, this_bitmask)
      END IF

      IF (.NOT. run) THEN
        IF (ASSOCIATED(this%info%on_key_no_trigger_fn)) THEN
          CALL this%info%on_key_no_trigger_fn(key, pass_number, parents, &
              parent_kind, this_stat, this_bitmask, this_err)
          errcode = IOR(errcode, this_err)
          status = IOR(status, this_stat)
          IF (PRESENT(host_state)) host_state = IOR(host_state, this_bitmask)
        END IF

        IF (ASSOCIATED(this%info%c_on_key_no_trigger_fn)) THEN
          CALL this%info%c_on_key_no_trigger_fn(C_LOC(c_key), &
              INT(pass_number, C_INT), SIZE(parents, KIND=C_INT), &
              INT(parents, C_INT), INT(parent_kind, C_INT), this_stat, &
              this_bitmask, this_err)
          errcode = IOR(errcode, this_err)
          status = IOR(status, this_stat)
          IF (PRESENT(host_state)) host_state = IOR(host_state, this_bitmask)
        END IF
        IF (errcode /= eis_err_none .AND. PRESENT(err_handler)) THEN
          CALL err_handler%add_error(eis_err_deck_parser, errcode, &
              filename = filename, line_number = line_number)
        END IF
        RETURN
      END IF

      IF (ASSOCIATED(dkd%set_variable)) dkd%set_variable = .TRUE.
      IF (ASSOCIATED(dkd%i32count_variable)) dkd%i32count_variable &
          = dkd%i32count_variable + 1_INT32
      IF (ASSOCIATED(dkd%i64count_variable)) dkd%i64count_variable &
          = dkd%i64count_variable + 1_INT64

      IF (.NOT. dkd%text_fallback) THEN
        IF (ASSOCIATED(dkd%key_text_fn)) THEN
          this_err = eis_err_none
          this_stat = base_stat
          CALL dkd%key_text_fn(TRIM(key_text), pass_number, parents, &
              parent_kind, this_stat, this_bitmask, this_err)
          !If the block is flagged handled then you care about the error code
          !value
          IF (IAND(this_stat, eis_status_not_handled) == 0) THEN
            errcode = IOR(errcode, this_err)
            status = IOR(status, this_stat)
          END IF
          handled = handled .OR. (IAND(this_stat, eis_status_not_handled) == 0)
        END IF
        IF (ASSOCIATED(dkd%c_key_text_fn) .AND. .NOT. handled) THEN
          this_err = eis_err_none
          this_stat = base_stat
          CALL dkd%c_key_text_fn(C_LOC(c_key_text), INT(pass_number, C_INT), &
              SIZE(parents, KIND=C_INT), INT(parents, C_INT), &
              INT(parent_kind, C_INT), this_stat, this_bitmask, this_err)
          !If the block is flagged handled then you care about the error code
          !value
          IF (IAND(this_stat, eis_status_not_handled) == 0) THEN
            errcode = IOR(errcode, this_err)
            status = IOR(status, this_stat)
          END IF
          handled = handled .OR. (IAND(this_stat, eis_status_not_handled) == 0)
        END IF
        IF (ASSOCIATED(dkd%key_value_fn) .AND. is_key_value .AND. &
            .NOT. handled) THEN
          this_err = eis_err_none
          this_stat = base_stat
          CALL dkd%key_value_fn(key, value, pass_number, parents, parent_kind, &
              this_stat, this_bitmask, this_err)
          !If the block is flagged handled then you care about the error code
          !value
          IF (IAND(this_stat, eis_status_not_handled) == 0) THEN
            errcode = IOR(errcode, this_err)
            status = IOR(status, this_stat)
          END IF
          handled = handled .OR. (IAND(this_stat, eis_status_not_handled) == 0)
        END IF
        IF (ASSOCIATED(dkd%c_key_value_fn) .AND. is_key_value .AND. &
            .NOT. handled) THEN
          this_err = eis_err_none
          this_stat = base_stat
          CALL dkd%c_key_value_fn(C_LOC(c_key), C_LOC(c_value), &
              INT(pass_number, C_INT), SIZE(parents, KIND=C_INT), &
              INT(parents, C_INT), INT(parent_kind, C_INT), this_stat, &
              this_bitmask, this_err)
          !If the block is flagged handled then you care about the error code
          !value
          IF (IAND(this_stat, eis_status_not_handled) == 0) THEN
            errcode = IOR(errcode, this_err)
            status = IOR(status, this_stat)
          END IF
          handled = handled .OR. (IAND(this_stat, eis_status_not_handled) == 0)
        END IF
      END IF

      IF (ASSOCIATED(dkd%key_numeric_value_fn) .AND. &
          ASSOCIATED(ps) .AND. is_key_stack &
          .AND. .NOT. handled) THEN
        this_err = eis_err_none
        this_stat = base_stat
        IF (.NOT. PRESENT(value_function)) THEN
          IF (is_key_value) THEN
            ct = ps%evaluate(value, value_array, this_err, &
                filename = filename, line_number = line_number, &
                char_offset = char_offset, cap_bits = cbits)
            parser_error = this_err /= eis_err_none
          ELSE
            this_err = eis_err_bad_value
          END IF
        ELSE
          CALL ps%set_result_function(value_function, stack, &
              this_err)
          cbits = 0_eis_bitmask
          ct = ps%evaluate(stack, value_array, this_err)
        END IF
        IF (this_err == eis_err_none) THEN
          IF (ct == dkd%expected_params .OR. dkd%expected_params < 1) THEN
            CALL dkd%key_numeric_value_fn(key, value_array, pass_number, &
                cbits, ps, parents, parent_kind, this_stat, this_bitmask, &
                this_err)
            parser_error = this_err /= eis_err_none
          ELSE
            this_err = eis_err_bad_value
          END IF
          IF (ALLOCATED(value_array)) DEALLOCATE(value_array)
        END IF
        !If the block is flagged handled then you care about the error code
        !value
        IF (IAND(this_stat, eis_status_not_handled) == 0) THEN
          errcode = IOR(errcode, this_err)
          status = IOR(status, this_stat)
        END IF
        handled = handled .OR. (IAND(this_stat, eis_status_not_handled) == 0)
      END IF
      IF (ASSOCIATED(dkd%c_key_numeric_value_fn) .AND. &
          ASSOCIATED(ps) .AND. is_key_stack &
          .AND. .NOT. handled) THEN
        this_err = eis_err_none
        this_stat = base_stat
        IF (.NOT. PRESENT(value_function)) THEN
          IF (is_key_value) THEN
            ct = ps%evaluate(value, value_array, this_err, &
                filename = filename, line_number = line_number, &
                char_offset = char_offset, cap_bits = cbits)
            parser_error = this_err /= eis_err_none
          ELSE
            errcode = eis_err_bad_value
          END IF
        ELSE
          CALL ps%set_result_function(value_function, stack, &
              this_err)
          cbits = 0_eis_bitmask
          ct = ps%evaluate(stack, value_array, this_err)
        END IF
        IF (this_err == eis_err_none) THEN
          CALL dkd%c_key_numeric_value_fn(C_LOC(c_key), &
              SIZE(value_array, KIND=C_INT), value_array, &
              INT(pass_number, C_INT), &
              INT(cbits, eis_bitmask_c),&
              interop_parser, SIZE(parents, KIND=C_INT), &
              INT(parents, C_INT), INT(parent_kind, C_INT), this_stat, &
              this_bitmask, this_err)
          IF (ALLOCATED(value_array)) DEALLOCATE(value_array)
        END IF
        !If the block is flagged handled then you care about the error code
        !value
        IF (IAND(this_stat, eis_status_not_handled) == 0) THEN
          errcode = IOR(errcode, this_err)
          status = IOR(status, this_stat)
        END IF
        handled = handled .OR. (IAND(this_stat, eis_status_not_handled) == 0)
      END IF
      IF ((ASSOCIATED(dkd%key_stack_fn) .OR. ASSOCIATED(dkd%c_key_stack_fn)) &
          .AND. is_key_stack .AND. .NOT. handled) THEN
        IF (.NOT. PRESENT(value_function)) THEN
          IF (is_key_value) THEN
            CALL ps%tokenize(value, stack, stack_err, &
                filename = filename, line_number = line_number, &
                char_offset = char_offset)
            parser_error = stack_err /= eis_err_none
          ELSE
            stack_err = eis_err_bad_value
          END IF
        ELSE
          CALL ps%set_result_function(value_function, stack, &
              stack_err)
            parser_error = stack_err /= eis_err_none
        END IF
        IF (stack_err /= eis_err_none) errcode = IOR(errcode, stack_err)
        IF (ASSOCIATED(dkd%key_stack_fn) .AND. stack_err == eis_err_none) THEN
          this_err = eis_err_none
          this_stat = base_stat
          CALL dkd%key_stack_fn(key, stack, pass_number, ps, parents, &
              parent_kind, this_stat, this_bitmask, this_err)
          !If the block is flagged handled then you care about the error code
          !value
          IF (IAND(this_stat, eis_status_not_handled) == 0) THEN
            errcode = IOR(errcode, this_err)
            errcode = IOR(errcode, stack_err)
            status = IOR(status, this_stat)
          END IF
          handled = handled .OR. (IAND(this_stat, eis_status_not_handled) == 0)
        END IF
        IF (stack_err /= eis_err_none) errcode = IOR(errcode, stack_err)
        IF (ASSOCIATED(dkd%c_key_stack_fn) .AND. stack_err == eis_err_none &
            .AND. .NOT. handled) THEN
          ALLOCATE(stack_ptr)
          stack_ptr = stack
          stack_id = eis_add_interop_stack(stack_ptr, &
              interop_parser, owns = .TRUE.)
          this_err = eis_err_none
          this_stat = base_stat
          CALL dkd%c_key_stack_fn(C_LOC(c_key), INT(stack_id, C_INT), &
              INT(pass_number, C_INT), interop_parser, &
              SIZE(parents, KIND = C_INT), INT(parents, C_INT), &
              INT(parent_kind, C_INT), this_stat, this_bitmask, this_err)
          IF (IAND(this_stat, eis_status_retain_stack) == 0) &
              CALL eis_release_interop_stack(stack_id)
          !If the block is flagged handled then you care about the error code
          !value
          IF (IAND(this_stat, eis_status_not_handled) == 0) THEN
            errcode = IOR(errcode, this_err)
            errcode = IOR(errcode, stack_err)
            status = IOR(status, this_stat)
          END IF
          handled = handled .OR. (IAND(this_stat, eis_status_not_handled) == 0)
        END IF
      END IF

      IF ((isscalarvar .OR. isarrayvar) .AND. is_key_stack &
          .AND. .NOT. handled) THEN

        this_err = eis_err_none
        IF (.NOT. PRESENT(value_function)) THEN
          IF (is_key_value) THEN
            ct = ps%evaluate(value, value_array, this_err, &
                filename = filename, line_number = line_number, &
                char_offset = char_offset)
            parser_error = this_err /= eis_err_none
          ELSE
            this_err = eis_err_bad_value
          END IF
        ELSE 
          CALL ps%set_result_function(value_function, stack, &
              this_err)
          ct = ps%evaluate(stack, value_array, this_err)
        END IF
        IF (this_err == eis_err_none) THEN
          IF (ct == 1 .AND. isscalarvar) THEN
            handled = .TRUE.
            IF (ASSOCIATED(dkd%i32data)) dkd%i32data = NINT(value_array(1),&
                INT32)
            IF (ASSOCIATED(dkd%i64data)) dkd%i64data = NINT(value_array(1),&
                INT64)
            IF (ASSOCIATED(dkd%r32data)) dkd%r32data = REAL(value_array(1), &
                REAL32)
            IF (ASSOCIATED(dkd%r64data)) dkd%r64data = REAL(value_array(1), &
                REAL64)
            IF (ASSOCIATED(dkd%logicaldata)) dkd%logicaldata = &
                .NOT. (ABS(value_array(1)) < 1.0e-6_eis_num)
            IF (C_ASSOCIATED(dkd%c_i32data)) THEN
              CALL C_F_POINTER(dkd%c_i32data, c_i32, [1])
              c_i32(1) = NINT(value_array(1), INT32)
            END IF
            IF (C_ASSOCIATED(dkd%c_i64data)) THEN
              CALL C_F_POINTER(dkd%c_i64data, c_i64, [1])
              c_i64(1) = NINT(value_array(1), INT64)
            END IF
            IF (C_ASSOCIATED(dkd%c_r32data)) THEN
              CALL C_F_POINTER(dkd%c_r32data, c_r32, [1])
              c_r32(1) = REAL(value_array(1), REAL32)
            END IF
            IF (C_ASSOCIATED(dkd%c_r64data)) THEN
              CALL C_F_POINTER(dkd%c_r64data, c_r64, [1])
              c_r64(1) = REAL(value_array(1), REAL64)
            END IF
          END IF

          IF (isarrayvar) THEN
            IF (ASSOCIATED(dkd%i32arraydata)) THEN
              IF (SIZE(dkd%i32arraydata) >= ct) THEN
                dkd%i32arraydata(LBOUND(dkd%i32arraydata,1):&
                    LBOUND(dkd%i32arraydata,1) + ct-1) &
                    = NINT(value_array(1:ct), INT32)
                handled = .TRUE.
              ELSE
                this_err = IOR(this_err, eis_err_bad_value)
              END IF
            END IF

            IF (ASSOCIATED(dkd%i64arraydata)) THEN
              IF (SIZE(dkd%i64arraydata) >= ct) THEN
                dkd%i64arraydata(LBOUND(dkd%i64arraydata,1):&
                    LBOUND(dkd%i64arraydata,1) + ct-1) &
                    = NINT(value_array(1:ct), INT64)
                handled = .TRUE.
              ELSE
                this_err = IOR(this_err, eis_err_bad_value)
              END IF
            END IF

            IF (ASSOCIATED(dkd%r32arraydata)) THEN
              IF (SIZE(dkd%r32arraydata) >= ct) THEN
                dkd%r32arraydata(LBOUND(dkd%r32arraydata,1):&
                    LBOUND(dkd%r32arraydata,1) + ct-1) &
                    = REAL(value_array(1:ct), REAL32)
                handled = .TRUE.
              ELSE
                this_err = IOR(this_err, eis_err_bad_value)
              END IF
            END IF

            IF (ASSOCIATED(dkd%r64arraydata)) THEN
              IF (SIZE(dkd%r64arraydata) >= ct) THEN
                dkd%r64arraydata(LBOUND(dkd%r64arraydata,1):&
                    LBOUND(dkd%r64arraydata,1) + ct-1) &
                    = REAL(value_array(1:ct), REAL64)
                handled = .TRUE.
              ELSE
                this_err = IOR(this_err, eis_err_bad_value)
              END IF
            END IF

            IF (ASSOCIATED(dkd%logicalarraydata)) THEN
              IF (SIZE(dkd%logicalarraydata) >= ct) THEN
                dkd%logicalarraydata(LBOUND(dkd%logicalarraydata,1):&
                    LBOUND(dkd%logicalarraydata,1) + ct-1) = &
                    .NOT. (ABS(value_array(1:ct)) < 1.0e-6_eis_num)
                handled = .TRUE.
              ELSE
                this_err = IOR(this_err, eis_err_bad_value)
              END IF
            END IF

            IF (C_ASSOCIATED(dkd%c_i32data)) THEN
              IF (dkd%c_i32len >= ct) THEN
                CALL C_F_POINTER(dkd%c_i32data, c_i32, [ct])
                c_i32(1:ct) = NINT(value_array(1:ct), INT32)
              ELSE
                this_err = IOR(this_err, eis_err_bad_value)
              END IF
            END IF

            IF (C_ASSOCIATED(dkd%c_i64data)) THEN
              IF (dkd%c_i64len >= ct) THEN
                CALL C_F_POINTER(dkd%c_i64data, c_i64, [ct])
                c_i64(1:ct) = NINT(value_array(1:ct), INT64)
              ELSE
                this_err = IOR(this_err, eis_err_bad_value)
              END IF
            END IF

            IF (C_ASSOCIATED(dkd%c_r32data)) THEN
              IF (dkd%c_r32len >= ct) THEN
                CALL C_F_POINTER(dkd%c_r32data, c_r32, [ct])
                c_r32(1:ct) = REAL(value_array(1:ct), REAL32)
              ELSE
                this_err = IOR(this_err, eis_err_bad_value)
              END IF
            END IF

            IF (C_ASSOCIATED(dkd%c_r64data)) THEN
              IF (dkd%c_r64len >= ct) THEN
                CALL C_F_POINTER(dkd%c_r64data, c_r64, [ct])
                c_r64(1:ct) = REAL(value_array(1:ct), REAL64)
              ELSE
                this_err = IOR(this_err, eis_err_bad_value)
              END IF
            END IF

          END IF
        END IF
        IF (handled) THEN
          errcode = IOR(errcode, this_err)
          status = IOR(status, this_stat)
        END IF
      END IF
      !Fallback to text if not handled earlier
      IF (dkd%text_fallback .AND. errcode /= eis_err_none) THEN
        handled = .FALSE.
        IF (ASSOCIATED(dkd%key_text_fn)) THEN
          this_err = eis_err_none
          this_stat = base_stat
          CALL dkd%key_text_fn(TRIM(key_text), pass_number, parents, &
              parent_kind, this_stat, this_bitmask, this_err)
          handled = (handled &
              .OR. (IAND(this_stat, eis_status_not_handled) == 0) &
              .AND. this_err == eis_err_none)
        END IF
        IF (ASSOCIATED(dkd%c_key_text_fn) .AND. .NOT. handled) THEN
          this_err = eis_err_none
          this_stat = base_stat
          CALL dkd%c_key_text_fn(C_LOC(c_key_text), INT(pass_number, C_INT), &
              SIZE(parents, KIND=C_INT), INT(parents, C_INT), &
              INT(parent_kind, C_INT), this_stat, this_bitmask, this_err)
          handled = (handled &
              .OR. (IAND(this_stat, eis_status_not_handled) == 0) &
              .AND. this_err == eis_err_none)
        END IF
        IF (ASSOCIATED(dkd%key_value_fn) .AND. is_key_value .AND. &
            .NOT. handled) THEN
          this_err = eis_err_none
          this_stat = base_stat
          CALL dkd%key_value_fn(key, value, pass_number, parents, parent_kind, &
              this_stat, this_bitmask, this_err)
          handled = (handled &
              .OR. (IAND(this_stat, eis_status_not_handled) == 0) &
              .AND. this_err == eis_err_none)
        END IF
        IF (ASSOCIATED(dkd%c_key_value_fn) .AND. is_key_value .AND. &
            .NOT. handled) THEN
          this_err = eis_err_none
          this_stat = base_stat
          CALL dkd%c_key_value_fn(C_LOC(c_key), C_LOC(c_value), &
              INT(pass_number, C_INT), SIZE(parents, KIND=C_INT), &
              INT(parents, C_INT), INT(parent_kind, C_INT), this_stat, &
              this_bitmask, this_err)
          handled = (handled &
              .OR. (IAND(this_stat, eis_status_not_handled) == 0) &
              .AND. this_err == eis_err_none)
        END IF
        !If have handled then there is no error
        IF (handled) errcode = eis_err_none
      END IF
    END IF

    run = .NOT. ANY([this%use_eq, this%use_le, this%use_ge])
    IF (this%use_eq) run = run .OR. (pass_number == this%pass_eq)
    IF (this%use_le) run = run .OR. (pass_number <= this%pass_le)
    IF (this%use_ge) run = run .OR. (pass_number >= this%pass_ge)

    IF (run .AND. (.NOT. ASSOCIATED(dkd) .OR. (ASSOCIATED(dkd) &
        .AND. any_candidates))) THEN
      IF (.NOT. this%text_fallback) THEN
        IF (ASSOCIATED(this%any_key_text_fn) .AND. .NOT. handled) THEN
          this_err = eis_err_none
          this_stat = base_stat
          CALL this%any_key_text_fn(TRIM(key_text), pass_number, parents,&
              parent_kind, this_stat, this_bitmask, this_err)
          !If the block is flagged handled then you care about the error code
          !value
          IF (IAND(this_stat, eis_status_not_handled) == 0) THEN
            errcode = IOR(errcode, this_err)
            status = IOR(status, this_stat)
          END IF
          handled = handled .OR. (IAND(this_stat, eis_status_not_handled) == 0)
        END IF
        IF (ASSOCIATED(this%c_any_key_text_fn) .AND. .NOT. handled) THEN
          this_err = eis_err_none
          this_stat = base_stat
          CALL this%c_any_key_text_fn(C_LOC(c_key_text), &
              INT(pass_number, C_INT), SIZE(parents, KIND = C_INT), &
              INT(parents, C_INT), INT(parent_kind, C_INT), this_stat, &
              this_bitmask, this_err)
          !If the block is flagged handled then you care about the error code
          !value
          IF (IAND(this_stat, eis_status_not_handled) == 0) THEN
            errcode = IOR(errcode, this_err)
            status = IOR(status, this_stat)
          END IF
          handled = handled .OR. (IAND(this_stat, eis_status_not_handled) == 0)
        END IF
        IF (ASSOCIATED(this%any_key_value_fn) .AND. is_key_value .AND. &
            .NOT. handled) THEN
          this_err = eis_err_none
          this_stat = base_stat
          CALL this%any_key_value_fn(key, value, pass_number, parents, &
              parent_kind, this_stat, this_bitmask, this_err)
          !If the block is flagged handled then you care about the error code
          !value
          IF (IAND(this_stat, eis_status_not_handled) == 0) THEN
            errcode = IOR(errcode, this_err)
            status = IOR(status, this_stat)
          END IF
          handled = handled .OR. (IAND(this_stat, eis_status_not_handled) == 0)
        END IF
        IF (ASSOCIATED(this%c_any_key_value_fn) .AND. is_key_value .AND. &
            .NOT. handled) THEN
          this_err = eis_err_none
          this_stat = base_stat
          CALL this%c_any_key_value_fn(C_LOC(c_key), C_LOC(c_value), &
              INT(pass_number, C_INT), SIZE(parents, KIND = C_INT), &
              INT(parents, C_INT), INT(parent_kind, C_INT), this_stat, &
              this_bitmask, this_err)
          !If the block is flagged handled then you care about the error code
          !value
          IF (IAND(this_stat, eis_status_not_handled) == 0) THEN
            errcode = IOR(errcode, this_err)
            status = IOR(status, this_stat)
          END IF
          handled = handled .OR. (IAND(this_stat, eis_status_not_handled) == 0)
        END IF
      END IF
      IF (ASSOCIATED(this%any_key_numeric_value_fn) .AND. &
          ASSOCIATED(ps) .AND. is_key_stack &
          .AND. .NOT. handled) THEN
        this_err = eis_err_none
        this_stat = base_stat
        IF (.NOT. PRESENT(value_function)) THEN
          IF (is_key_value) THEN
            ct = ps%evaluate(value, value_array, this_err, &
                filename = filename, line_number = line_number, &
                char_offset = char_offset, cap_bits = cbits)
            parser_error = this_err /= eis_err_none
          ELSE
            this_err = eis_err_bad_value
          END IF
        ELSE
          CALL ps%set_result_function(value_function, stack, &
              this_err)
          cbits = 0_eis_bitmask
          ct = ps%evaluate(stack, value_array, this_err)
        END IF
        IF (this_err == eis_err_none) THEN
          CALL this%any_key_numeric_value_fn(key, value_array, pass_number, &
              cbits, ps, parents, parent_kind, this_stat, this_bitmask, &
              this_err)
        END IF
        IF (ALLOCATED(value_array)) DEALLOCATE(value_array)
        !If the block is flagged handled then you care about the error code
        !value
        IF (IAND(this_stat, eis_status_not_handled) == 0) THEN
          errcode = IOR(errcode, this_err)
          status = IOR(status, this_stat)
        END IF
        handled = handled .OR. (IAND(this_stat, eis_status_not_handled) == 0)
      END IF
      IF (ASSOCIATED(this%c_any_key_numeric_value_fn) .AND. &
          ASSOCIATED(ps) .AND. is_key_stack &
          .AND. .NOT. handled) THEN
        this_err = eis_err_none
        this_stat = base_stat
        IF (.NOT. PRESENT(value_function)) THEN
          IF (is_key_value) THEN
            ct = ps%evaluate(value, value_array, this_err, &
                filename = filename, line_number = line_number, &
                char_offset = char_offset, cap_bits = cbits)
            parser_error = this_err /= eis_err_none
          ELSE
            this_err = eis_err_bad_value
          END IF
        ELSE
          CALL ps%set_result_function(value_function, stack, &
              this_err)
          cbits = 0_eis_bitmask
          ct = ps%evaluate(stack, value_array, this_err)
        END IF
        IF (this_err == eis_err_none) THEN
          CALL this%c_any_key_numeric_value_fn(C_LOC(c_key), &
              SIZE(value_array, KIND = C_INT), value_array, &
              INT(pass_number, C_INT), INT(cbits, eis_bitmask_c), &
              interop_parser, SIZE(parents, KIND = C_INT), &
              INT(parents, C_INT), INT(parent_kind, C_INT), this_stat, &
              this_bitmask, this_err)
        END IF
        IF (ALLOCATED(value_array)) DEALLOCATE(value_array)
        !If the block is flagged handled then you care about the error code
        !value
        IF (IAND(this_stat, eis_status_not_handled) == 0) THEN
          errcode = IOR(errcode, this_err)
          status = IOR(status, this_stat)
        END IF
        handled = handled .OR. (IAND(this_stat, eis_status_not_handled) == 0)
      END IF
      IF ((ASSOCIATED(this%any_key_stack_fn) &
          .OR. ASSOCIATED(this%c_any_key_stack_fn)) &
          .AND. ASSOCIATED(ps) &
          .AND. is_key_stack .AND. .NOT. handled) THEN
        this_err = eis_err_none
        this_stat = base_stat
        IF (.NOT. PRESENT(value_function)) THEN
          IF (is_key_value) THEN
            CALL ps%tokenize(value, stack, stack_err, &
                filename = filename, line_number = line_number, &
                char_offset = char_offset)
            parser_error = stack_err /= eis_err_none
          ELSE
            stack_err = eis_err_bad_value
          END IF
        ELSE
          CALL ps%set_result_function(value_function, stack, &
              stack_err)
            parser_error = stack_err /= eis_err_none
        END IF
        IF (stack_err == eis_err_none) THEN
          CALL this%any_key_stack_fn(key, stack, pass_number, ps, parents, &
            parent_kind, this_stat, this_bitmask, this_err)
          IF (IAND(this_stat, eis_status_not_handled) == 0) THEN
            errcode = IOR(errcode, this_err)
            status = IOR(status, this_stat)
          END IF
        ELSE
          errcode = IOR(errcode, stack_err)
        END IF
        !If the block is flagged handled then you care about the error code
        !value
        IF (IAND(this_stat, eis_status_not_handled) == 0) THEN
          errcode = IOR(errcode, this_err)
          status = IOR(status, this_stat)
        END IF
        handled = handled .OR. (IAND(this_stat, eis_status_not_handled) == 0)
        IF (stack_err == eis_err_none .AND. .NOT. handled) THEN
          stack_ptr => stack
          stack_id = eis_add_interop_stack(stack_ptr, &
              interop_parser, owns = .FALSE.)
          this_err = eis_err_none
          this_stat = base_stat
          CALL this%c_any_key_stack_fn(C_LOC(c_key), INT(stack_id, C_INT), &
              INT(pass_number, C_INT), interop_parser, &
              SIZE(parents, KIND = C_INT), INT(parents, C_INT), &
              INT(parent_kind, C_INT), this_stat, this_bitmask, this_err)
          IF (IAND(this_stat, eis_status_retain_stack) == 0) &
              CALL eis_release_interop_stack(stack_id)
          !If the block is flagged handled then you care about the error code
          !value
          IF (IAND(this_stat, eis_status_not_handled) == 0) THEN
            errcode = IOR(errcode, this_err)
            errcode = IOR(errcode, stack_err)
            status = IOR(status, this_stat)
          END IF
          handled = handled .OR. (IAND(this_stat, eis_status_not_handled) == 0)
        ELSE
          errcode = IOR(errcode, stack_err)
        END IF
      END IF
      IF (this%text_fallback .AND. errcode /= eis_err_none) THEN
        handled = .FALSE.
        IF (ASSOCIATED(this%any_key_text_fn) .AND. .NOT. handled) THEN
          this_err = eis_err_none
          this_stat = base_stat
          CALL this%any_key_text_fn(TRIM(key_text), pass_number, parents,&
              parent_kind, this_stat, this_bitmask, this_err)
          handled = (handled &
              .OR. (IAND(this_stat, eis_status_not_handled) == 0) &
              .AND. this_err == eis_err_none)
        END IF
        IF (ASSOCIATED(this%c_any_key_text_fn) .AND. .NOT. handled) THEN
          this_err = eis_err_none
          this_stat = base_stat
          CALL this%c_any_key_text_fn(C_LOC(c_key_text), &
              INT(pass_number, C_INT), SIZE(parents, KIND = C_INT), &
              INT(parents, C_INT), INT(parent_kind, C_INT), this_stat, &
              this_bitmask, this_err)
          handled = (handled &
              .OR. (IAND(this_stat, eis_status_not_handled) == 0) &
              .AND. this_err == eis_err_none)
        END IF
        IF (ASSOCIATED(this%any_key_value_fn) .AND. is_key_value .AND. &
            .NOT. handled) THEN
          this_err = eis_err_none
          this_stat = base_stat
          CALL this%any_key_value_fn(key, value, pass_number, parents, &
              parent_kind, this_stat, this_bitmask, this_err)
          handled = (handled &
              .OR. (IAND(this_stat, eis_status_not_handled) == 0) &
              .AND. this_err == eis_err_none)
        END IF
        IF (ASSOCIATED(this%c_any_key_value_fn) .AND. is_key_value .AND. &
            .NOT. handled) THEN
          this_err = eis_err_none
          this_stat = base_stat
          CALL this%c_any_key_value_fn(C_LOC(c_key), C_LOC(c_value), &
              INT(pass_number, C_INT), SIZE(parents, KIND = C_INT), &
              INT(parents, C_INT), INT(parent_kind, C_INT), this_stat, &
              this_bitmask, this_err)
          handled = (handled &
              .OR. (IAND(this_stat, eis_status_not_handled) == 0) &
              .AND. this_err == eis_err_none)
        END IF
      END IF
    END IF

    !If there are no candidate functions but the block exists then you _have_
    !handled it
    handled = handled .OR. (ASSOCIATED(dkd) .AND. .NOT. any_candidates)

    IF (.NOT. handled) THEN
      IF (ASSOCIATED(dkd)) THEN
        errcode = IOR(errcode, eis_err_bad_value)
        status = IOR(status, this_stat)
      ELSE
        errcode = IOR(errcode, eis_err_unknown_key)
        status = IOR(status, this_stat)
      END IF
    END IF

    IF (errcode == eis_err_none) THEN
      IF (ASSOCIATED(this%info%on_key_success_fn)) THEN
        CALL this%info%on_key_success_fn(TRIM(key_text), pass_number, parents, &
              parent_kind, this_stat, this_bitmask, this_err)
        errcode = IOR(errcode, this_err)
        status = IOR(status, this_stat)
      END IF

      IF (ASSOCIATED(this%info%c_on_key_success_fn)) THEN
        CALL this%info%c_on_key_success_fn(C_LOC(c_key_text), &
            INT(pass_number, C_INT), SIZE(parents, KIND=C_INT), &
            INT(parents, C_INT), INT(parent_kind, C_INT), this_stat, &
            this_bitmask, this_err)
        errcode = IOR(errcode, this_err)
        status = IOR(status, this_stat)
      END IF
    ELSE
      IF (ASSOCIATED(this%info%on_key_failure_fn)) THEN
        CALL this%info%on_key_failure_fn(TRIM(key_text), pass_number, parents, &
              parent_kind, this_stat, this_bitmask, errcode)
      END IF

      IF (ASSOCIATED(this%info%c_on_key_failure_fn)) THEN
        CALL this%info%c_on_key_failure_fn(C_LOC(c_key_text), &
            INT(pass_number, C_INT), SIZE(parents, KIND=C_INT), &
            INT(parents, C_INT), INT(parent_kind, C_INT), this_stat, &
            this_bitmask, errcode)
        errcode = IOR(errcode, this_err)
        status = IOR(status, this_stat)
      END IF
    END IF

    IF (PRESENT(host_state)) host_state = IOR(host_state, this_bitmask)
    IF (errcode /= eis_err_none .AND. PRESENT(err_handler) &
        .AND. .NOT. parser_error) THEN
      CALL err_handler%add_error(eis_err_deck_parser, errcode, &
          filename = filename, line_number = line_number)
    END IF

    DEALLOCATE(parent_kind)
    DEALLOCATE(c_key)
    DEALLOCATE(c_value)

  END SUBROUTINE dbd_call_key_text



  SUBROUTINE dkd_init(this, parent_block, key_name, key_text_fn, key_value_fn, &
      key_numeric_value_fn, key_stack_fn, should_key_trigger_fn, &
      c_key_text_fn, c_key_value_fn, c_key_numeric_value_fn, c_key_stack_fn, &
      c_should_key_trigger_fn, i32value, i64value, r32value, &
      r64value, logicalvalue, i32array, i64array, r32array, r64array, &
      logicalarray, c_i32value, c_i64value, c_r32value, c_r64value, c_i32len, &
      c_i64len, c_r32len, c_r64len, init_flag, i32count, i64count, &
      expected_params, pass_eq, pass_le, pass_ge, description, hidden, &
      text_fallback)
    CLASS(deck_key_definition), INTENT(INOUT) :: this
    CLASS(eis_deck_block_definition), INTENT(IN) :: parent_block
    CHARACTER(LEN=*), INTENT(IN) :: key_name
    PROCEDURE(key_text_callback), OPTIONAL :: key_text_fn
    PROCEDURE(key_value_callback), OPTIONAL :: key_value_fn
    PROCEDURE(key_numeric_value_callback), OPTIONAL :: key_numeric_value_fn
    PROCEDURE(key_stack_callback), OPTIONAL :: key_stack_fn
    PROCEDURE(should_key_trigger_callback), OPTIONAL :: should_key_trigger_fn
    PROCEDURE(key_text_callback_c), OPTIONAL :: c_key_text_fn
    PROCEDURE(key_value_callback_c), OPTIONAL :: c_key_value_fn
    PROCEDURE(key_numeric_value_callback_c), OPTIONAL :: c_key_numeric_value_fn
    PROCEDURE(key_stack_callback_c), OPTIONAL :: c_key_stack_fn
    PROCEDURE(should_key_trigger_callback_c), OPTIONAL :: &
        c_should_key_trigger_fn
#ifdef F2008
    INTEGER(INT32), TARGET, OPTIONAL :: i32value
    INTEGER(INT64), TARGET, OPTIONAL :: i64value
    REAL(REAL32), TARGET, OPTIONAL :: r32value
    REAL(REAL64), TARGET, OPTIONAL :: r64value
    LOGICAL, TARGET, OPTIONAL :: logicalvalue
    INTEGER(INT32), DIMENSION(:), TARGET, OPTIONAL :: i32array
    INTEGER(INT64), DIMENSION(:), TARGET, OPTIONAL :: i64array
    REAL(REAL32), DIMENSION(:), TARGET, OPTIONAL :: r32array
    REAL(REAL64), DIMENSION(:), TARGET, OPTIONAL :: r64array
    LOGICAL, DIMENSION(:), TARGET, OPTIONAL :: logicalarray

    LOGICAL, TARGET, OPTIONAL :: init_flag
    INTEGER(INT32), TARGET, OPTIONAL :: i32count
    INTEGER(INT64), TARGET, OPTIONAL :: i64count
#else
    INTEGER(INT32), POINTER, OPTIONAL :: i32value
    INTEGER(INT64), POINTER, OPTIONAL :: i64value
    REAL(REAL32), POINTER, OPTIONAL :: r32value
    REAL(REAL64), POINTER, OPTIONAL :: r64value
    LOGICAL, POINTER, OPTIONAL :: logicalvalue
    INTEGER(INT32), DIMENSION(:), POINTER, OPTIONAL :: i32array
    INTEGER(INT64), DIMENSION(:), POINTER, OPTIONAL :: i64array
    REAL(REAL32), DIMENSION(:), POINTER, OPTIONAL :: r32array
    REAL(REAL64), DIMENSION(:), POINTER, OPTIONAL :: r64array
    LOGICAL, DIMENSION(:), POINTER, OPTIONAL :: logicalarray

    LOGICAL, POINTER, OPTIONAL :: init_flag
    INTEGER(INT32), POINTER, OPTIONAL :: i32count
    INTEGER(INT64), POINTER, OPTIONAL :: i64count
#endif
    TYPE(C_PTR), INTENT(IN), OPTIONAL :: c_i32value
    TYPE(C_PTR), INTENT(IN), OPTIONAL :: c_i64value
    TYPE(C_PTR), INTENT(IN), OPTIONAL :: c_r32value
    TYPE(C_PTR), INTENT(IN), OPTIONAL :: c_r64value
    INTEGER, INTENT(IN), OPTIONAL :: c_i32len
    INTEGER, INTENT(IN), OPTIONAL :: c_i64len
    INTEGER, INTENT(IN), OPTIONAL :: c_r32len
    INTEGER, INTENT(IN), OPTIONAL :: c_r64len
    INTEGER, INTENT(IN), OPTIONAL :: expected_params
    INTEGER, INTENT(IN), OPTIONAL :: pass_eq, pass_le, pass_ge
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    LOGICAL, INTENT(IN), OPTIONAL :: hidden
    LOGICAL, INTENT(IN), OPTIONAL :: text_fallback
    LOGICAL :: inherit

    ALLOCATE(this%name, SOURCE = key_name)
    IF (PRESENT(key_text_fn)) this%key_text_fn => key_text_fn
    IF (PRESENT(key_value_fn)) this%key_value_fn => key_value_fn
    IF (PRESENT(key_numeric_value_fn)) this%key_numeric_value_fn &
        => key_numeric_value_fn
    IF (PRESENT(key_stack_fn)) this%key_stack_fn => key_stack_fn
    IF (PRESENT(should_key_trigger_fn)) this%should_key_trigger_fn &
        => should_key_trigger_fn
    IF (PRESENT(c_key_text_fn)) this%c_key_text_fn => c_key_text_fn
    IF (PRESENT(c_key_value_fn)) this%c_key_value_fn => c_key_value_fn
    IF (PRESENT(c_key_numeric_value_fn)) this%c_key_numeric_value_fn &
        => c_key_numeric_value_fn
    IF (PRESENT(c_key_stack_fn)) this%c_key_stack_fn => c_key_stack_fn
    IF (PRESENT(c_should_key_trigger_fn)) this%c_should_key_trigger_fn &
        => c_should_key_trigger_fn

    IF (PRESENT(i32value)) this%i32data => i32value
    IF (PRESENT(i64value)) this%i64data => i64value
    IF (PRESENT(r32value)) this%r32data => r32value
    IF (PRESENT(r64value)) this%r64data => r64value

    IF (PRESENT(i32array)) this%i32arraydata => i32array
    IF (PRESENT(i64array)) this%i64arraydata => i64array
    IF (PRESENT(r32array)) this%r32arraydata => r32array
    IF (PRESENT(r64array)) this%r64arraydata => r64array

    IF (PRESENT(init_flag)) this%set_variable => init_flag
    IF (PRESENT(i32count)) this%i32count_variable => i32count
    IF (PRESENT(i64count)) this%i64count_variable => i64count

    IF (PRESENT(c_i32value)) this%c_i32data = c_i32value
    IF (PRESENT(c_i64value)) this%c_i64data = c_i64value
    IF (PRESENT(c_r32value)) this%c_r32data = c_r32value
    IF (PRESENT(c_r64value)) this%c_r64data = c_r64value

    IF (PRESENT(c_i32len)) this%c_i32len = c_i32len
    IF (PRESENT(c_i64len)) this%c_i64len = c_i64len
    IF (PRESENT(c_r32len)) this%c_r32len = c_r32len
    IF (PRESENT(c_r64len)) this%c_r64len = c_r64len

    IF (PRESENT(expected_params)) this%expected_params = expected_params
    IF (PRESENT(description)) ALLOCATE(this%description, SOURCE = description)
    IF (PRESENT(hidden)) this%is_hidden = hidden
    inherit = .TRUE.
    IF (PRESENT(pass_eq)) THEN
      this%pass_eq = pass_eq
      this%use_eq = .TRUE.
      inherit = .FALSE.
    END IF
    IF (PRESENT(pass_le)) THEN
      this%pass_le = pass_le
      this%use_le = .TRUE.
      inherit = .FALSE.
    END IF
    IF (PRESENT(pass_ge)) THEN
      this%pass_ge = pass_ge
      this%use_ge = .TRUE.
      inherit = .FALSE.
    END IF

    IF (inherit) THEN
      this%pass_eq = parent_block%pass_eq
      this%use_eq = parent_block%use_eq
      this%pass_le = parent_block%pass_le
      this%use_le = parent_block%use_le
      this%pass_ge = parent_block%pass_ge
      this%use_ge = parent_block%use_ge
    END IF

    IF (PRESENT(text_fallback)) THEN
      this%text_fallback = text_fallback
    ELSE
      this%text_fallback = parent_block%text_fallback
    END IF

  END SUBROUTINE dkd_init

END MODULE eis_deck_definition_mod
