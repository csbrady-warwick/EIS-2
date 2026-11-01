! Copyright (c) 2019-2026, C. S. Brady and H. Ratcliffe
! SPDX-License-Identifier: BSD-3-Clause

#include "interop_version.inc"
#if (EIS_C_INTEROP_VERSION <= 2)
MODULE eis_deck_definition_interop

  USE, INTRINSIC :: ISO_C_BINDING
  USE eis_header
  USE eis_utils
  USE eis_deck_function_mod
  USE eis_deck_definition_mod
  IMPLICIT NONE

  PRIVATE :: add_interop_definition

  !> Holder for a deck definition that is referenced from C by an integer ID
  TYPE :: deck_definition_holder
    TYPE(eis_deck_definition), POINTER :: contents => NULL()
    INTEGER :: refcount = 1
    LOGICAL :: in_use = .FALSE.
  END TYPE deck_definition_holder

  TYPE(deck_definition_holder), DIMENSION(:), ALLOCATABLE :: &
      interop_definitions

  !> Parameters for eis_deck_create_definition
  TYPE, BIND(C) :: eis_deck_create_definition_params
    TYPE(C_FUNPTR) :: init_deck
    TYPE(C_FUNPTR) :: start_pass
    TYPE(C_FUNPTR) :: start_deck
    TYPE(C_FUNPTR) :: end_deck
    TYPE(C_FUNPTR) :: end_pass
    TYPE(C_FUNPTR) :: final_deck
    TYPE(C_FUNPTR) :: any_key_text
    TYPE(C_FUNPTR) :: any_key_value
    TYPE(C_FUNPTR) :: any_key_numeric_value
    TYPE(C_FUNPTR) :: any_key_stack
    TYPE(C_FUNPTR) :: block_remapper
    TYPE(C_FUNPTR) :: on_key_success
    TYPE(C_FUNPTR) :: on_key_failure
    TYPE(C_FUNPTR) :: on_key_no_trigger
    TYPE(C_FUNPTR) :: on_block_start
    TYPE(C_FUNPTR) :: on_block_end
    TYPE(C_FUNPTR) :: on_block_no_trigger
    TYPE(C_FUNPTR) :: on_block_failure
    TYPE(C_FUNPTR) :: on_init
    TYPE(C_FUNPTR) :: on_start_pass
    TYPE(C_FUNPTR) :: on_end_pass
    TYPE(C_FUNPTR) :: on_final
    TYPE(C_FUNPTR) :: on_generic_block_failure
    INTEGER(C_INT) :: interop_parser
    INTEGER(C_INT) :: text_fallback
    INTEGER(C_INT) :: no_interop
  END TYPE eis_deck_create_definition_params

  !> Parameters for eis_deck_definition_inc_ref
  TYPE, BIND(C) :: eis_deck_definition_inc_ref_params
    INTEGER(C_INT) :: dummy
  END TYPE eis_deck_definition_inc_ref_params

  !> Parameters for eis_deck_definition_dec_ref
  TYPE, BIND(C) :: eis_deck_definition_dec_ref_params
    INTEGER(C_INT) :: dummy
  END TYPE eis_deck_definition_dec_ref_params

  !> Parameters for eis_deck_get_root_block
  TYPE, BIND(C) :: eis_deck_get_root_block_params
    INTEGER(C_INT) :: dummy
  END TYPE eis_deck_get_root_block_params

  !> Parameters for eis_deck_add_block
  TYPE, BIND(C) :: eis_deck_add_block_params
    TYPE(C_FUNPTR) :: init_block
    TYPE(C_FUNPTR) :: start_pass
    TYPE(C_FUNPTR) :: start_block
    TYPE(C_FUNPTR) :: end_block
    TYPE(C_FUNPTR) :: end_pass
    TYPE(C_FUNPTR) :: final_block
    TYPE(C_FUNPTR) :: any_key_text
    TYPE(C_FUNPTR) :: any_key_value
    TYPE(C_FUNPTR) :: any_key_numeric_value
    TYPE(C_FUNPTR) :: any_key_stack
    TYPE(C_FUNPTR) :: block_remapper
    INTEGER(C_INT) :: pass_eq
    INTEGER(C_INT) :: pass_le
    INTEGER(C_INT) :: pass_ge
    TYPE(C_PTR) :: init_flag
    TYPE(C_PTR) :: i32count
    TYPE(C_PTR) :: i64count
    TYPE(C_PTR) :: description
    INTEGER(C_INT) :: hidden
    INTEGER(C_INT) :: text_fallback
  END TYPE eis_deck_add_block_params

  !> Parameters for eis_deck_add_key
  TYPE, BIND(C) :: eis_deck_add_key_params
    TYPE(C_FUNPTR) :: key_text_fn
    TYPE(C_FUNPTR) :: key_value_fn
    TYPE(C_FUNPTR) :: key_numeric_value_fn
    TYPE(C_FUNPTR) :: key_stack_fn
    TYPE(C_FUNPTR) :: should_key_trigger_fn
    TYPE(C_PTR) :: i32value
    INTEGER(C_INT) :: i32len
    TYPE(C_PTR) :: i64value
    INTEGER(C_INT) :: i64len
    TYPE(C_PTR) :: r32value
    INTEGER(C_INT) :: r32len
    TYPE(C_PTR) :: r64value
    INTEGER(C_INT) :: r64len
    INTEGER(C_INT) :: stackvalue
    TYPE(C_PTR) :: init_flag
    TYPE(C_PTR) :: i32count
    TYPE(C_PTR) :: i64count
    INTEGER(C_INT) :: expected_params
    INTEGER(C_INT) :: pass_eq
    INTEGER(C_INT) :: pass_le
    INTEGER(C_INT) :: pass_ge
    TYPE(C_PTR) :: description
    INTEGER(C_INT) :: hidden
    INTEGER(C_INT) :: text_fallback
  END TYPE eis_deck_add_key_params

  !> Parameters for eis_deck_optimise_definition
  TYPE, BIND(C) :: eis_deck_optimise_definition_params
    INTEGER(C_INT) :: dummy
  END TYPE eis_deck_optimise_definition_params

  !> Parameters for eis_deck_visualise_definition
  TYPE, BIND(C) :: eis_deck_visualise_definition_params
    INTEGER(C_INT) :: dummy
  END TYPE eis_deck_visualise_definition_params

  !> Parameters for eis_deck_markdown_definition
  TYPE, BIND(C) :: eis_deck_markdown_definition_params
    TYPE(C_PTR) :: title
  END TYPE eis_deck_markdown_definition_params

  !> Parameters for eis_deck_get_block_name
  TYPE, BIND(C) :: eis_deck_get_block_name_params
    INTEGER(C_INT) :: dummy
  END TYPE eis_deck_get_block_name_params

  CONTAINS

  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Store a deck definition so that it can be referenced from C
  !> @param[in] definition - Definition to store
  !> @return definition_id - ID of the stored definition
  FUNCTION add_interop_definition(definition) RESULT(definition_id)

    TYPE(eis_deck_definition), POINTER, INTENT(IN) :: definition
    INTEGER :: definition_id
    TYPE(deck_definition_holder), DIMENSION(:), ALLOCATABLE :: temp
    INTEGER :: i, old_size

    IF (.NOT. ALLOCATED(interop_definitions)) ALLOCATE(interop_definitions(10))

    DO i = 1, SIZE(interop_definitions)
      IF (.NOT. interop_definitions(i)%in_use) THEN
        interop_definitions(i)%contents => definition
        interop_definitions(i)%refcount = 1
        interop_definitions(i)%in_use = .TRUE.
        definition_id = i
        RETURN
      END IF
    END DO

    old_size = SIZE(interop_definitions)
    ALLOCATE(temp(old_size * 2))
    temp(1:old_size) = interop_definitions
    CALL MOVE_ALLOC(temp, interop_definitions)
    interop_definitions(old_size + 1)%contents => definition
    interop_definitions(old_size + 1)%refcount = 1
    interop_definitions(old_size + 1)%in_use = .TRUE.
    definition_id = old_size + 1

  END FUNCTION add_interop_definition



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get a pointer to a deck definition that is referenced from C
  !> @param[in] definition_id - ID of the definition
  !> @return definition - Pointer to the definition. NULL if the ID is invalid
  FUNCTION eis_deck_get_interop_definition(definition_id) RESULT(definition)

    INTEGER, INTENT(IN) :: definition_id
    TYPE(eis_deck_definition), POINTER :: definition

    definition => NULL()
    IF (.NOT. ALLOCATED(interop_definitions)) RETURN
    IF (definition_id < 1 .OR. definition_id > SIZE(interop_definitions)) RETURN
    IF (.NOT. interop_definitions(definition_id)%in_use) RETURN
    definition => interop_definitions(definition_id)%contents

  END FUNCTION eis_deck_get_interop_definition



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_deck_create_definition to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_deck_create_definition_params_init(params) BIND(C)

    TYPE(eis_deck_create_definition_params), INTENT(OUT) :: params

    params%init_deck = C_NULL_FUNPTR
    params%start_pass = C_NULL_FUNPTR
    params%start_deck = C_NULL_FUNPTR
    params%end_deck = C_NULL_FUNPTR
    params%end_pass = C_NULL_FUNPTR
    params%final_deck = C_NULL_FUNPTR
    params%any_key_text = C_NULL_FUNPTR
    params%any_key_value = C_NULL_FUNPTR
    params%any_key_numeric_value = C_NULL_FUNPTR
    params%any_key_stack = C_NULL_FUNPTR
    params%block_remapper = C_NULL_FUNPTR
    params%on_key_success = C_NULL_FUNPTR
    params%on_key_failure = C_NULL_FUNPTR
    params%on_key_no_trigger = C_NULL_FUNPTR
    params%on_block_start = C_NULL_FUNPTR
    params%on_block_end = C_NULL_FUNPTR
    params%on_block_no_trigger = C_NULL_FUNPTR
    params%on_block_failure = C_NULL_FUNPTR
    params%on_init = C_NULL_FUNPTR
    params%on_start_pass = C_NULL_FUNPTR
    params%on_end_pass = C_NULL_FUNPTR
    params%on_final = C_NULL_FUNPTR
    params%on_generic_block_failure = C_NULL_FUNPTR
    params%interop_parser = -1
    params%text_fallback = 0
    params%no_interop = 0

  END SUBROUTINE eis_deck_create_definition_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_deck_definition_inc_ref to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_deck_definition_inc_ref_params_init(params) BIND(C)

    TYPE(eis_deck_definition_inc_ref_params), INTENT(OUT) :: params

    params%dummy = 0

  END SUBROUTINE eis_deck_definition_inc_ref_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_deck_definition_dec_ref to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_deck_definition_dec_ref_params_init(params) BIND(C)

    TYPE(eis_deck_definition_dec_ref_params), INTENT(OUT) :: params

    params%dummy = 0

  END SUBROUTINE eis_deck_definition_dec_ref_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_deck_get_root_block to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_deck_get_root_block_params_init(params) BIND(C)

    TYPE(eis_deck_get_root_block_params), INTENT(OUT) :: params

    params%dummy = 0

  END SUBROUTINE eis_deck_get_root_block_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for eis_deck_add_block to
  !> their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_deck_add_block_params_init(params) BIND(C)

    TYPE(eis_deck_add_block_params), INTENT(OUT) :: params

    params%init_block = C_NULL_FUNPTR
    params%start_pass = C_NULL_FUNPTR
    params%start_block = C_NULL_FUNPTR
    params%end_block = C_NULL_FUNPTR
    params%end_pass = C_NULL_FUNPTR
    params%final_block = C_NULL_FUNPTR
    params%any_key_text = C_NULL_FUNPTR
    params%any_key_value = C_NULL_FUNPTR
    params%any_key_numeric_value = C_NULL_FUNPTR
    params%any_key_stack = C_NULL_FUNPTR
    params%block_remapper = C_NULL_FUNPTR
    params%pass_eq = -1
    params%pass_le = -1
    params%pass_ge = -1
    params%init_flag = C_NULL_PTR
    params%i32count = C_NULL_PTR
    params%i64count = C_NULL_PTR
    params%description = C_NULL_PTR
    params%hidden = 0
    params%text_fallback = -1

  END SUBROUTINE eis_deck_add_block_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for eis_deck_add_key to
  !> their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_deck_add_key_params_init(params) BIND(C)

    TYPE(eis_deck_add_key_params), INTENT(OUT) :: params

    params%key_text_fn = C_NULL_FUNPTR
    params%key_value_fn = C_NULL_FUNPTR
    params%key_numeric_value_fn = C_NULL_FUNPTR
    params%key_stack_fn = C_NULL_FUNPTR
    params%should_key_trigger_fn = C_NULL_FUNPTR
    params%i32value = C_NULL_PTR
    params%i32len = 1
    params%i64value = C_NULL_PTR
    params%i64len = 1
    params%r32value = C_NULL_PTR
    params%r32len = 1
    params%r64value = C_NULL_PTR
    params%r64len = 1
    params%stackvalue = -1
    params%init_flag = C_NULL_PTR
    params%i32count = C_NULL_PTR
    params%i64count = C_NULL_PTR
    params%expected_params = -1
    params%pass_eq = -1
    params%pass_le = -1
    params%pass_ge = -1
    params%description = C_NULL_PTR
    params%hidden = 0
    params%text_fallback = -1

  END SUBROUTINE eis_deck_add_key_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_deck_optimise_definition to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_deck_optimise_definition_params_init(params) BIND(C)

    TYPE(eis_deck_optimise_definition_params), INTENT(OUT) :: params

    params%dummy = 0

  END SUBROUTINE eis_deck_optimise_definition_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_deck_visualise_definition to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_deck_visualise_definition_params_init(params) BIND(C)

    TYPE(eis_deck_visualise_definition_params), INTENT(OUT) :: params

    params%dummy = 0

  END SUBROUTINE eis_deck_visualise_definition_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_deck_markdown_definition to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_deck_markdown_definition_params_init(params) BIND(C)

    TYPE(eis_deck_markdown_definition_params), INTENT(OUT) :: params

    params%title = C_NULL_PTR

  END SUBROUTINE eis_deck_markdown_definition_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_deck_get_block_name to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_deck_get_block_name_params_init(params) BIND(C)

    TYPE(eis_deck_get_block_name_params), INTENT(OUT) :: params

    params%dummy = 0

  END SUBROUTINE eis_deck_get_block_name_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to create a deck definition and return a unique
  !> ID that is used in future calls
  !> @param[inout] errcode - Error code
  !> @param[in] params - Pointer to an eis_deck_create_definition_params
  !> structure. NULL gives the default parameters
  !> @return definition_id - Unique ID of the created definition
  FUNCTION eis_deck_create_definition(errcode, params) BIND(C) &
      RESULT(definition_id)

    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    INTEGER(C_INT) :: definition_id
    TYPE(eis_deck_definition), POINTER :: definition
    CLASS(eis_deck_block_definition), POINTER :: root
    INTEGER :: errcode_i
    INTEGER, POINTER :: pinterop
    PROCEDURE(block_generic_callback_c), POINTER :: c_init_deck
    PROCEDURE(block_generic_callback_c), POINTER :: c_start_pass
    PROCEDURE(block_generic_callback_c), POINTER :: c_end_pass
    PROCEDURE(block_generic_callback_c), POINTER :: c_final_deck
    PROCEDURE(block_callback_c), POINTER :: c_start_deck
    PROCEDURE(block_callback_c), POINTER :: c_end_deck
    PROCEDURE(key_text_callback_c), POINTER :: c_any_key_text
    PROCEDURE(key_value_callback_c), POINTER :: c_any_key_value
    PROCEDURE(key_numeric_value_callback_c), POINTER :: c_any_key_numeric_value
    PROCEDURE(key_stack_callback_c), POINTER :: c_any_key_stack
    PROCEDURE(block_remap_callback_c), POINTER :: c_block_remapper
    PROCEDURE(event_callback_c), POINTER :: c_on_key_success
    PROCEDURE(event_callback_c), POINTER :: c_on_key_failure
    PROCEDURE(event_callback_c), POINTER :: c_on_key_no_trigger
    PROCEDURE(event_callback_c), POINTER :: c_on_block_start
    PROCEDURE(event_callback_c), POINTER :: c_on_block_end
    PROCEDURE(event_callback_c), POINTER :: c_on_block_no_trigger
    PROCEDURE(event_callback_c), POINTER :: c_on_block_failure
    PROCEDURE(generic_event_callback_c), POINTER :: c_on_init
    PROCEDURE(generic_event_callback_c), POINTER :: c_on_start_pass
    PROCEDURE(generic_event_callback_c), POINTER :: c_on_end_pass
    PROCEDURE(generic_event_callback_c), POINTER :: c_on_final
    PROCEDURE(generic_event_callback_c), POINTER :: c_on_generic_block_failure
    TYPE(eis_deck_create_definition_params), POINTER :: p
    TYPE(eis_deck_create_definition_params), TARGET :: defaults

    errcode = INT(eis_err_none, eis_error_c)
    definition_id = -1

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_deck_create_definition_params_init(defaults)
      p => defaults
    END IF

    c_init_deck => NULL()
    c_start_pass => NULL()
    c_end_pass => NULL()
    c_final_deck => NULL()
    c_start_deck => NULL()
    c_end_deck => NULL()
    c_any_key_text => NULL()
    c_any_key_value => NULL()
    c_any_key_numeric_value => NULL()
    c_any_key_stack => NULL()
    c_block_remapper => NULL()
    c_on_key_success => NULL()
    c_on_key_failure => NULL()
    c_on_key_no_trigger => NULL()
    c_on_block_start => NULL()
    c_on_block_end => NULL()
    c_on_block_no_trigger => NULL()
    c_on_block_failure => NULL()
    c_on_init => NULL()
    c_on_start_pass => NULL()
    c_on_end_pass => NULL()
    c_on_final => NULL()
    c_on_generic_block_failure => NULL()
    IF (C_ASSOCIATED(p%init_deck)) &
        CALL C_F_PROCPOINTER(p%init_deck, c_init_deck)
    IF (C_ASSOCIATED(p%start_pass)) &
        CALL C_F_PROCPOINTER(p%start_pass, c_start_pass)
    IF (C_ASSOCIATED(p%end_pass)) &
        CALL C_F_PROCPOINTER(p%end_pass, c_end_pass)
    IF (C_ASSOCIATED(p%final_deck)) &
        CALL C_F_PROCPOINTER(p%final_deck, c_final_deck)
    IF (C_ASSOCIATED(p%start_deck)) &
        CALL C_F_PROCPOINTER(p%start_deck, c_start_deck)
    IF (C_ASSOCIATED(p%end_deck)) &
        CALL C_F_PROCPOINTER(p%end_deck, c_end_deck)
    IF (C_ASSOCIATED(p%any_key_text)) &
        CALL C_F_PROCPOINTER(p%any_key_text, c_any_key_text)
    IF (C_ASSOCIATED(p%any_key_value)) &
        CALL C_F_PROCPOINTER(p%any_key_value, c_any_key_value)
    IF (C_ASSOCIATED(p%any_key_numeric_value)) &
        CALL C_F_PROCPOINTER(p%any_key_numeric_value, c_any_key_numeric_value)
    IF (C_ASSOCIATED(p%any_key_stack)) &
        CALL C_F_PROCPOINTER(p%any_key_stack, c_any_key_stack)
    IF (C_ASSOCIATED(p%block_remapper)) &
        CALL C_F_PROCPOINTER(p%block_remapper, c_block_remapper)
    IF (C_ASSOCIATED(p%on_key_success)) &
        CALL C_F_PROCPOINTER(p%on_key_success, c_on_key_success)
    IF (C_ASSOCIATED(p%on_key_failure)) &
        CALL C_F_PROCPOINTER(p%on_key_failure, c_on_key_failure)
    IF (C_ASSOCIATED(p%on_key_no_trigger)) &
        CALL C_F_PROCPOINTER(p%on_key_no_trigger, c_on_key_no_trigger)
    IF (C_ASSOCIATED(p%on_block_start)) &
        CALL C_F_PROCPOINTER(p%on_block_start, c_on_block_start)
    IF (C_ASSOCIATED(p%on_block_end)) &
        CALL C_F_PROCPOINTER(p%on_block_end, c_on_block_end)
    IF (C_ASSOCIATED(p%on_block_no_trigger)) &
        CALL C_F_PROCPOINTER(p%on_block_no_trigger, c_on_block_no_trigger)
    IF (C_ASSOCIATED(p%on_block_failure)) &
        CALL C_F_PROCPOINTER(p%on_block_failure, c_on_block_failure)
    IF (C_ASSOCIATED(p%on_init)) &
        CALL C_F_PROCPOINTER(p%on_init, c_on_init)
    IF (C_ASSOCIATED(p%on_start_pass)) &
        CALL C_F_PROCPOINTER(p%on_start_pass, c_on_start_pass)
    IF (C_ASSOCIATED(p%on_end_pass)) &
        CALL C_F_PROCPOINTER(p%on_end_pass, c_on_end_pass)
    IF (C_ASSOCIATED(p%on_final)) &
        CALL C_F_PROCPOINTER(p%on_final, c_on_final)
    IF (C_ASSOCIATED(p%on_generic_block_failure)) &
        CALL C_F_PROCPOINTER(p%on_generic_block_failure, &
        c_on_generic_block_failure)

    pinterop => NULL()
    IF (p%interop_parser > 0) pinterop => p%interop_parser

    ALLOCATE(definition)
    errcode_i = eis_err_none
    root => definition%init(errcode_i, c_init_deck = c_init_deck, &
        c_start_pass = c_start_pass, c_start_deck = c_start_deck, &
        c_end_deck = c_end_deck, c_end_pass = c_end_pass, &
        c_final_deck = c_final_deck, c_any_key_text = c_any_key_text, &
        c_any_key_value = c_any_key_value, &
        c_any_key_numeric_value = c_any_key_numeric_value, &
        c_any_key_stack = c_any_key_stack, c_block_remapper = c_block_remapper, &
        c_on_key_success = c_on_key_success, &
        c_on_key_failure = c_on_key_failure, &
        c_on_key_no_trigger = c_on_key_no_trigger, &
        c_on_block_start = c_on_block_start, c_on_block_end = c_on_block_end, &
        c_on_block_no_trigger = c_on_block_no_trigger, &
        c_on_block_failure = c_on_block_failure, c_on_init = c_on_init, &
        c_on_start_pass = c_on_start_pass, c_on_end_pass = c_on_end_pass, &
        c_on_final = c_on_final, &
        c_on_generic_block_failure = c_on_generic_block_failure, &
        interop_parser = pinterop, text_fallback = (p%text_fallback /= 0), &
        no_interop = (p%no_interop /= 0))
    errcode = IOR(errcode, INT(errcode_i, eis_error_c))
    definition_id = add_interop_definition(definition)

  END FUNCTION eis_deck_create_definition



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Increment the reference count for a deck definition
  !> @param[in] definition_id - ID of the definition
  !> @param[in] params - Pointer to an eis_deck_definition_inc_ref_params
  !> structure. NULL gives the default parameters. Currently unused
  SUBROUTINE eis_deck_definition_inc_ref(definition_id, params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: definition_id
    TYPE(C_PTR), VALUE, INTENT(IN) :: params

    IF (C_ASSOCIATED(params)) CONTINUE

    IF (ASSOCIATED(eis_deck_get_interop_definition(definition_id))) THEN
      interop_definitions(definition_id)%refcount &
          = interop_definitions(definition_id)%refcount + 1
    END IF

  END SUBROUTINE eis_deck_definition_inc_ref



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Decrement the reference count for a deck definition. When the reference
  !> count reaches zero the definition is deallocated
  !> @param[in] definition_id - ID of the definition
  !> @param[in] params - Pointer to an eis_deck_definition_dec_ref_params
  !> structure. NULL gives the default parameters. Currently unused
  SUBROUTINE eis_deck_definition_dec_ref(definition_id, params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: definition_id
    TYPE(C_PTR), VALUE, INTENT(IN) :: params

    IF (C_ASSOCIATED(params)) CONTINUE

    IF (.NOT. ASSOCIATED(eis_deck_get_interop_definition(definition_id))) RETURN

    interop_definitions(definition_id)%refcount = MAX(&
        interop_definitions(definition_id)%refcount - 1, 0)
    IF (interop_definitions(definition_id)%refcount == 0) THEN
      DEALLOCATE(interop_definitions(definition_id)%contents)
      interop_definitions(definition_id)%contents => NULL()
      interop_definitions(definition_id)%in_use = .FALSE.
    END IF

  END SUBROUTINE eis_deck_definition_dec_ref



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to get the ID of the root block of a deck
  !> definition
  !> @param[in] definition_id - ID of the definition
  !> @param[inout] errcode - Error code
  !> @param[in] params - Pointer to an eis_deck_get_root_block_params
  !> structure. NULL gives the default parameters. Currently unused
  !> @return block_id - ID of the root block. -1 if the definition is invalid
  FUNCTION eis_deck_get_root_block(definition_id, errcode, params) BIND(C) &
      RESULT(block_id)

    INTEGER(C_INT), VALUE, INTENT(IN) :: definition_id
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    INTEGER(C_INT) :: block_id
    TYPE(eis_deck_definition), POINTER :: definition
    CLASS(eis_deck_block_definition), POINTER :: root

    IF (C_ASSOCIATED(params)) CONTINUE

    errcode = INT(eis_err_none, eis_error_c)
    block_id = -1
    definition => eis_deck_get_interop_definition(definition_id)
    IF (.NOT. ASSOCIATED(definition)) THEN
      errcode = IOR(errcode, INT(eis_err_bad_value, eis_error_c))
      RETURN
    END IF

    root => definition%get_root()
    IF (ASSOCIATED(root)) block_id = root%id

  END FUNCTION eis_deck_get_root_block



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to add a block to a deck definition
  !> @param[in] definition_id - ID of the definition
  !> @param[in] parent_block_id - ID of the block to add the new block to
  !> @param[in] name - Name of the new block
  !> @param[inout] errcode - Error code
  !> @param[in] params - Pointer to an eis_deck_add_block_params structure.
  !> NULL gives the default parameters
  !> @return block_id - ID of the new block. -1 if the block was not added
  FUNCTION eis_deck_add_block(definition_id, parent_block_id, name, errcode, &
      params) BIND(C) RESULT(block_id)

    INTEGER(C_INT), VALUE, INTENT(IN) :: definition_id
    INTEGER(C_INT), VALUE, INTENT(IN) :: parent_block_id
    TYPE(C_PTR), VALUE, INTENT(IN) :: name
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    INTEGER(C_INT) :: block_id
    TYPE(eis_deck_definition), POINTER :: definition
    CLASS(eis_deck_block_definition), POINTER :: parent, new_block
    CHARACTER(LEN=:), ALLOCATABLE :: name_f
    CHARACTER(LEN=:), ALLOCATABLE, TARGET :: dstring
    CHARACTER(LEN=:), POINTER :: pdesc
    INTEGER, POINTER :: ppass_eq, ppass_le, ppass_ge
    LOGICAL, TARGET :: fallback
    LOGICAL, POINTER :: pfallback
    INTEGER(C_INT), POINTER :: pc_init_flag
    INTEGER(INT32), POINTER :: pi32count
    INTEGER(INT64), POINTER :: pi64count
    PROCEDURE(block_generic_callback_c), POINTER :: c_init_block
    PROCEDURE(block_generic_callback_c), POINTER :: c_start_pass
    PROCEDURE(block_generic_callback_c), POINTER :: c_end_pass
    PROCEDURE(block_generic_callback_c), POINTER :: c_final_block
    PROCEDURE(block_callback_c), POINTER :: c_start_block
    PROCEDURE(block_callback_c), POINTER :: c_end_block
    PROCEDURE(key_text_callback_c), POINTER :: c_any_key_text
    PROCEDURE(key_value_callback_c), POINTER :: c_any_key_value
    PROCEDURE(key_numeric_value_callback_c), POINTER :: c_any_key_numeric_value
    PROCEDURE(key_stack_callback_c), POINTER :: c_any_key_stack
    PROCEDURE(block_remap_callback_c), POINTER :: c_block_remapper
    TYPE(eis_deck_add_block_params), POINTER :: p
    TYPE(eis_deck_add_block_params), TARGET :: defaults

    errcode = INT(eis_err_none, eis_error_c)
    block_id = -1

    definition => eis_deck_get_interop_definition(definition_id)
    IF (ASSOCIATED(definition)) THEN
      parent => definition%get_block(parent_block_id)
    ELSE
      parent => NULL()
    END IF
    IF (.NOT. ASSOCIATED(parent) .OR. .NOT. C_ASSOCIATED(name)) THEN
      errcode = IOR(errcode, INT(eis_err_bad_value, eis_error_c))
      RETURN
    END IF

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_deck_add_block_params_init(defaults)
      p => defaults
    END IF

    c_init_block => NULL()
    c_start_pass => NULL()
    c_end_pass => NULL()
    c_final_block => NULL()
    c_start_block => NULL()
    c_end_block => NULL()
    c_any_key_text => NULL()
    c_any_key_value => NULL()
    c_any_key_numeric_value => NULL()
    c_any_key_stack => NULL()
    c_block_remapper => NULL()
    IF (C_ASSOCIATED(p%init_block)) &
        CALL C_F_PROCPOINTER(p%init_block, c_init_block)
    IF (C_ASSOCIATED(p%start_pass)) &
        CALL C_F_PROCPOINTER(p%start_pass, c_start_pass)
    IF (C_ASSOCIATED(p%end_pass)) &
        CALL C_F_PROCPOINTER(p%end_pass, c_end_pass)
    IF (C_ASSOCIATED(p%final_block)) &
        CALL C_F_PROCPOINTER(p%final_block, c_final_block)
    IF (C_ASSOCIATED(p%start_block)) &
        CALL C_F_PROCPOINTER(p%start_block, c_start_block)
    IF (C_ASSOCIATED(p%end_block)) &
        CALL C_F_PROCPOINTER(p%end_block, c_end_block)
    IF (C_ASSOCIATED(p%any_key_text)) &
        CALL C_F_PROCPOINTER(p%any_key_text, c_any_key_text)
    IF (C_ASSOCIATED(p%any_key_value)) &
        CALL C_F_PROCPOINTER(p%any_key_value, c_any_key_value)
    IF (C_ASSOCIATED(p%any_key_numeric_value)) &
        CALL C_F_PROCPOINTER(p%any_key_numeric_value, c_any_key_numeric_value)
    IF (C_ASSOCIATED(p%any_key_stack)) &
        CALL C_F_PROCPOINTER(p%any_key_stack, c_any_key_stack)
    IF (C_ASSOCIATED(p%block_remapper)) &
        CALL C_F_PROCPOINTER(p%block_remapper, c_block_remapper)

    ppass_eq => NULL()
    ppass_le => NULL()
    ppass_ge => NULL()
    IF (p%pass_eq >= 0) ppass_eq => p%pass_eq
    IF (p%pass_le >= 0) ppass_le => p%pass_le
    IF (p%pass_ge >= 0) ppass_ge => p%pass_ge

    pfallback => NULL()
    IF (p%text_fallback >= 0) THEN
      fallback = (p%text_fallback /= 0)
      pfallback => fallback
    END IF

    pc_init_flag => NULL()
    pi32count => NULL()
    pi64count => NULL()
    IF (C_ASSOCIATED(p%init_flag)) CALL C_F_POINTER(p%init_flag, pc_init_flag)
    IF (C_ASSOCIATED(p%i32count)) CALL C_F_POINTER(p%i32count, pi32count)
    IF (C_ASSOCIATED(p%i64count)) CALL C_F_POINTER(p%i64count, pi64count)

    pdesc => NULL()
    IF (C_ASSOCIATED(p%description)) THEN
      CALL eis_c_f_string(p%description, dstring)
      pdesc => dstring
    END IF

    CALL eis_c_f_string(name, name_f)
    new_block => parent%add_block(name_f, c_init_block = c_init_block, &
        c_start_pass = c_start_pass, c_start_block = c_start_block, &
        c_end_block = c_end_block, c_end_pass = c_end_pass, &
        c_final_block = c_final_block, c_any_key_text = c_any_key_text, &
        c_any_key_value = c_any_key_value, &
        c_any_key_numeric_value = c_any_key_numeric_value, &
        c_any_key_stack = c_any_key_stack, c_block_remapper = c_block_remapper, &
        pass_eq = ppass_eq, pass_le = ppass_le, pass_ge = ppass_ge, &
        c_init_flag = pc_init_flag, i32count = pi32count, &
        i64count = pi64count, &
        description = pdesc, hidden = (p%hidden /= 0), &
        text_fallback = pfallback)
    IF (ASSOCIATED(new_block)) block_id = new_block%id

  END FUNCTION eis_deck_add_block



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to add a key to a block of a deck definition
  !> @param[in] definition_id - ID of the definition
  !> @param[in] block_id - ID of the block to add the key to
  !> @param[in] name - Name of the new key
  !> @param[inout] errcode - Error code
  !> @param[in] params - Pointer to an eis_deck_add_key_params structure. NULL
  !> gives the default parameters
  SUBROUTINE eis_deck_add_key(definition_id, block_id, name, errcode, params) &
      BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: definition_id
    INTEGER(C_INT), VALUE, INTENT(IN) :: block_id
    TYPE(C_PTR), VALUE, INTENT(IN) :: name
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    TYPE(eis_deck_definition), POINTER :: definition
    CLASS(eis_deck_block_definition), POINTER :: block
    CHARACTER(LEN=:), ALLOCATABLE :: name_f
    CHARACTER(LEN=:), ALLOCATABLE, TARGET :: dstring
    CHARACTER(LEN=:), POINTER :: pdesc
    INTEGER, POINTER :: ppass_eq, ppass_le, ppass_ge
    LOGICAL, TARGET :: fallback
    LOGICAL, POINTER :: pfallback
    INTEGER(C_INT), POINTER :: pc_init_flag
    INTEGER(INT32), POINTER :: pi32count
    INTEGER(INT64), POINTER :: pi64count
    PROCEDURE(key_text_callback_c), POINTER :: c_key_text_fn
    PROCEDURE(key_value_callback_c), POINTER :: c_key_value_fn
    PROCEDURE(key_numeric_value_callback_c), POINTER :: c_key_numeric_value_fn
    PROCEDURE(key_stack_callback_c), POINTER :: c_key_stack_fn
    PROCEDURE(should_key_trigger_callback_c), POINTER :: &
        c_should_key_trigger_fn
    TYPE(eis_deck_add_key_params), POINTER :: p
    TYPE(eis_deck_add_key_params), TARGET :: defaults

    errcode = INT(eis_err_none, eis_error_c)

    definition => eis_deck_get_interop_definition(definition_id)
    IF (ASSOCIATED(definition)) THEN
      block => definition%get_block(block_id)
    ELSE
      block => NULL()
    END IF
    IF (.NOT. ASSOCIATED(block) .OR. .NOT. C_ASSOCIATED(name)) THEN
      errcode = IOR(errcode, INT(eis_err_bad_value, eis_error_c))
      RETURN
    END IF

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_deck_add_key_params_init(defaults)
      p => defaults
    END IF

    c_key_text_fn => NULL()
    c_key_value_fn => NULL()
    c_key_numeric_value_fn => NULL()
    c_key_stack_fn => NULL()
    c_should_key_trigger_fn => NULL()
    IF (C_ASSOCIATED(p%key_text_fn)) &
        CALL C_F_PROCPOINTER(p%key_text_fn, c_key_text_fn)
    IF (C_ASSOCIATED(p%key_value_fn)) &
        CALL C_F_PROCPOINTER(p%key_value_fn, c_key_value_fn)
    IF (C_ASSOCIATED(p%key_numeric_value_fn)) &
        CALL C_F_PROCPOINTER(p%key_numeric_value_fn, c_key_numeric_value_fn)
    IF (C_ASSOCIATED(p%key_stack_fn)) &
        CALL C_F_PROCPOINTER(p%key_stack_fn, c_key_stack_fn)
    IF (C_ASSOCIATED(p%should_key_trigger_fn)) &
        CALL C_F_PROCPOINTER(p%should_key_trigger_fn, c_should_key_trigger_fn)

    ppass_eq => NULL()
    ppass_le => NULL()
    ppass_ge => NULL()
    IF (p%pass_eq >= 0) ppass_eq => p%pass_eq
    IF (p%pass_le >= 0) ppass_le => p%pass_le
    IF (p%pass_ge >= 0) ppass_ge => p%pass_ge

    pfallback => NULL()
    IF (p%text_fallback >= 0) THEN
      fallback = (p%text_fallback /= 0)
      pfallback => fallback
    END IF

    pc_init_flag => NULL()
    pi32count => NULL()
    pi64count => NULL()
    IF (C_ASSOCIATED(p%init_flag)) CALL C_F_POINTER(p%init_flag, pc_init_flag)
    IF (C_ASSOCIATED(p%i32count)) CALL C_F_POINTER(p%i32count, pi32count)
    IF (C_ASSOCIATED(p%i64count)) CALL C_F_POINTER(p%i64count, pi64count)

    pdesc => NULL()
    IF (C_ASSOCIATED(p%description)) THEN
      CALL eis_c_f_string(p%description, dstring)
      pdesc => dstring
    END IF

    CALL eis_c_f_string(name, name_f)
    CALL block%add_key(name_f, c_key_text_fn = c_key_text_fn, &
        c_key_value_fn = c_key_value_fn, &
        c_key_numeric_value_fn = c_key_numeric_value_fn, &
        c_key_stack_fn = c_key_stack_fn, &
        c_should_key_trigger_fn = c_should_key_trigger_fn, &
        c_i32value = p%i32value, c_i32len = INT(p%i32len), &
        c_i64value = p%i64value, c_i64len = INT(p%i64len), &
        c_r32value = p%r32value, c_r32len = INT(p%r32len), &
        c_r64value = p%r64value, c_r64len = INT(p%r64len), &
        c_stackvalue = INT(p%stackvalue), c_init_flag = pc_init_flag, &
        i32count = pi32count, i64count = pi64count, &
        expected_params = INT(p%expected_params), pass_eq = ppass_eq, &
        pass_le = ppass_le, pass_ge = ppass_ge, description = pdesc, &
        hidden = (p%hidden /= 0), text_fallback = pfallback)

  END SUBROUTINE eis_deck_add_key



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to optimise a deck definition for repeated use
  !> @param[in] definition_id - ID of the definition
  !> @param[inout] errcode - Error code
  !> @param[in] params - Pointer to an eis_deck_optimise_definition_params
  !> structure. NULL gives the default parameters. Currently unused
  SUBROUTINE eis_deck_optimise_definition(definition_id, errcode, params) &
      BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: definition_id
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    TYPE(eis_deck_definition), POINTER :: definition

    IF (C_ASSOCIATED(params)) CONTINUE

    errcode = INT(eis_err_none, eis_error_c)
    definition => eis_deck_get_interop_definition(definition_id)
    IF (.NOT. ASSOCIATED(definition)) THEN
      errcode = IOR(errcode, INT(eis_err_bad_value, eis_error_c))
      RETURN
    END IF

    CALL definition%optimise()

  END SUBROUTINE eis_deck_optimise_definition



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to get a graphviz dot representation of the
  !> structure of a deck definition
  !> @param[in] definition_id - ID of the definition
  !> @param[in] text_length - Length of the buffer that "text" points to
  !> @param[inout] text - Pointer to string to be populated. Passing NULL
  !> is acceptable to only probe for the required length of string
  !> @param[in] params - Pointer to an eis_deck_visualise_definition_params
  !> structure. NULL gives the default parameters. Currently unused
  !> @return eis_deck_visualise_definition - Number of characters needed to hold
  !> the representation. -1 if the definition is invalid
  FUNCTION eis_deck_visualise_definition(definition_id, text_length, text, &
      params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: definition_id
    INTEGER(C_INT), VALUE, INTENT(IN) :: text_length
    TYPE(C_PTR), VALUE, INTENT(IN) :: text
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    INTEGER(C_INT) :: eis_deck_visualise_definition
    TYPE(eis_deck_definition), POINTER :: definition
    CHARACTER(LEN=:), ALLOCATABLE :: fstr

    IF (C_ASSOCIATED(params)) CONTINUE

    eis_deck_visualise_definition = -1
    definition => eis_deck_get_interop_definition(definition_id)
    IF (.NOT. ASSOCIATED(definition)) RETURN

    CALL definition%visualise(fstr)
    IF (.NOT. ALLOCATED(fstr)) RETURN
    eis_deck_visualise_definition = LEN(fstr)
    IF (C_ASSOCIATED(text)) CALL eis_f_c_string(fstr, text_length, text)

  END FUNCTION eis_deck_visualise_definition



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to get a markdown description of a deck
  !> definition
  !> @param[in] definition_id - ID of the definition
  !> @param[in] text_length - Length of the buffer that "text" points to
  !> @param[inout] text - Pointer to string to be populated. Passing NULL
  !> is acceptable to only probe for the required length of string
  !> @param[in] params - Pointer to an eis_deck_markdown_definition_params
  !> structure. NULL gives the default parameters
  !> @return eis_deck_markdown_definition - Number of characters needed to hold
  !> the description. -1 if the definition is invalid
  FUNCTION eis_deck_markdown_definition(definition_id, text_length, text, &
      params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: definition_id
    INTEGER(C_INT), VALUE, INTENT(IN) :: text_length
    TYPE(C_PTR), VALUE, INTENT(IN) :: text
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    INTEGER(C_INT) :: eis_deck_markdown_definition
    TYPE(eis_deck_definition), POINTER :: definition
    CHARACTER(LEN=:), ALLOCATABLE :: fstr
    CHARACTER(LEN=:), ALLOCATABLE, TARGET :: title_f
    CHARACTER(LEN=:), POINTER :: ptitle
    TYPE(eis_deck_markdown_definition_params), POINTER :: p
    TYPE(eis_deck_markdown_definition_params), TARGET :: defaults

    eis_deck_markdown_definition = -1
    definition => eis_deck_get_interop_definition(definition_id)
    IF (.NOT. ASSOCIATED(definition)) RETURN

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_deck_markdown_definition_params_init(defaults)
      p => defaults
    END IF

    ptitle => NULL()
    IF (C_ASSOCIATED(p%title)) THEN
      CALL eis_c_f_string(p%title, title_f)
      ptitle => title_f
    END IF

    CALL definition%get_block_structure_as_markdown(fstr, title = ptitle)
    IF (.NOT. ALLOCATED(fstr)) RETURN
    eis_deck_markdown_definition = LEN(fstr)
    IF (C_ASSOCIATED(text)) CALL eis_f_c_string(fstr, text_length, text)

  END FUNCTION eis_deck_markdown_definition



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to get the name of a block in a deck definition
  !> @param[in] definition_id - ID of the definition
  !> @param[in] block_id - ID of the block
  !> @param[in] text_length - Length of the buffer that "text" points to
  !> @param[inout] text - Pointer to string to be populated. Passing NULL
  !> is acceptable to only probe for the required length of string
  !> @param[in] params - Pointer to an eis_deck_get_block_name_params
  !> structure. NULL gives the default parameters. Currently unused
  !> @return eis_deck_get_block_name - Number of characters needed to hold the
  !> name. -1 if the definition or block is invalid
  FUNCTION eis_deck_get_block_name(definition_id, block_id, text_length, text, &
      params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: definition_id
    INTEGER(C_INT), VALUE, INTENT(IN) :: block_id
    INTEGER(C_INT), VALUE, INTENT(IN) :: text_length
    TYPE(C_PTR), VALUE, INTENT(IN) :: text
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    INTEGER(C_INT) :: eis_deck_get_block_name
    TYPE(eis_deck_definition), POINTER :: definition
    CHARACTER(LEN=:), ALLOCATABLE :: fstr

    IF (C_ASSOCIATED(params)) CONTINUE

    eis_deck_get_block_name = -1
    definition => eis_deck_get_interop_definition(definition_id)
    IF (.NOT. ASSOCIATED(definition)) RETURN

    CALL definition%get_block_name(block_id, fstr)
    IF (.NOT. ALLOCATED(fstr)) RETURN
    eis_deck_get_block_name = LEN(fstr)
    IF (C_ASSOCIATED(text)) CALL eis_f_c_string(fstr, text_length, text)

  END FUNCTION eis_deck_get_block_name

END MODULE eis_deck_definition_interop
#else
#error Unsupported EIS_C_INTEROP_VERSION
#endif
