MODULE eis_c_interop

  USE, INTRINSIC :: ISO_C_BINDING
  USE eis_header
  USE eis_parser_mod
  USE eis_utils
  IMPLICIT NONE

  INTEGER, PARAMETER :: n_parsers_default = 4
  INTEGER, PARAMETER :: n_stacks_default = 32

  TYPE :: parser_holder
    CLASS(eis_parser), POINTER :: contents => NULL()
    CONTAINS
    FINAL :: ph_destructor
  END TYPE parser_holder

  TYPE :: stack_holder
    INTEGER :: refcount = 1
    CLASS(eis_parser), POINTER :: parser => NULL()
    CLASS(eis_stack), POINTER :: contents => NULL()
    CONTAINS
    FINAL :: sh_destructor
  END TYPE stack_holder

  INTEGER :: parser_count=0, stack_count=0
  TYPE(parser_holder), DIMENSION(:), ALLOCATABLE :: parsers
  TYPE(stack_holder), DIMENSION(:), ALLOCATABLE :: stacks

  CONTAINS

  PURE ELEMENTAL SUBROUTINE ph_destructor(this)
    TYPE(parser_holder), INTENT(INOUT) :: this
    IF (ASSOCIATED(this%contents)) DEALLOCATE(this%contents)
  END SUBROUTINE ph_destructor
  PURE ELEMENTAL SUBROUTINE sh_destructor(this)
    TYPE(stack_holder), INTENT(INOUT) :: this
    IF (ASSOCIATED(this%contents)) DEALLOCATE(this%contents)
  END SUBROUTINE sh_destructor



  FUNCTION create_new_parser()
    INTEGER :: create_new_parser
    TYPE(parser_holder), DIMENSION(:), ALLOCATABLE :: temp
    IF (.NOT. ALLOCATED(parsers)) THEN
      ALLOCATE(parsers(n_parsers_default))
      ALLOCATE(parsers(1)%contents)
      create_new_parser = 1
      parser_count = 1
    ELSE
      parser_count = parser_count + 1
      IF (parser_count > SIZE(parsers)) THEN
        ALLOCATE(temp(SIZE(parsers)*2))
        temp(1:SIZE(parsers)) = parsers
        DEALLOCATE(parsers)
        CALL MOVE_ALLOC(temp, parsers)
      END IF
      ALLOCATE(parsers(parser_count)%contents)
      create_new_parser = parser_count
    END IF
  END FUNCTION create_new_parser



  FUNCTION create_new_stack(parser_id)
    INTEGER, INTENT(IN) :: parser_id
    INTEGER :: create_new_stack
    TYPE(stack_holder), DIMENSION(:), ALLOCATABLE :: temp

    IF (.NOT. ALLOCATED(stacks)) THEN
      ALLOCATE(stacks(n_stacks_default))
      stack_count = 1
    ELSE
      stack_count = stack_count + 1
      IF (stack_count > SIZE(stacks)) THEN
        ALLOCATE(temp(SIZE(stacks)*2))
        temp(1:SIZE(stacks)) = stacks
        DEALLOCATE(stacks)
        CALL MOVE_ALLOC(temp, stacks)
      END IF
    END IF
    ALLOCATE(stacks(stack_count)%contents)
    IF (ALLOCATED(parsers)) THEN
      IF (parser_id > 0 .AND. parser_id <= parser_count) THEN
        stacks(stack_count)%parser => parsers(parser_id)%contents
      END IF
    END IF
    create_new_stack = stack_count

  END FUNCTION create_new_stack



  FUNCTION eis_create_parser(should_simplify, should_minify, no_import, &
      physics, language) BIND(C) RESULT(parser_id)

   INTEGER(C_INT), VALUE, INTENT(IN) :: should_simplify, should_minify
   INTEGER(C_INT), VALUE, INTENT(IN) :: no_import, physics, language
   INTEGER(C_INT) :: parser_id

   parser_id = create_new_parser()
   CALL parsers(parser_id)%contents%init(&
       should_simplify = (should_simplify /= 0), &
       should_minify = (should_minify /= 0), &
       no_import = (no_import /= 0), &
       physics = INT(physics), &
       language = INT(language))

  END FUNCTION eis_create_parser



  FUNCTION eis_create_stack(parser_id, expression, cap_bits, errcode) &
      BIND(C) RESULT(stack_id)

    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    TYPE(C_PTR), VALUE :: expression
    INTEGER(eis_bitmask_c), INTENT(INOUT) :: cap_bits
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    INTEGER(C_INT) :: stack_id
    CHARACTER(LEN=:), ALLOCATABLE :: fstring
    INTEGER(eis_error) :: f_errcode

    IF (parser_id < 1 .OR. parser_id > parser_count) THEN
      stack_id = -1
      RETURN
    END IF
    
    stack_id = create_new_stack(INT(parser_id))
    CALL c_f_string(expression, fstring)
    f_errcode = 0_eis_error
    CALL stacks(stack_id)%parser%tokenize(fstring, stacks(stack_id)%contents, &
        f_errcode)
    cap_bits = IOR(errcode, INT(stacks(stack_id)%contents%cap_bits, &
        eis_bitmask_c))
    errcode = IOR(errcode, INT(f_errcode, eis_error_c))

  END FUNCTION eis_create_stack



  SUBROUTINE eis_add_function(parser_id, name, fn, cap_bits, &
      expected_params, can_simplify, global, errcode) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    TYPE(C_PTR), VALUE :: name
    PROCEDURE(parser_eval_fn) :: fn
    INTEGER(eis_bitmask_c), VALUE,  INTENT(IN) :: cap_bits
    INTEGER(C_INT), VALUE, INTENT(IN) :: expected_params
    INTEGER(C_INT), VALUE, INTENT(IN) :: can_simplify
    INTEGER(C_INT), VALUE, INTENT(IN) :: global
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(eis_parser), POINTER :: parser
    CHARACTER(LEN=:), ALLOCATABLE :: fstring
    INTEGER(eis_error) :: f_errcode
    INTEGER(eis_bitmask) :: f_bitmask

    IF (parser_id < 1 .OR. parser_id > parser_count) RETURN
    parser => parsers(parser_id)%contents

    CALL c_f_string(name, fstring)
    f_errcode = eis_err_none
    f_bitmask = INT(cap_bits, eis_bitmask)
    CALL parser%add_function(fstring, fn, errcode, f_bitmask, expected_params, &
        can_simplify /= 0, .FALSE., global /= 0)
    errcode = IOR(errcode, INT(f_errcode, eis_error_c))
    DEALLOCATE(fstring)

  END SUBROUTINE eis_add_function



  FUNCTION eis_evaluate_stack(stack_id, res_len, result, errcode, &
      user_params, is_no_op) BIND(C)

    INTEGER(C_INT), VALUE :: stack_id, res_len
    REAL(eis_num_c), DIMENSION(res_len) :: result
    INTEGER(eis_error_c), INTENT(OUT) :: errcode
    TYPE(C_PTR), VALUE :: user_params
    INTEGER(C_INT), INTENT(OUT) :: is_no_op
    INTEGER(C_INT) :: eis_evaluate_stack
    REAL(eis_num), DIMENSION(:), ALLOCATABLE :: f_result
    INTEGER(eis_error) :: f_errcode
    LOGICAL :: f_is_no_op
    INTEGER :: returned_results, copied_results

    eis_evaluate_stack = -1
    f_errcode = 0

    IF (stack_id < 1 .OR. stack_id > stack_count) THEN
      errcode = IOR(errcode, INT(eis_err_bad_stack, eis_error_c))
      is_no_op = 1_C_INT
      RETURN
    END IF

    returned_results =  stacks(stack_id)%parser%evaluate(&
        stacks(stack_id)%contents, f_result, f_errcode, user_params, f_is_no_op)

    errcode = IOR(errcode, INT(f_errcode, eis_error_c))
    IF (.NOT. ALLOCATED(f_result)) RETURN
    IF (res_len < SIZE(f_result)) THEN
      errcode = IOR(errcode, INT(eis_err_extra_results, eis_error_c))
    END IF
    eis_evaluate_stack = SIZE(f_result)

    copied_results = MIN(SIZE(f_result), res_len)
    result(1:copied_results) = REAL(f_result(1:copied_results), eis_num_c)
    IF (f_is_no_op) THEN
      is_no_op = 1_C_INT
    ELSE
      is_no_op = 0_C_INT
    END IF

  END FUNCTION eis_evaluate_stack



  SUBROUTINE eis_stack_inc_ref(stack_id) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: stack_id

    IF (stack_id < 1 .OR. stack_id > stack_count) THEN
      RETURN
    END IF

    stacks(stack_id)%refcount = stacks(stack_id)%refcount + 1

  END SUBROUTINE eis_stack_inc_ref



  SUBROUTINE eis_stack_dec_ref(stack_id) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: stack_id

    IF (stack_id < 1 .OR. stack_id > stack_count) THEN
      RETURN
    END IF

    stacks(stack_id)%refcount = stacks(stack_id)%refcount - 1
    IF (stacks(stack_id)%refcount == 0) THEN
      DEALLOCATE(stacks(stack_id)%contents)
      stacks(stack_id)%contents => NULL()
    END IF

  END SUBROUTINE eis_stack_dec_ref


END MODULE eis_c_interop
