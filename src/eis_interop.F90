MODULE eis_parser_interop

  USE, INTRINSIC :: ISO_C_BINDING
  USE eis_header
  USE eis_parser_mod
  USE eis_utils
  IMPLICIT NONE

  CONTAINS

  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Create a new interoperable parser
  !> @return create_new_parser
  FUNCTION create_new_parser()
    TYPE(eis_parser), POINTER :: new
    INTEGER :: create_new_parser !< Index of returned parser

    ALLOCATE(new)
    create_new_parser = eis_add_interop_parser(new, holds = .TRUE.)

  END FUNCTION create_new_parser



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Create a new interoperable stack
  !> @param[in] parser_id
  !> @return create_new_parser
  FUNCTION create_new_stack(parser_id)
    INTEGER, INTENT(IN) :: parser_id !< ID of parser to associate the stack with
    INTEGER :: create_new_stack !< Index of created stack
    TYPE(eis_stack), POINTER :: new

    ALLOCATE(new)
    create_new_stack = eis_add_interop_stack(new, parser_id, holds = .TRUE.)

  END FUNCTION create_new_stack



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to create a parser and return a unique id that
  !> is used in future calls to the parser system
  !> @param[in] should_simplify - Should this parser auto-simplify? True if != 0
  !> @param[in] should_minify - Should the parser auto-minify? True if !=0
  !> @param[in] no_import - Should the parser suppress import of namespaces.
  !> True if != 0
  !> @param[in] physics - Which if any physics modules should be automatically
  !> imported into the global namespace
  !> @param language - Which language should error messages be reported in 
  !> @return parser_id - Unique ID of the created parser
  FUNCTION eis_create_parser(should_simplify, should_minify, no_import, &
      physics, language) BIND(C) RESULT(parser_id)

   INTEGER(C_INT), VALUE, INTENT(IN) :: should_simplify, should_minify
   INTEGER(C_INT), VALUE, INTENT(IN) :: no_import, physics, language
   INTEGER(C_INT) :: parser_id

   parser_id = create_new_parser()
   CALL interop_parsers(parser_id)%contents%init(&
       should_simplify = (should_simplify /= 0), &
       should_minify = (should_minify /= 0), &
       no_import = (no_import /= 0), &
       physics = INT(physics), &
       language = INT(language))

  END FUNCTION eis_create_parser



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to create a stack associated with a parser
  !> this stack can then be evaluated by ID as required
  !> @param[in] parser_id - ID of parser to associate the stack with
  !> @param[in] expression - String to evaluate to stack
  !> @param[inout] capbits - Capability bits returned from parsing the stack
  !> @param[inout] errcode - Error code
  !> @return stack_id - Unique ID of the created parser
  FUNCTION eis_create_stack(parser_id, expression, cap_bits, errcode) &
      BIND(C) RESULT(stack_id)

    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    TYPE(C_PTR), VALUE :: expression
    INTEGER(eis_bitmask_c), INTENT(INOUT) :: cap_bits
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    INTEGER(C_INT) :: stack_id
    CHARACTER(LEN=:), ALLOCATABLE :: fstring
    INTEGER(eis_error) :: f_errcode

    IF (parser_id < 1 .OR. parser_id > interop_parser_count) THEN
      stack_id = -1
      RETURN
    END IF
    
    stack_id = create_new_stack(INT(parser_id))
    CALL c_f_string(expression, fstring)
    f_errcode = 0_eis_error
    CALL interop_stacks(stack_id)%parser%tokenize(fstring, &
        interop_stacks(stack_id)%contents, f_errcode)
    cap_bits = IOR(errcode, INT(interop_stacks(stack_id)%contents%cap_bits, &
        eis_bitmask_c))
    errcode = IOR(errcode, INT(f_errcode, eis_error_c))

  END FUNCTION eis_create_stack



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to add a function associated with a name that can
  !> be called from a parser expression
  !> @param[in] parser_id - ID of parser to add the function to
  !> @param[in] name - Name to associate function with in expression
  !> @param[in] fn - Pointer to function to be called when the name is
  !> encountered
  !> @param[in] cap_bits - Capability bits
  !> @param[in] expected params - Expected number of parameters for the function
  !> if <0 then function will be variadic
  !> @param[in] can_simplify - Should the function be simplified through
  !> in general this should be 1 unless the function has different effects for
  !> different host code parameters
  !> @param[in] global - If 1 add the function to all parsers, if 0 only the
  !> specified parser
  !> @param[inout] errcode - Error code
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

    IF (parser_id < 1 .OR. parser_id > interop_parser_count) RETURN
    parser => interop_parsers(parser_id)%contents

    CALL c_f_string(name, fstring)
    f_errcode = eis_err_none
    f_bitmask = INT(cap_bits, eis_bitmask)
    CALL parser%add_function(fstring, fn, errcode, f_bitmask, expected_params, &
        can_simplify /= 0, .FALSE., global /= 0)
    errcode = IOR(errcode, INT(f_errcode, eis_error_c))
    DEALLOCATE(fstring)

  END SUBROUTINE eis_add_function



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to add a variable associated with a name that can
  !> be called from a parser expression
  !> @param[in] parser_id - ID of parser to add the variable to
  !> @param[in] name - Name to associate variable with in expression
  !> @param[in] fn - Pointer to function to be called when the name is
  !> encountered
  !> @param[in] cap_bits - Capability bits
  !> @param[in] can_simplify - Should the variable be simplified
  !> in general this should be 1 unless the variable has different values for
  !> different host code parameters
  !> @param[in] global - If 1 add the function to all parsers, if 0 only the
  !> specified parser
  !> @param[inout] errcode - Error code
  SUBROUTINE eis_add_variable(parser_id, name, fn, cap_bits, &
      can_simplify, global, errcode) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    TYPE(C_PTR), VALUE :: name
    PROCEDURE(parser_eval_fn) :: fn
    INTEGER(eis_bitmask_c), VALUE,  INTENT(IN) :: cap_bits
    INTEGER(C_INT), VALUE, INTENT(IN) :: can_simplify
    INTEGER(C_INT), VALUE, INTENT(IN) :: global
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(eis_parser), POINTER :: parser
    CHARACTER(LEN=:), ALLOCATABLE :: fstring
    INTEGER(eis_error) :: f_errcode
    INTEGER(eis_bitmask) :: f_bitmask

    IF (parser_id < 1 .OR. parser_id > interop_parser_count) RETURN
    parser => interop_parsers(parser_id)%contents

    CALL c_f_string(name, fstring)
    f_errcode = eis_err_none
    f_bitmask = INT(cap_bits, eis_bitmask)
    CALL parser%add_variable(fstring, fn, errcode, f_bitmask, &
        can_simplify /= 0, .FALSE., global /= 0)
    errcode = IOR(errcode, INT(f_errcode, eis_error_c))
    DEALLOCATE(fstring)

  END SUBROUTINE eis_add_variable



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to add a constant associated with a name that can
  !> be called from a parser expression
  !> @param[in] parser_id - ID of parser to add the variable to
  !> @param[in] name - Name to associate variable with in expression
  !> @param[in] value - Value to associate with name
  !> @param[in] cap_bits - Capability bits
  !> @param[in] can_simplify - Should the constant be simplified. 
  !> Generally always 1
  !> @param[in] global - If 1 add the function to all parsers, if 0 only the
  !> specified parser
  !> @param[inout] errcode - Error code
  SUBROUTINE eis_add_constant(parser_id, name, value, cap_bits, &
      can_simplify, global, errcode) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    TYPE(C_PTR), VALUE :: name
    REAL(eis_num_c), VALUE :: value
    INTEGER(eis_bitmask_c), VALUE,  INTENT(IN) :: cap_bits
    INTEGER(C_INT), VALUE, INTENT(IN) :: can_simplify
    INTEGER(C_INT), VALUE, INTENT(IN) :: global
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(eis_parser), POINTER :: parser
    CHARACTER(LEN=:), ALLOCATABLE :: fstring
    INTEGER(eis_error) :: f_errcode
    INTEGER(eis_bitmask) :: f_bitmask

    IF (parser_id < 1 .OR. parser_id > interop_parser_count) RETURN
    parser => interop_parsers(parser_id)%contents

    CALL c_f_string(name, fstring)
    f_errcode = eis_err_none
    f_bitmask = INT(cap_bits, eis_bitmask)
    CALL parser%add_constant(fstring, value, errcode, f_bitmask, &
        can_simplify /= 0, .FALSE., global /= 0)
    errcode = IOR(errcode, INT(f_errcode, eis_error_c))
    DEALLOCATE(fstring)

  END SUBROUTINE eis_add_constant



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to evaluate a stored stack and return
  !> the restul
  !> @details
  !> Unlike the Fortran version of this function this function takes
  !> a number of expected return values and returns an error if the number of
  !> actual return values is different to this value. The number of returned
  !> results is MIN(res_len, results_from_eval).
  !> @param[in] stack_id - ID of stored stack to evaluate
  !> @param[in] res_len - Length of the expected result and number of elements
  !> of the "result" array
  !> @param[out] result - Array that will be populated with the results of the
  !> evaluation. Array must be at least res_len elements long
  !> @param[inout] errcode - Error code
  !> @param[in] user_params - Pointer to C interoperable structure that is
  !> passed to all function that are called as expression keys are evaluated
  !> @param[out] is_no_op - Used with the special "where" directive to specify
  !> that the where condition has not been satisfied and no changes should be
  !> made
  !> @return eis_evaluate_stack - Number of results actually returned
  FUNCTION eis_evaluate_stack(stack_id, res_len, result, errcode, &
      user_params, is_no_op) BIND(C)

    INTEGER(C_INT), VALUE :: stack_id, res_len
    REAL(eis_num_c), DIMENSION(res_len), INTENT(OUT) :: result
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

    IF (stack_id < 1 .OR. stack_id > interop_stack_count) THEN
      errcode = IOR(errcode, INT(eis_err_bad_stack, eis_error_c))
      is_no_op = 1_C_INT
      RETURN
    END IF

    returned_results =  interop_stacks(stack_id)%parser%evaluate(&
        interop_stacks(stack_id)%contents, f_result, f_errcode, user_params, &
        f_is_no_op)

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


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to get the number of errors caused by evaluating a
  !> stack
  !> @param[in] stack_id - ID of stored stack to evaluate
  !> @return eis_get_error_count - Number of errors in the error handler
  FUNCTION eis_get_error_count(stack_id) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: stack_id
    INTEGER(C_INT) :: eis_get_error_count
    TYPE(eis_parser), POINTER :: parser

    parser => eis_get_interop_parser(stack_id)
    eis_get_error_count = parser%get_error_count()

  END FUNCTION eis_get_error_count



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to get the error report from a stack id
  !> and an error id. The error ID is an integer between 1 and the result
  !> from eis_get_error_count
  !> @param[in] stack_id - ID of stored stack to evaluate
  !> @param[in] error_id - ID of stored error
  !> @param[in] buflen - Size of the output buffer
  !> @param[out] string_out - char* array of at least length buflen
  !> will hold the returned output buffer. Does not have a terminal
  !> newline sequence. If it is not long enough to hold the error report
  !> then it will copy as many characters as it can to fill the buffer. Always
  !> finished with a null terminator character unless buflen = 0
  !> @return eis_get_error_report - Length of the error report + 1. If you
  !> allocate a string at least this long it will be able to hold the 
  !> full error report and the null terminator character
  FUNCTION eis_get_error_report(stack_id, error_id, buflen, string_out) BIND(C)
    INTEGER(C_INT), VALUE, INTENT(IN) :: stack_id, error_id, buflen
    TYPE(C_PTR), VALUE, INTENT(IN) :: string_out
    INTEGER(C_INT) :: eis_get_error_report
    CHARACTER(LEN=:), ALLOCATABLE :: errstr
    TYPE(eis_parser), POINTER :: parser

    parser => eis_get_interop_parser(stack_id)
    CALL parser%get_error_report(error_id, errstr)
    CALL f_c_string(errstr, buflen, string_out)
    eis_get_error_report = LEN(errstr) + 1
    DEALLOCATE(errstr)

  END FUNCTION eis_get_error_report



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Increment the reference count for a stack. Use to indicate
  !> that another part of your code wants to use a given stored stack
  !> @param[in] stack_id - ID of stored stack to increment the reference of
  SUBROUTINE eis_stack_inc_ref(stack_id) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: stack_id

    IF (stack_id < 1 .OR. stack_id > interop_stack_count) THEN
      RETURN
    END IF

    interop_stacks(stack_id)%refcount = interop_stacks(stack_id)%refcount + 1

  END SUBROUTINE eis_stack_inc_ref



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Decrement the reference count for a stack. Use to indicate
  !> that another part of your code has finished using a given stack.
  !> When the reference count reaches zero the stack will be deallocated
  !> IF it was created by eis_create_stack. If it was created by a Fortran
  !> code and given to the interoperable interface then it will not be 
  !> deallocated even when the reference count reaches zero
  !> @param[in] stack_id - ID of stored stack to decrement the reference of
  SUBROUTINE eis_stack_dec_ref(stack_id) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: stack_id

    IF (stack_id < 1 .OR. stack_id > interop_stack_count) THEN
      RETURN
    END IF

    interop_stacks(stack_id)%refcount = interop_stacks(stack_id)%refcount - 1
    IF (interop_stacks(stack_id)%refcount == 0 .AND. &
        interop_stacks(stack_id)%holds_stack) THEN
      DEALLOCATE(interop_stacks(stack_id)%contents)
      interop_stacks(stack_id)%contents => NULL()
    END IF

  END SUBROUTINE eis_stack_dec_ref


END MODULE eis_parser_interop
