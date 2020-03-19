MODULE eis_parser_interop

  USE, INTRINSIC :: ISO_C_BINDING
  USE eis_header
  USE eis_parser_constants
  USE eis_parser_mod
  USE eis_utils
  USE eis_core_functions_mod
  IMPLICIT NONE

  PRIVATE :: if_operate, interop_functor, create_new_parser, create_new_stack

  ABSTRACT INTERFACE
    FUNCTION c_functor_function(nparams, params, host_params, bound_data, &
        status_code, errcode) BIND(C)
      IMPORT eis_num, eis_i4, C_PTR, eis_error, eis_status
      INTEGER(eis_i4), VALUE, INTENT(IN) :: nparams
      REAL(eis_num), DIMENSION(nparams), INTENT(IN) :: params
      TYPE(C_PTR), VALUE, INTENT(IN) :: host_params
      TYPE(C_PTR), VALUE, INTENT(IN) :: bound_data
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_error), INTENT(INOUT) :: errcode
      REAL(eis_num) :: parser_eval_fn
    END FUNCTION
  END INTERFACE

  TYPE, EXTENDS(eis_functor) :: interop_functor
    TYPE(C_PTR) :: bound_data = C_NULL_PTR
    PROCEDURE(c_functor_function), NOPASS, POINTER :: c_bound_function
  CONTAINS
    PROCEDURE :: operate => if_operate
  END TYPE interop_functor

  CONTAINS

  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Operate function for C functor
  !> @param[in] this
  !> @param[in] nparams
  !> @param[in] params
  !> @param[in] host_params
  !> @param[inout] status_code
  !> @param[inout] errcode
  !> @return if_operate
  FUNCTION if_operate(this, nparams, params, host_params, status_code, &
      errcode)
    CLASS(interop_functor), INTENT(INOUT) :: this
    INTEGER(eis_i4), INTENT(IN) :: nparams
    REAL(eis_num), DIMENSION(nparams), INTENT(IN) :: params
    TYPE(C_PTR), INTENT(IN) :: host_params
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    REAL(eis_num) :: if_operate
    REAL(eis_num), DIMENSION(1) :: results
    INTEGER :: result_count

    if_operate = this%c_bound_function(nparams, params, host_params, &
        this%bound_data, status_code, errcode)
  END FUNCTION if_operate



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Create a new interoperable parser
  !> @return create_new_parser
  FUNCTION create_new_parser()
    CLASS(eis_parser), POINTER :: new
    INTEGER :: create_new_parser !< Index of returned parser

    ALLOCATE(new)
    create_new_parser = eis_add_interop_parser(new, owns = .TRUE.)

  END FUNCTION create_new_parser



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Create a new interoperable stack
  !> @param[in] parser_id
  !> @return create_new_parser
  FUNCTION create_new_stack(parser_id)
    INTEGER, INTENT(IN) :: parser_id !< ID of parser to associate the stack with
    INTEGER :: create_new_stack !< Index of created stack
    CLASS(eis_stack), POINTER :: new

    ALLOCATE(eis_stack::new)
    create_new_stack = eis_add_interop_stack(new, parser_id, owns = .TRUE.)

  END FUNCTION create_new_stack



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to create a parser and return a unique id that
  !> is used in future calls to the parser system
  !> @param[inout] errcode - Error code from initialising the parser
  !> @param[in] should_simplify - Should this parser auto-simplify? True if != 0
  !> @param[in] should_minify - Should the parser auto-minify? True if !=0
  !> @param[in] no_import - Should the parser suppress import of namespaces.
  !> True if != 0
  !> @param[in] physics - Which if any physics modules should be automatically
  !> imported into the global namespace
  !> @param[in] errhandler - Integer ID for an error handler. Currently unused
  !> @return parser_id - Unique ID of the created parser
  FUNCTION eis_create_parser(errcode, should_simplify, &
      should_minify, no_import, physics, errhandler) BIND(C) RESULT(parser_id)

   INTEGER(eis_error_c), INTENT(INOUT) :: errcode
   INTEGER(C_INT), VALUE, INTENT(IN) :: should_simplify, should_minify
   INTEGER(C_INT), VALUE, INTENT(IN) :: no_import, physics
   INTEGER(C_INT), VALUE, INTENT(IN) :: errhandler
   INTEGER(C_INT) :: parser_id
   CLASS(eis_parser), POINTER :: parser

   parser_id = create_new_parser()
   parser => eis_get_interop_parser(parser_id)
   CALL parser%init(errcode, &
       should_simplify = (should_simplify /= 0), &
       should_minify = (should_minify /= 0), &
       no_import = (no_import /= 0), &
       physics = INT(physics))

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

    errcode = INT(eis_err_none, eis_error_c)

    IF (parser_id < 1 .OR. parser_id > interop_parser_count) THEN
      stack_id = -1
      RETURN
    END IF

    stack_id = create_new_stack(INT(parser_id))
    IF (C_ASSOCIATED(expression)) THEN
      CALL eis_c_f_string(expression, fstring)
      f_errcode = 0_eis_error
      CALL interop_stacks(stack_id)%parser%tokenize(fstring, &
          interop_stacks(stack_id)%contents, f_errcode)
      cap_bits = INT(interop_stacks(stack_id)%contents%cap_bits, &
          eis_bitmask_c)
      errcode = IOR(errcode, INT(f_errcode, eis_error_c))
    END IF

  END FUNCTION eis_create_stack



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to append a new expression to a stack
  !> @param[in] stack_id - ID of the stack to
  !> @param[in] expression - String to evaluate to stack
  !> @param[inout] capbits - Capability bits returned from parsing the stack
  !> @param[inout] errcode - Error code
  SUBROUTINE eis_append_stack(stack_id, expression, cap_bits, errcode) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: stack_id
    TYPE(C_PTR), VALUE :: expression
    INTEGER(eis_bitmask_c), INTENT(INOUT) :: cap_bits
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    CHARACTER(LEN=:), ALLOCATABLE :: fstring
    INTEGER(eis_error) :: f_errcode
    CLASS(eis_parser), POINTER :: parser
    CLASS(eis_stack), POINTER :: stack

    errcode = INT(eis_err_none, eis_error_c)
    CALL eis_c_f_string(expression, fstring)
    f_errcode = eis_err_none
    stack => eis_get_interop_stack(stack_id, parser = parser)
    IF (ASSOCIATED(stack) .AND. ASSOCIATED(parser)) THEN
      CALL parser%tokenize(fstring, stack, f_errcode, append = .TRUE.)
      cap_bits = INT(interop_stacks(stack_id)%contents%cap_bits, &
          eis_bitmask_c)
      errcode = IOR(errcode, INT(f_errcode, eis_error_c))
    ELSE
      IF (.NOT. ASSOCIATED(stack)) errcode = IOR(errcode, &
          INT(eis_err_bad_stack, eis_error_c))
      IF (.NOT. ASSOCIATED(parser)) errcode = IOR(errcode, &
          INT(eis_err_bad_parser, eis_error_c))
    END IF

  END SUBROUTINE eis_append_stack



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set or replace the expression in a stack with
  !> another stack
  !> @param[in] stack_id - ID of the stack to
  !> @param[in] expression - String to evaluate to stack
  !> @param[inout] capbits - Capability bits returned from parsing the stack
  !> @param[inout] errcode - Error code
  SUBROUTINE eis_populate_stack(stack_id, expression, cap_bits, errcode) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: stack_id
    TYPE(C_PTR), VALUE :: expression
    INTEGER(eis_bitmask_c), INTENT(INOUT) :: cap_bits
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    CHARACTER(LEN=:), ALLOCATABLE :: fstring
    INTEGER(eis_error) :: f_errcode
    CLASS(eis_parser), POINTER :: parser
    CLASS(eis_stack), POINTER :: stack

    errcode = INT(eis_err_none, eis_error_c)
    CALL eis_c_f_string(expression, fstring)
    f_errcode = eis_err_none
    stack => eis_get_interop_stack(stack_id, parser = parser)
    IF (ASSOCIATED(stack) .AND. ASSOCIATED(parser)) THEN
      CALL parser%tokenize(fstring, stack, f_errcode, append = .FALSE.)
      cap_bits = INT(interop_stacks(stack_id)%contents%cap_bits, &
          eis_bitmask_c)
      errcode = IOR(errcode, INT(f_errcode, eis_error_c))
    ELSE
      IF (.NOT. ASSOCIATED(stack)) errcode = IOR(errcode, &
          INT(eis_err_bad_stack, eis_error_c))
      IF (.NOT. ASSOCIATED(parser)) errcode = IOR(errcode, &
          INT(eis_err_bad_parser, eis_error_c))
    END IF

  END SUBROUTINE eis_populate_stack



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to empty a stack
  !> @param[in] stack_id - ID of the stack to
  !> @param[inout] errcode - Error code
  SUBROUTINE eis_empty_stack(stack_id, errcode) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: stack_id
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    CHARACTER(LEN=:), ALLOCATABLE :: fstring
    INTEGER(eis_error) :: f_errcode
    CLASS(eis_parser), POINTER :: parser
    CLASS(eis_stack), POINTER :: stack

    errcode = INT(eis_err_none, eis_error_c)
    stack => eis_get_interop_stack(stack_id, parser = parser)
    IF (ASSOCIATED(stack) .AND. ASSOCIATED(parser)) THEN
      CALL parser%blank_stack(stack)
    ELSE
      IF (.NOT. ASSOCIATED(stack)) errcode = IOR(errcode, &
          INT(eis_err_bad_stack, eis_error_c))
      IF (.NOT. ASSOCIATED(parser)) errcode = IOR(errcode, &
          INT(eis_err_bad_parser, eis_error_c))
    END IF

  END SUBROUTINE eis_empty_stack



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to bind a result function to a stack
  !> @param[in] stack_id - ID of the stack to
  !> @param[in] fn - Function pointer to the result function
  !> @param[inout] errcode - Error code
  !> @param[in] filename - Filename of the source of the function. Used to
  !> report errors. If this is null then error reporting will not include
  !> information about the file or line
  !> @param[in] line_number - Line number of the source of the function. Used
  !> to report errors
  !> @param[in] char_offset - Character position of the part of the line
  !> specified in line_number to report errors at
  !> @param[in] cap_bits - Capability bits for the result function
  !> @param[in] append - Whether the result function should be appended or
  !> used to replace the stack. 0 is overwrite, 1 is append
  SUBROUTINE eis_bind_result_to_stack(stack_id, fn, errcode, filename, &
      line_number, char_offset, cap_bits, append) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: stack_id
    TYPE(C_FUNPTR), VALUE, INTENT(IN) :: fn
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: filename
    INTEGER(C_INT), VALUE, INTENT(IN) :: line_number
    INTEGER(C_INT), VALUE, INTENT(IN) :: char_offset
    INTEGER(eis_bitmask_c), VALUE, INTENT(IN) :: cap_bits
    INTEGER(C_INT), VALUE, INTENT(IN) :: append
    CHARACTER(LEN=:), ALLOCATABLE :: fstring
    INTEGER(eis_error) :: f_errcode
    CLASS(eis_parser), POINTER :: parser
    CLASS(eis_stack), POINTER :: stack
    PROCEDURE(parser_result_function), POINTER :: fn_ptr

    errcode = INT(eis_err_none, eis_error_c)
    stack => eis_get_interop_stack(stack_id, parser = parser)
    CALL C_F_PROCPOINTER(fn, fn_ptr)
    IF (ASSOCIATED(stack) .AND. ASSOCIATED(parser)) THEN
      IF (C_ASSOCIATED(filename)) THEN
        CALL eis_c_f_string(filename, fstring)
        CALL parser%set_result_function(fn_ptr, stack, errcode, &
            fstring, INT(line_number), INT(char_offset), &
            INT(cap_bits, eis_bitmask), append /= 0)
      ELSE
        CALL parser%set_result_function(fn_ptr, stack, errcode, &
            cap_bits = INT(cap_bits, eis_bitmask), append = (append /= 0))
      END IF
    ELSE
      IF (.NOT. ASSOCIATED(stack)) errcode = IOR(errcode, &
          INT(eis_err_bad_stack, eis_error_c))
      IF (.NOT. ASSOCIATED(parser)) errcode = IOR(errcode, &
          INT(eis_err_bad_parser, eis_error_c))
    END IF

  END SUBROUTINE eis_bind_result_to_stack



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to add a function associated with a name that can
  !> be called from a parser expression
  !> @param[in] parser_id - ID of parser to add the function to
  !> @param[in] name - Name to associate function with in expression
  !> @param[in] fn - Pointer to function to be called when the name is
  !> encountered. If fn is NULL then the function will be an EIS deferred
  !> function
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
    TYPE(C_FUNPTR), VALUE :: fn
    INTEGER(eis_bitmask_c), VALUE,  INTENT(IN) :: cap_bits
    INTEGER(C_INT), VALUE, INTENT(IN) :: expected_params
    INTEGER(C_INT), VALUE, INTENT(IN) :: can_simplify
    INTEGER(C_INT), VALUE, INTENT(IN) :: global
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    CLASS(eis_parser), POINTER :: parser
    CHARACTER(LEN=:), ALLOCATABLE :: fstring
    INTEGER(eis_error) :: f_errcode
    INTEGER(eis_bitmask) :: f_bitmask
    PROCEDURE(parser_eval_fn), POINTER :: fn_ptr

    errcode = INT(eis_err_none, eis_error_c)

    IF (parser_id < 1 .OR. parser_id > interop_parser_count) RETURN
    parser => interop_parsers(parser_id)%contents

    IF (C_ASSOCIATED(fn)) THEN
      CALL C_F_PROCPOINTER(fn, fn_ptr)
    ELSE
      fn_ptr => eis_dummy
    END IF

    CALL eis_c_f_string(name, fstring)
    f_errcode = eis_err_none
    f_bitmask = INT(cap_bits, eis_bitmask)
    CALL parser%add_function(fstring, fn_ptr, errcode, f_bitmask, &
        expected_params, can_simplify /= 0, .NOT. C_ASSOCIATED(fn), global /= 0)
    errcode = IOR(errcode, INT(f_errcode, eis_error_c))
    DEALLOCATE(fstring)

  END SUBROUTINE eis_add_function



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to add an emplaced function
  !> @param[in] parser_id - ID of parser to add the function to
  !> @param[in] name - Name to associate function with in expression
  !> @param[in] fn - Pointer to function to be called for the emplacement
  !> action. Must not be NULL
  !> @param[in] expected params - Expected number of parameters for the function
  !> if <0 then function will be variadic
  !> @param[inout] errcode - Error code
  SUBROUTINE eis_add_emplaced_function(parser_id, name, fn, errcode, &
      expected_params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    TYPE(C_PTR), VALUE, INTENT(IN) :: name
    TYPE(C_FUNPTR), VALUE, INTENT(IN) :: fn
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    INTEGER(C_INT), VALUE, INTENT(IN) :: expected_params
    PROCEDURE(parser_late_bind_interop_fn), POINTER :: fn_ptr
    INTEGER(eis_error) :: errcode_f
    CLASS(eis_parser), POINTER :: parser
    CHARACTER(LEN=:), ALLOCATABLE :: name_f

    errcode = INT(eis_err_none, eis_error_c)

    parser => eis_get_interop_parser(parser_id)
    IF (.NOT. ASSOCIATED(parser)) RETURN

    CALL eis_c_f_string(name, name_f)
    CALL C_F_PROCPOINTER(fn, fn_ptr)
    errcode_f = eis_err_none
    CALL parser%add_emplaced_c_function(name_f, fn_ptr, errcode_f, &
        expected_params)

    errcode = IOR(errcode, INT(errcode_f, eis_error_c))

  END SUBROUTINE eis_add_emplaced_function



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to cause emplacement of emplaced functions
  !> @param[in] stack_id - ID of stack to emplace
  !> @param[inout] errcode - Error code
  !> @param[in] stack_id_out - ID of stack to put the emplaced result in
  !> if < 0 then emplacement happens in the stack that was passed in
  !> @param[in] host_params - Host parameters
  SUBROUTINE eis_emplace_stack(stack_id, errcode, stack_id_out, host_params) &
      BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: stack_id
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    INTEGER(C_INT), VALUE, INTENT(IN) :: stack_id_out
    TYPE(C_PTR), VALUE, INTENT(IN) :: host_params
    INTEGER(eis_error) :: errcode_f
    CLASS(eis_parser), POINTER :: parser
    TYPE(eis_stack), POINTER :: stack, stack_dest

    errcode = INT(eis_err_none, eis_error_c)
    stack => eis_get_interop_stack(stack_id, parser = parser)
    stack_dest => eis_get_interop_stack(stack_id_out)
    errcode_f = eis_err_none
    IF (ASSOCIATED(parser) .AND. ASSOCIATED(stack)) THEN
      IF (ASSOCIATED(stack_dest)) THEN
        CALL parser%emplace(stack, errcode_f, host_params, stack_dest)
      ELSE
        CALL parser%emplace(stack, errcode_f, host_params)
      END IF
      errcode = IOR(errcode, INT(errcode_f, eis_error_c))
    ELSE
      IF (.NOT. ASSOCIATED(stack)) errcode = IOR(errcode, &
          INT(eis_err_bad_stack, eis_error_c))
      IF (.NOT. ASSOCIATED(parser)) errcode = IOR(errcode, &
          INT(eis_err_bad_parser, eis_error_c))
    END IF

  END SUBROUTINE eis_emplace_stack



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to add a functor
  !> @param[in] parser_id - ID of parser to add the function to
  !> @param[in] name - Name to associate function with in expression
  !> @param[in] fn - Pointer to function to be called when the name is
  !> encountered. If fn is NULL then the functor will be an EIS deferred
  !> functor
  !> @param[in] bound_data
  !> @param[in] cap_bits - Capability bits
  !> @param[in] expected params - Expected number of parameters for the function
  !> if <0 then function will be variadic
  !> @param[in] can_simplify - Should the function be simplified through
  !> in general this should be 1 unless the function has different effects for
  !> different host code parameters
  !> @param[in] global - If 1 add the function to all parsers, if 0 only the
  !> specified parser
  !> @param[inout] errcode - Error code
  SUBROUTINE eis_add_functor(parser_id, name, fn, bound_data, cap_bits, &
      expected_params, can_simplify, global, errcode) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    TYPE(C_PTR), VALUE :: name
    TYPE(C_FUNPTR), VALUE :: fn
    TYPE(C_PTR), VALUE :: bound_data
    INTEGER(eis_bitmask_c), VALUE,  INTENT(IN) :: cap_bits
    INTEGER(C_INT), VALUE, INTENT(IN) :: expected_params
    INTEGER(C_INT), VALUE, INTENT(IN) :: can_simplify
    INTEGER(C_INT), VALUE, INTENT(IN) :: global
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    CLASS(eis_parser), POINTER :: parser
    CHARACTER(LEN=:), ALLOCATABLE :: fstring
    INTEGER(eis_error) :: f_errcode
    INTEGER(eis_bitmask) :: f_bitmask
    PROCEDURE(c_functor_function), POINTER :: fn_ptr
    TYPE(interop_functor) :: ifunc

    errcode = INT(eis_err_none, eis_error_c)

    IF (parser_id < 1 .OR. parser_id > interop_parser_count) RETURN
    parser => interop_parsers(parser_id)%contents

    IF (C_ASSOCIATED(fn)) THEN
      CALL C_F_PROCPOINTER(fn, fn_ptr)
    ELSE
      fn_ptr => NULL()
    END IF

    ifunc%bound_data = bound_data
    ifunc%c_bound_function => fn_ptr

    CALL eis_c_f_string(name, fstring)
    f_errcode = eis_err_none
    f_bitmask = INT(cap_bits, eis_bitmask)
    CALL parser%add_functor(fstring, ifunc, errcode, f_bitmask, &
        expected_params, can_simplify /= 0, .NOT. C_ASSOCIATED(fn), global /= 0)
    errcode = IOR(errcode, INT(f_errcode, eis_error_c))
    DEALLOCATE(fstring)

  END SUBROUTINE eis_add_functor


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to add a variable associated with a name that can
  !> be called from a parser expression
  !> @param[in] parser_id - ID of parser to add the variable to
  !> @param[in] name - Name to associate variable with in expression
  !> @param[in] fn - Pointer to function to be called when the name is
  !> encountered. If fn is NULL then this is a deferred variable
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
    TYPE(C_FUNPTR), VALUE :: fn
    INTEGER(eis_bitmask_c), VALUE,  INTENT(IN) :: cap_bits
    INTEGER(C_INT), VALUE, INTENT(IN) :: can_simplify
    INTEGER(C_INT), VALUE, INTENT(IN) :: global
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    CLASS(eis_parser), POINTER :: parser
    CHARACTER(LEN=:), ALLOCATABLE :: fstring
    INTEGER(eis_error) :: f_errcode
    INTEGER(eis_bitmask) :: f_bitmask
    PROCEDURE(parser_eval_fn), POINTER :: fn_ptr

    errcode = INT(eis_err_none, eis_error_c)

    IF (parser_id < 1 .OR. parser_id > interop_parser_count) RETURN
    parser => interop_parsers(parser_id)%contents

    IF (C_ASSOCIATED(fn)) THEN
      CALL C_F_PROCPOINTER(fn, fn_ptr)
    ELSE
      fn_ptr => eis_dummy
    END IF

    CALL eis_c_f_string(name, fstring)
    f_errcode = eis_err_none
    f_bitmask = INT(cap_bits, eis_bitmask)
    CALL parser%add_variable(fstring, fn_ptr, errcode, f_bitmask, &
        can_simplify /= 0, .NOT. C_ASSOCIATED(fn), global /= 0)
    errcode = IOR(errcode, INT(f_errcode, eis_error_c))
    DEALLOCATE(fstring)

  END SUBROUTINE eis_add_variable



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to add a stack variable
  !> @param[in] parser_id - ID of parser to add the variable to
  !> @param[in] name - Name to associate variable with in expression
  !> @param[in] stack_id - Stack to be returned when the variable is encountered
  !> If -1 then this will be a deferred variable
  !> @param[in] global - If 1 add the function to all parsers, if 0 only the
  !> specified parser
  !> @param[inout] errcode - Error code
  SUBROUTINE eis_add_stack_variable(parser_id, name, stack_id, global, &
      errcode) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    TYPE(C_PTR), VALUE :: name
    INTEGER(C_INT), VALUE, INTENT(IN) :: stack_id
    INTEGER(C_INT), VALUE, INTENT(IN) :: global
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    CLASS(eis_parser), POINTER :: parser
    CHARACTER(LEN=:), ALLOCATABLE :: fstring
    INTEGER(eis_error) :: f_errcode
    TYPE(eis_stack), POINTER :: stack

    errcode = INT(eis_err_none, eis_error_c)

    IF (parser_id < 1 .OR. parser_id > interop_parser_count) RETURN
    parser => eis_get_interop_parser(parser_id)
    stack => eis_get_interop_stack(stack_id)

    CALL eis_c_f_string(name, fstring)
    f_errcode = eis_err_none
    IF (ASSOCIATED(stack)) THEN
      CALL parser%add_stack_variable(fstring, stack, errcode, global /= 0)
    ELSE
      CALL parser%add_stack_variable(fstring, errcode, global /= 0)
    END IF
    errcode = IOR(errcode, INT(f_errcode, eis_error_c))
    DEALLOCATE(fstring)

  END SUBROUTINE eis_add_stack_variable



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to add a stack variable specified as a string
  !> @param[in] parser_id - ID of parser to add the variable to
  !> @param[in] name - Name to associate variable with in expression
  !> @param[in] stack_text - Stack text to be returned when the variable is
  !> encountered. If NULL then this will be a deferred variable
  !> @param[in] global - If 1 add the function to all parsers, if 0 only the
  !> specified parser
  !> @param[inout] errcode - Error code
  SUBROUTINE eis_add_stack_expression_variable(parser_id, name, stack_text, &
      global, errcode) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    TYPE(C_PTR), VALUE :: name
    TYPE(C_PTR), VALUE :: stack_text
    INTEGER(C_INT), VALUE, INTENT(IN) :: global
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    CLASS(eis_parser), POINTER :: parser
    CHARACTER(LEN=:), ALLOCATABLE :: fstring, stack_string
    INTEGER(eis_error) :: f_errcode

    errcode = INT(eis_err_none, eis_error_c)

    IF (parser_id < 1 .OR. parser_id > interop_parser_count) RETURN
    parser => eis_get_interop_parser(parser_id)

    CALL eis_c_f_string(name, fstring)
    CALL eis_c_f_string(stack_text, stack_string)
    f_errcode = eis_err_none
    IF (C_ASSOCIATED(stack_text)) THEN
      CALL parser%add_stack_variable(fstring, stack_string, errcode, &
          global /= 0)
    ELSE
      CALL parser%add_stack_variable(fstring, errcode, global /= 0)
    END IF
    errcode = IOR(errcode, INT(f_errcode, eis_error_c))
    DEALLOCATE(fstring)

  END SUBROUTINE eis_add_stack_expression_variable


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to add a variable associated with a name that can
  !> be called from a parser expression. The variable will be provided by
  !> dereferencing the provided pointer
  !> @detail
  !> This function allows adding a variable that dereferences a pointer to give
  !> a value. This value may be an int32_t, int64_t, float or double. There are
  !> two paramaters for whether the pointer is an integer or a real and whether
  !> the type is a 32bit or 64bit value. If a NULL pointer is passed then this
  !> is a deferred variable
  !> @param[in] parser_id - ID of parser to add the variable to
  !> @param[in] name - Name to associate variable with in expression
  !> @param[in] ptr - Pointer to underlying data
  !> @param[in] ptr_is_int - Is the pointer a pointer to an integer. Set to 1
  !> if true
  !> @param[in] ptr_is_64_bit - Is the pointer a pointer to a 64 bit type. Set
  !> to 1 if true, 0 if false
  !> @param[in] cap_bits - Capability bits
  !> @param[in] can_simplify - Should the variable be simplified
  !> in general this should be 1 unless the variable has different values for
  !> different host code parameters
  !> @param[in] global - If 1 add the function to all parsers, if 0 only the
  !> specified parser
  !> @param[inout] errcode - Error code
  SUBROUTINE eis_add_pointer_variable(parser_id, name, ptr, ptr_is_integer, &
      ptr_is_64_bit, cap_bits, can_simplify, global, errcode) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    TYPE(C_PTR), VALUE :: name
    TYPE(C_PTR), VALUE :: ptr
    INTEGER(C_INT), VALUE, INTENT(IN) :: ptr_is_integer
    INTEGER(C_INT), VALUE, INTENT(IN) :: ptr_is_64_bit
    INTEGER(eis_bitmask_c), VALUE,  INTENT(IN) :: cap_bits
    INTEGER(C_INT), VALUE, INTENT(IN) :: can_simplify
    INTEGER(C_INT), VALUE, INTENT(IN) :: global
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    CLASS(eis_parser), POINTER :: parser
    CHARACTER(LEN=:), ALLOCATABLE :: fstring
    INTEGER(eis_error) :: f_errcode
    INTEGER(eis_bitmask) :: f_bitmask

    errcode = INT(eis_err_none, eis_error_c)

    IF (parser_id < 1 .OR. parser_id > interop_parser_count) RETURN
    parser => interop_parsers(parser_id)%contents

    CALL eis_c_f_string(name, fstring)
    f_errcode = eis_err_none
    f_bitmask = INT(cap_bits, eis_bitmask)
    CALL parser%add_pointer_variable(fstring, ptr, ptr_is_integer /= 0, &
        ptr_is_64_bit /=0, errcode, f_bitmask, can_simplify /= 0, &
        .NOT. C_ASSOCIATED(ptr), global /= 0)
    errcode = IOR(errcode, INT(f_errcode, eis_error_c))
    DEALLOCATE(fstring)

  END SUBROUTINE eis_add_pointer_variable



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
    CLASS(eis_parser), POINTER :: parser
    CHARACTER(LEN=:), ALLOCATABLE :: fstring
    INTEGER(eis_error) :: f_errcode
    INTEGER(eis_bitmask) :: f_bitmask

    errcode = INT(eis_err_none, eis_error_c)

    IF (parser_id < 1 .OR. parser_id > interop_parser_count) RETURN
    parser => interop_parsers(parser_id)%contents

    CALL eis_c_f_string(name, fstring)
    f_errcode = eis_err_none
    f_bitmask = INT(cap_bits, eis_bitmask)
    CALL parser%add_constant(fstring, value, errcode, f_bitmask, &
        can_simplify /= 0, .FALSE., global /= 0)
    errcode = IOR(errcode, INT(f_errcode, eis_error_c))
    DEALLOCATE(fstring)

  END SUBROUTINE eis_add_constant



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to add a deferred constant
  !> @param[in] parser_id - ID of parser to add the variable to
  !> @param[in] name - Name to associate variable with in expression
  !> @param[in] cap_bits - Capability bits
  !> @param[in] can_simplify - Should the constant be simplified. 
  !> Generally always 1
  !> @param[in] global - If 1 add the function to all parsers, if 0 only the
  !> specified parser
  !> @param[inout] errcode - Error code
  SUBROUTINE eis_add_deferred_constant(parser_id, name, cap_bits, &
      can_simplify, global, errcode) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    TYPE(C_PTR), VALUE :: name
    INTEGER(eis_bitmask_c), VALUE,  INTENT(IN) :: cap_bits
    INTEGER(C_INT), VALUE, INTENT(IN) :: can_simplify
    INTEGER(C_INT), VALUE, INTENT(IN) :: global
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    CLASS(eis_parser), POINTER :: parser
    CHARACTER(LEN=:), ALLOCATABLE :: fstring
    INTEGER(eis_error) :: f_errcode
    INTEGER(eis_bitmask) :: f_bitmask

    errcode = INT(eis_err_none, eis_error_c)

    IF (parser_id < 1 .OR. parser_id > interop_parser_count) RETURN
    parser => interop_parsers(parser_id)%contents

    CALL eis_c_f_string(name, fstring)
    f_errcode = eis_err_none
    f_bitmask = INT(cap_bits, eis_bitmask)
    CALL parser%add_constant(fstring, errcode, f_bitmask, &
        can_simplify /= 0, global /= 0)
    errcode = IOR(errcode, INT(f_errcode, eis_error_c))
    DEALLOCATE(fstring)

  END SUBROUTINE eis_add_deferred_constant



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to evaluate a stored stack and return
  !> the result
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
  !> @param[in] host_params - Pointer to C interoperable structure that is
  !> passed to all function that are called as expression keys are evaluated
  !> @return eis_evaluate_stack - Number of results actually returned
  FUNCTION eis_evaluate_stack(stack_id, res_len, result, errcode, &
      host_params) BIND(C)

    INTEGER(C_INT), VALUE :: stack_id, res_len
    REAL(eis_num_c), DIMENSION(res_len), INTENT(OUT) :: result
    INTEGER(eis_error_c), INTENT(OUT) :: errcode
    TYPE(C_PTR), VALUE :: host_params
    INTEGER(C_INT) :: eis_evaluate_stack
    REAL(eis_num), DIMENSION(:), ALLOCATABLE :: f_result
    INTEGER(eis_error) :: f_errcode
    INTEGER :: returned_results, copied_results

    errcode = INT(eis_err_none, eis_error_c)
    eis_evaluate_stack = -1
    f_errcode = eis_err_none

    IF (stack_id < 1 .OR. stack_id > interop_stack_count) THEN
      errcode = IOR(errcode, INT(eis_err_bad_stack, eis_error_c))
      RETURN
    END IF

    returned_results =  interop_stacks(stack_id)%parser%evaluate(&
        interop_stacks(stack_id)%contents, f_result, f_errcode, host_params)

    errcode = IOR(errcode, INT(f_errcode, eis_error_c))
    IF (.NOT. ALLOCATED(f_result)) RETURN
    IF (res_len < SIZE(f_result)) THEN
      errcode = IOR(errcode, INT(eis_err_extra_results, eis_error_c))
    END IF
    eis_evaluate_stack = SIZE(f_result)

    copied_results = MIN(SIZE(f_result), res_len)
    result(1:copied_results) = REAL(f_result(1:copied_results), eis_num_c)

  END FUNCTION eis_evaluate_stack



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to get the number of errors caused by evaluating a
  !> stack
  !> @param[in] parser_id - ID of stored parser to count errors in
  !> @return eis_get_error_count - Number of errors in the error handler
  FUNCTION eis_parser_get_error_count(parser_id) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    INTEGER(C_INT) :: eis_parser_get_error_count
    CLASS(eis_parser), POINTER :: parser

    parser => eis_get_interop_parser(parser_id)
    eis_parser_get_error_count = 0
    IF (.NOT. ASSOCIATED(parser)) RETURN
    eis_parser_get_error_count = parser%get_error_count()

  END FUNCTION eis_parser_get_error_count



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to get the error report from a stack id
  !> and an error id. The error ID is an integer between 1 and the result
  !> from eis_get_error_count
  !> @param[in] parser_id - ID of stored parser to get errors from
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
  FUNCTION eis_parser_get_error_report(parser_id, error_id, buflen, &
      string_out) BIND(C)
    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id, error_id, buflen
    TYPE(C_PTR), VALUE, INTENT(IN) :: string_out
    INTEGER(C_INT) :: eis_parser_get_error_report
    CHARACTER(LEN=:), ALLOCATABLE :: errstr
    CLASS(eis_parser), POINTER :: parser

    parser => eis_get_interop_parser(parser_id)
    eis_parser_get_error_report = 0
    IF (.NOT. ASSOCIATED(parser)) RETURN
    CALL parser%get_error_report(error_id, errstr)
    CALL eis_f_c_string(errstr, buflen, string_out)
    eis_parser_get_error_report = LEN(errstr) + 1
    DEALLOCATE(errstr)

  END FUNCTION eis_parser_get_error_report



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to empty the list of all errors
  !> @param[in] parser_id - ID of stored parser to get errors from
  SUBROUTINE eis_parser_flush_errors(parser_id) BIND(C)
    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    CLASS(eis_parser), POINTER :: parser

    parser => eis_get_interop_parser(parser_id)
    IF (.NOT. ASSOCIATED(parser)) RETURN
    CALL parser%flush_errors()

  END SUBROUTINE eis_parser_flush_errors



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
  !> Increment the reference count for a parser. Use to indicate
  !> that another part of your code wants to use a given parser
  !> @param[in] parser_id - ID of parser to increment the reference of
  SUBROUTINE eis_parser_inc_ref(parser_id) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    IF (parser_id < 1 .OR. parser_id > interop_parser_count) THEN
      RETURN
    END IF

    interop_parsers(parser_id)%refcount = interop_parsers(parser_id)%refcount &
       + 1

  END SUBROUTINE eis_parser_inc_ref



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

    interop_stacks(stack_id)%refcount = MAX(&
        interop_stacks(stack_id)%refcount - 1, 0)
    IF (interop_stacks(stack_id)%refcount == 0) THEN
      CALL eis_release_interop_stack(stack_id)
    END IF

  END SUBROUTINE eis_stack_dec_ref



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Decrement the reference count for a parser. Use to indicate
  !> that another part of your code has finished using a given parser.
  !> When the reference count reaches zero the parser will be deallocated
  !> IF it was created by eis_create_parser. If it was created by a Fortran
  !> code and given to the interoperable interface then it will not be 
  !> deallocated even when the reference count reaches zero
  !> @param[in] parser_id - ID of parser to decrement the reference of
  SUBROUTINE eis_parser_dec_ref(parser_id) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id

    IF (parser_id < 1 .OR. parser_id > interop_parser_count) THEN
      RETURN
    END IF

    interop_parsers(parser_id)%refcount = MAX(&
        interop_parsers(parser_id)%refcount - 1, 0)
    
    IF (interop_parsers(parser_id)%refcount == 0) THEN
      CALL eis_release_interop_parser(parser_id)
    END IF

  END SUBROUTINE eis_parser_dec_ref



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Creates a copy of a stack. Returns the integer ID of the copy
  !> @param[in] stack_id - ID of stored stack to decrement the reference of
  !> @return eis_copy_stack - ID of copy of stack
  FUNCTION eis_copy_stack(stack_id) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: stack_id
    INTEGER(C_INT) :: eis_copy_stack
    CLASS(eis_parser), POINTER :: parser
    CLASS(eis_stack), POINTER :: old

    IF (stack_id < 1 .OR. stack_id > interop_stack_count) THEN
      eis_copy_stack = -1
      RETURN
    END IF

    eis_copy_stack = -1
    eis_copy_stack = eis_copy_interop_stack(stack_id);

  END FUNCTION eis_copy_stack



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Visualises a stack as a graphviz dot structure
  !> @param[in] stack_id - ID of stored stack to visualise
  !> @param[inout] text - Pointer to string to be populated. Passing NULL
  !> is acceptable to only probe for the required length of string
  !> @param[in] formatstr - A fortran REAL format string to be used
  !> for describing numerical literals. If NULL then a default format is used
  !> @return eis_visualise_stack - Number of characters actually returned
  !> from the visualisation. If this is less than text_length then
  !> the function should be run again with a buffer large enough to
  !> contain this many characters or the dot structure will be invalid
  FUNCTION eis_visualise_stack(stack_id, text_length, text, formatstr) BIND(C)
    INTEGER(C_INT), VALUE, INTENT(IN) :: stack_id
    INTEGER(C_INT), VALUE, INTENT(IN) :: text_length
    TYPE(C_PTR), VALUE, INTENT(IN) :: text
    TYPE(C_PTR), VALUE, INTENT(IN) :: formatstr
    INTEGER(C_INT) :: eis_visualise_stack
    CHARACTER(LEN=:), ALLOCATABLE :: fstr, fform
    CLASS(eis_parser), POINTER :: parser
    CLASS(eis_stack), POINTER :: stack

    stack => eis_get_interop_stack(stack_id, parser = parser)
    eis_visualise_stack = -1
    IF (ASSOCIATED(stack) .AND. ASSOCIATED(parser)) THEN
      IF (C_ASSOCIATED(formatstr)) THEN
        CALL eis_c_f_string(formatstr, fform)
        CALL parser%visualise_stack(stack, fstr, fform)
        DEALLOCATE(fform)
      ELSE
        CALL parser%visualise_stack(stack, fstr)
      END IF
      eis_visualise_stack = LEN(fstr)
      IF (C_ASSOCIATED(text)) CALL eis_f_c_string(fstr, text_length, text)
    END IF

  END FUNCTION eis_visualise_stack



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Returns a stack converted back into infix maths
  !> This will be the stack after simplification, emplacement and inlining
  !> of all stack variables. Bracketing is not guaranteed to be the same as
  !> the original expression that generated the stack but the expression
  !> WILL be mathematically equivalent
  !> @param[in] stack_id - ID of stored stack to get infix maths of
  !> @param[inout] text - Pointer to string to be populated. Passing NULL
  !> is acceptable to only probe for the required length of string
  !> @param[in] formatstr - A fortran REAL format string to be used
  !> for describing numerical literals. If NULL then a default format is used
  !> @return eis_get_infix_stack - Number of characters actually returned
  !> from the stack. If this is less than text_length then
  !> the function should be run again with a buffer large enough to
  !> contain this many characters or the dot structure will be invalid
  FUNCTION eis_get_infix_stack(stack_id, text_length, text, formatstr) BIND(C)
    INTEGER(C_INT), VALUE, INTENT(IN) :: stack_id
    INTEGER(C_INT), VALUE, INTENT(IN) :: text_length
    TYPE(C_PTR), VALUE, INTENT(IN) :: text
    TYPE(C_PTR), VALUE, INTENT(IN) :: formatstr
    INTEGER(C_INT) :: eis_get_infix_stack
    CHARACTER(LEN=:), ALLOCATABLE :: fstr, fform
    CLASS(eis_parser), POINTER :: parser
    TYPE(eis_stack), POINTER :: stack
    INTEGER(eis_error) :: errcode

    stack => eis_get_interop_stack(stack_id, parser = parser)
    eis_get_infix_stack = -1
    IF (ASSOCIATED(stack) .AND. ASSOCIATED(parser)) THEN
      IF (C_ASSOCIATED(formatstr)) THEN
        CALL eis_c_f_string(formatstr, fform)
        CALL parser%get_infix(stack, fstr, fform)
        DEALLOCATE(fform)
      ELSE
        CALL parser%get_infix(stack, fstr)
      END IF
      IF (ALLOCATED(fstr)) THEN
        eis_get_infix_stack = LEN(fstr)
        IF (C_ASSOCIATED(text)) CALL eis_f_c_string(fstr, text_length, text)
      END IF
    END IF

  END FUNCTION eis_get_infix_stack


END MODULE eis_parser_interop
