#include "interop_version.inc"
#if (EIS_C_INTEROP_VERSION == 1)
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
      REAL(eis_num) :: c_functor_function
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
    PROCEDURE(parser_result_function), POINTER :: result_fn_ptr

    errcode = INT(eis_err_none, eis_error_c)
    stack => eis_get_interop_stack(stack_id, parser = parser)
    CALL C_F_PROCPOINTER(fn, result_fn_ptr)
    IF (ASSOCIATED(stack) .AND. ASSOCIATED(parser)) THEN
      IF (C_ASSOCIATED(filename)) THEN
        CALL eis_c_f_string(filename, fstring)
        CALL parser%set_result_function(result_fn_ptr, stack, errcode, &
            fstring, INT(line_number), INT(char_offset), &
            INT(cap_bits, eis_bitmask), append /= 0)
      ELSE
        CALL parser%set_result_function(result_fn_ptr, stack, errcode, &
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
    PROCEDURE(parser_late_bind_interop_fn), POINTER :: late_bind_fn_ptr
    INTEGER(eis_error) :: errcode_f
    CLASS(eis_parser), POINTER :: parser
    CHARACTER(LEN=:), ALLOCATABLE :: name_f

    errcode = INT(eis_err_none, eis_error_c)

    parser => eis_get_interop_parser(parser_id)
    IF (.NOT. ASSOCIATED(parser)) RETURN

    CALL eis_c_f_string(name, name_f)
    CALL C_F_PROCPOINTER(fn, late_bind_fn_ptr)
    errcode_f = eis_err_none
    CALL parser%add_emplaced_c_function(name_f, late_bind_fn_ptr, errcode_f, &
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
    PROCEDURE(c_functor_function), POINTER :: functor_fn_ptr
    TYPE(interop_functor) :: ifunc

    errcode = INT(eis_err_none, eis_error_c)

    IF (parser_id < 1 .OR. parser_id > interop_parser_count) RETURN
    parser => interop_parsers(parser_id)%contents

    IF (C_ASSOCIATED(fn)) THEN
      CALL C_F_PROCPOINTER(fn, functor_fn_ptr)
    ELSE
      functor_fn_ptr => NULL()
    END IF

    ifunc%bound_data = bound_data
    ifunc%c_bound_function => functor_fn_ptr

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
#elif (EIS_C_INTEROP_VERSION == 2)
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
      REAL(eis_num) :: c_functor_function
    END FUNCTION
  END INTERFACE

  TYPE, EXTENDS(eis_functor) :: interop_functor
    TYPE(C_PTR) :: bound_data = C_NULL_PTR
    PROCEDURE(c_functor_function), NOPASS, POINTER :: c_bound_function
  CONTAINS
    PROCEDURE :: operate => if_operate
  END TYPE interop_functor

  !> Parameters for eis_create_parser
  TYPE, BIND(C) :: eis_create_parser_params
    INTEGER(C_INT) :: should_simplify
    INTEGER(C_INT) :: should_minify
    INTEGER(C_INT) :: allow_text
    INTEGER(C_INT) :: no_import
    INTEGER(C_INT) :: maths_fail_quickly
    INTEGER(C_INT) :: errhandler
  END TYPE eis_create_parser_params

  !> Parameters for eis_create_stack
  TYPE, BIND(C) :: eis_create_stack_params
    INTEGER(C_INT) :: simplify
    INTEGER(C_INT) :: minify
    INTEGER(C_INT) :: allow_text
    TYPE(C_PTR) :: filename
    INTEGER(C_INT) :: line_number
    INTEGER(C_INT) :: char_offset
  END TYPE eis_create_stack_params

  !> Parameters for eis_append_stack
  TYPE, BIND(C) :: eis_append_stack_params
    INTEGER(C_INT) :: simplify
    INTEGER(C_INT) :: minify
    INTEGER(C_INT) :: allow_text
    TYPE(C_PTR) :: filename
    INTEGER(C_INT) :: line_number
    INTEGER(C_INT) :: char_offset
  END TYPE eis_append_stack_params

  !> Parameters for eis_populate_stack
  TYPE, BIND(C) :: eis_populate_stack_params
    INTEGER(C_INT) :: simplify
    INTEGER(C_INT) :: minify
    INTEGER(C_INT) :: allow_text
    TYPE(C_PTR) :: filename
    INTEGER(C_INT) :: line_number
    INTEGER(C_INT) :: char_offset
  END TYPE eis_populate_stack_params

  !> Parameters for eis_empty_stack
  TYPE, BIND(C) :: eis_empty_stack_params
    INTEGER(C_INT) :: dummy
  END TYPE eis_empty_stack_params

  !> Parameters for eis_bind_result_to_stack
  TYPE, BIND(C) :: eis_bind_result_to_stack_params
    TYPE(C_PTR) :: filename
    INTEGER(C_INT) :: line_number
    INTEGER(C_INT) :: char_offset
    INTEGER(eis_bitmask_c) :: cap_bits
    INTEGER(C_INT) :: append
  END TYPE eis_bind_result_to_stack_params

  !> Parameters for eis_evaluate_stack
  TYPE, BIND(C) :: eis_evaluate_stack_params
    INTEGER(C_INT) :: dummy
  END TYPE eis_evaluate_stack_params

  !> Parameters for eis_add_function
  TYPE, BIND(C) :: eis_add_function_params
    INTEGER(eis_bitmask_c) :: cap_bits
    INTEGER(C_INT) :: can_simplify
    INTEGER(C_INT) :: defer
    INTEGER(C_INT) :: global
    TYPE(C_PTR) :: description
    INTEGER(C_INT) :: hidden
    INTEGER(C_INT) :: text_params
    INTEGER(C_INT) :: is_idempotent
    INTEGER(eis_bitmask_c) :: text_param_mask
  END TYPE eis_add_function_params

  !> Parameters for eis_add_emplaced_function
  TYPE, BIND(C) :: eis_add_emplaced_function_params
    TYPE(C_PTR) :: description
    INTEGER(C_INT) :: hidden
  END TYPE eis_add_emplaced_function_params

  !> Parameters for eis_emplace_stack
  TYPE, BIND(C) :: eis_emplace_stack_params
    INTEGER(C_INT) :: stack_id_out
  END TYPE eis_emplace_stack_params

  !> Parameters for eis_add_functor
  TYPE, BIND(C) :: eis_add_functor_params
    INTEGER(eis_bitmask_c) :: cap_bits
    INTEGER(C_INT) :: can_simplify
    INTEGER(C_INT) :: defer
    INTEGER(C_INT) :: global
    TYPE(C_PTR) :: description
    INTEGER(C_INT) :: hidden
    INTEGER(C_INT) :: text_params
    INTEGER(C_INT) :: is_idempotent
    INTEGER(eis_bitmask_c) :: text_param_mask
  END TYPE eis_add_functor_params

  !> Parameters for eis_add_variable
  TYPE, BIND(C) :: eis_add_variable_params
    INTEGER(eis_bitmask_c) :: cap_bits
    INTEGER(C_INT) :: can_simplify
    INTEGER(C_INT) :: defer
    INTEGER(C_INT) :: global
    TYPE(C_PTR) :: description
    INTEGER(C_INT) :: hidden
  END TYPE eis_add_variable_params

  !> Parameters for eis_add_stack_variable
  TYPE, BIND(C) :: eis_add_stack_variable_params
    INTEGER(C_INT) :: defer
    INTEGER(C_INT) :: global
    TYPE(C_PTR) :: description
    INTEGER(C_INT) :: hidden
  END TYPE eis_add_stack_variable_params

  !> Parameters for eis_add_stack_expression_variable
  TYPE, BIND(C) :: eis_add_stack_expression_variable_params
    INTEGER(C_INT) :: global
    TYPE(C_PTR) :: description
    INTEGER(C_INT) :: hidden
  END TYPE eis_add_stack_expression_variable_params

  !> Parameters for eis_add_pointer_variable
  TYPE, BIND(C) :: eis_add_pointer_variable_params
    INTEGER(eis_bitmask_c) :: cap_bits
    INTEGER(C_INT) :: can_simplify
    INTEGER(C_INT) :: defer
    INTEGER(C_INT) :: global
    TYPE(C_PTR) :: description
    INTEGER(C_INT) :: hidden
  END TYPE eis_add_pointer_variable_params

  !> Parameters for eis_add_constant
  TYPE, BIND(C) :: eis_add_constant_params
    INTEGER(eis_bitmask_c) :: cap_bits
    INTEGER(C_INT) :: can_simplify
    INTEGER(C_INT) :: defer
    INTEGER(C_INT) :: global
    TYPE(C_PTR) :: description
    INTEGER(C_INT) :: hidden
  END TYPE eis_add_constant_params

  !> Parameters for eis_add_deferred_constant
  TYPE, BIND(C) :: eis_add_deferred_constant_params
    INTEGER(eis_bitmask_c) :: cap_bits
    INTEGER(C_INT) :: can_simplify
    INTEGER(C_INT) :: global
    TYPE(C_PTR) :: description
    INTEGER(C_INT) :: hidden
  END TYPE eis_add_deferred_constant_params

  !> Parameters for eis_parser_get_error_count
  TYPE, BIND(C) :: eis_parser_get_error_count_params
    INTEGER(C_INT) :: dummy
  END TYPE eis_parser_get_error_count_params

  !> Parameters for eis_parser_get_error_report
  TYPE, BIND(C) :: eis_parser_get_error_report_params
    INTEGER(C_INT) :: dummy
  END TYPE eis_parser_get_error_report_params

  !> Parameters for eis_parser_flush_errors
  TYPE, BIND(C) :: eis_parser_flush_errors_params
    INTEGER(C_INT) :: dummy
  END TYPE eis_parser_flush_errors_params

  !> Parameters for eis_stack_inc_ref
  TYPE, BIND(C) :: eis_stack_inc_ref_params
    INTEGER(C_INT) :: dummy
  END TYPE eis_stack_inc_ref_params

  !> Parameters for eis_parser_inc_ref
  TYPE, BIND(C) :: eis_parser_inc_ref_params
    INTEGER(C_INT) :: dummy
  END TYPE eis_parser_inc_ref_params

  !> Parameters for eis_stack_dec_ref
  TYPE, BIND(C) :: eis_stack_dec_ref_params
    INTEGER(C_INT) :: dummy
  END TYPE eis_stack_dec_ref_params

  !> Parameters for eis_parser_dec_ref
  TYPE, BIND(C) :: eis_parser_dec_ref_params
    INTEGER(C_INT) :: dummy
  END TYPE eis_parser_dec_ref_params

  !> Parameters for eis_copy_stack
  TYPE, BIND(C) :: eis_copy_stack_params
    INTEGER(C_INT) :: dummy
  END TYPE eis_copy_stack_params

  !> Parameters for eis_visualise_stack
  TYPE, BIND(C) :: eis_visualise_stack_params
    TYPE(C_PTR) :: formatstr
  END TYPE eis_visualise_stack_params

  !> Parameters for eis_get_infix_stack
  TYPE, BIND(C) :: eis_get_infix_stack_params
    TYPE(C_PTR) :: formatstr
  END TYPE eis_get_infix_stack_params

  !> Parameters for eis_get_deriv_stack
  TYPE, BIND(C) :: eis_get_deriv_stack_params
    INTEGER(C_INT) :: dummy
  END TYPE eis_get_deriv_stack_params

  !> Parameters for eis_set_symbol_derivative
  TYPE, BIND(C) :: eis_set_symbol_derivative_params
    INTEGER(C_INT) :: global
  END TYPE eis_set_symbol_derivative_params

  !> Parameters for eis_lock_symbol_derivative
  TYPE, BIND(C) :: eis_lock_symbol_derivative_params
    INTEGER(C_INT) :: global
  END TYPE eis_lock_symbol_derivative_params

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
  !> C interoperable function to set the parameters for eis_create_parser to
  !> their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_create_parser_params_init(params) BIND(C)

    TYPE(eis_create_parser_params), INTENT(OUT) :: params

    params%should_simplify = 1
    params%should_minify = 0
    params%allow_text = 0
    params%no_import = 0
    params%maths_fail_quickly = 0
    params%errhandler = 0

  END SUBROUTINE eis_create_parser_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for eis_create_stack to
  !> their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_create_stack_params_init(params) BIND(C)

    TYPE(eis_create_stack_params), INTENT(OUT) :: params

    params%simplify = -1
    params%minify = -1
    params%allow_text = -1
    params%filename = C_NULL_PTR
    params%line_number = -1
    params%char_offset = 0

  END SUBROUTINE eis_create_stack_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for eis_append_stack to
  !> their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_append_stack_params_init(params) BIND(C)

    TYPE(eis_append_stack_params), INTENT(OUT) :: params

    params%simplify = -1
    params%minify = -1
    params%allow_text = -1
    params%filename = C_NULL_PTR
    params%line_number = -1
    params%char_offset = 0

  END SUBROUTINE eis_append_stack_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for eis_populate_stack to
  !> their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_populate_stack_params_init(params) BIND(C)

    TYPE(eis_populate_stack_params), INTENT(OUT) :: params

    params%simplify = -1
    params%minify = -1
    params%allow_text = -1
    params%filename = C_NULL_PTR
    params%line_number = -1
    params%char_offset = 0

  END SUBROUTINE eis_populate_stack_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for eis_empty_stack to
  !> their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_empty_stack_params_init(params) BIND(C)

    TYPE(eis_empty_stack_params), INTENT(OUT) :: params

    params%dummy = 0

  END SUBROUTINE eis_empty_stack_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_bind_result_to_stack to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_bind_result_to_stack_params_init(params) BIND(C)

    TYPE(eis_bind_result_to_stack_params), INTENT(OUT) :: params

    params%filename = C_NULL_PTR
    params%line_number = -1
    params%char_offset = 0
    params%cap_bits = 0_eis_bitmask_c
    params%append = 0

  END SUBROUTINE eis_bind_result_to_stack_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for eis_evaluate_stack to
  !> their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_evaluate_stack_params_init(params) BIND(C)

    TYPE(eis_evaluate_stack_params), INTENT(OUT) :: params

    params%dummy = 0

  END SUBROUTINE eis_evaluate_stack_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for eis_add_function to
  !> their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_add_function_params_init(params) BIND(C)

    TYPE(eis_add_function_params), INTENT(OUT) :: params

    params%cap_bits = 0_eis_bitmask_c
    params%can_simplify = 1
    params%defer = 0
    params%global = 0
    params%description = C_NULL_PTR
    params%hidden = 0
    params%text_params = 0
    params%is_idempotent = 0
    params%text_param_mask = 0_eis_bitmask_c

  END SUBROUTINE eis_add_function_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_add_emplaced_function to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_add_emplaced_function_params_init(params) BIND(C)

    TYPE(eis_add_emplaced_function_params), INTENT(OUT) :: params

    params%description = C_NULL_PTR
    params%hidden = 0

  END SUBROUTINE eis_add_emplaced_function_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for eis_emplace_stack to
  !> their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_emplace_stack_params_init(params) BIND(C)

    TYPE(eis_emplace_stack_params), INTENT(OUT) :: params

    params%stack_id_out = -1

  END SUBROUTINE eis_emplace_stack_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for eis_add_functor to
  !> their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_add_functor_params_init(params) BIND(C)

    TYPE(eis_add_functor_params), INTENT(OUT) :: params

    params%cap_bits = 0_eis_bitmask_c
    params%can_simplify = 1
    params%defer = 0
    params%global = 0
    params%description = C_NULL_PTR
    params%hidden = 0
    params%text_params = 0
    params%is_idempotent = 0
    params%text_param_mask = 0_eis_bitmask_c

  END SUBROUTINE eis_add_functor_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for eis_add_variable to
  !> their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_add_variable_params_init(params) BIND(C)

    TYPE(eis_add_variable_params), INTENT(OUT) :: params

    params%cap_bits = 0_eis_bitmask_c
    params%can_simplify = 0
    params%defer = 0
    params%global = 0
    params%description = C_NULL_PTR
    params%hidden = 0

  END SUBROUTINE eis_add_variable_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for eis_add_stack_variable
  !> to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_add_stack_variable_params_init(params) BIND(C)

    TYPE(eis_add_stack_variable_params), INTENT(OUT) :: params

    params%defer = 0
    params%global = 0
    params%description = C_NULL_PTR
    params%hidden = 0

  END SUBROUTINE eis_add_stack_variable_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_add_stack_expression_variable to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_add_stack_expression_variable_params_init(params) BIND(C)

    TYPE(eis_add_stack_expression_variable_params), INTENT(OUT) :: params

    params%global = 0
    params%description = C_NULL_PTR
    params%hidden = 0

  END SUBROUTINE eis_add_stack_expression_variable_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_add_pointer_variable to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_add_pointer_variable_params_init(params) BIND(C)

    TYPE(eis_add_pointer_variable_params), INTENT(OUT) :: params

    params%cap_bits = 0_eis_bitmask_c
    params%can_simplify = 0
    params%defer = 0
    params%global = 0
    params%description = C_NULL_PTR
    params%hidden = 0

  END SUBROUTINE eis_add_pointer_variable_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for eis_add_constant to
  !> their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_add_constant_params_init(params) BIND(C)

    TYPE(eis_add_constant_params), INTENT(OUT) :: params

    params%cap_bits = 0_eis_bitmask_c
    params%can_simplify = 1
    params%defer = 0
    params%global = 0
    params%description = C_NULL_PTR
    params%hidden = 0

  END SUBROUTINE eis_add_constant_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_add_deferred_constant to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_add_deferred_constant_params_init(params) BIND(C)

    TYPE(eis_add_deferred_constant_params), INTENT(OUT) :: params

    params%cap_bits = 0_eis_bitmask_c
    params%can_simplify = 1
    params%global = 0
    params%description = C_NULL_PTR
    params%hidden = 0

  END SUBROUTINE eis_add_deferred_constant_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_parser_get_error_count to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_parser_get_error_count_params_init(params) BIND(C)

    TYPE(eis_parser_get_error_count_params), INTENT(OUT) :: params

    params%dummy = 0

  END SUBROUTINE eis_parser_get_error_count_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_parser_get_error_report to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_parser_get_error_report_params_init(params) BIND(C)

    TYPE(eis_parser_get_error_report_params), INTENT(OUT) :: params

    params%dummy = 0

  END SUBROUTINE eis_parser_get_error_report_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_parser_flush_errors to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_parser_flush_errors_params_init(params) BIND(C)

    TYPE(eis_parser_flush_errors_params), INTENT(OUT) :: params

    params%dummy = 0

  END SUBROUTINE eis_parser_flush_errors_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for eis_stack_inc_ref to
  !> their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_stack_inc_ref_params_init(params) BIND(C)

    TYPE(eis_stack_inc_ref_params), INTENT(OUT) :: params

    params%dummy = 0

  END SUBROUTINE eis_stack_inc_ref_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for eis_parser_inc_ref to
  !> their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_parser_inc_ref_params_init(params) BIND(C)

    TYPE(eis_parser_inc_ref_params), INTENT(OUT) :: params

    params%dummy = 0

  END SUBROUTINE eis_parser_inc_ref_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for eis_stack_dec_ref to
  !> their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_stack_dec_ref_params_init(params) BIND(C)

    TYPE(eis_stack_dec_ref_params), INTENT(OUT) :: params

    params%dummy = 0

  END SUBROUTINE eis_stack_dec_ref_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for eis_parser_dec_ref to
  !> their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_parser_dec_ref_params_init(params) BIND(C)

    TYPE(eis_parser_dec_ref_params), INTENT(OUT) :: params

    params%dummy = 0

  END SUBROUTINE eis_parser_dec_ref_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for eis_copy_stack to
  !> their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_copy_stack_params_init(params) BIND(C)

    TYPE(eis_copy_stack_params), INTENT(OUT) :: params

    params%dummy = 0

  END SUBROUTINE eis_copy_stack_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for eis_visualise_stack to
  !> their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_visualise_stack_params_init(params) BIND(C)

    TYPE(eis_visualise_stack_params), INTENT(OUT) :: params

    params%formatstr = C_NULL_PTR

  END SUBROUTINE eis_visualise_stack_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for eis_get_infix_stack to
  !> their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_get_infix_stack_params_init(params) BIND(C)

    TYPE(eis_get_infix_stack_params), INTENT(OUT) :: params

    params%formatstr = C_NULL_PTR

  END SUBROUTINE eis_get_infix_stack_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to create a parser and return a unique id that
  !> is used in future calls to the parser system
  !> @param[in] physics - Physics package to import. One of the eis_physics_
  !> constants
  !> @param[inout] errcode - Error code from initialising the parser
  !> @param[in] params - Pointer to an eis_create_parser_params structure.
  !> NULL gives the default parameters
  !> @return parser_id - Unique ID of the created parser
  FUNCTION eis_create_parser(physics, errcode, params) BIND(C) &
      RESULT(parser_id)

    INTEGER(C_INT), VALUE, INTENT(IN) :: physics
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    INTEGER(C_INT) :: parser_id
    CLASS(eis_parser), POINTER :: parser
    TYPE(eis_create_parser_params), POINTER :: p
    TYPE(eis_create_parser_params), TARGET :: defaults

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_create_parser_params_init(defaults)
      p => defaults
    END IF

    parser_id = create_new_parser()
    parser => eis_get_interop_parser(parser_id)
    CALL parser%init(errcode, &
        should_simplify = (p%should_simplify /= 0), &
        should_minify = (p%should_minify /= 0), &
        allow_text = (p%allow_text /= 0), &
        no_import = (p%no_import /= 0), &
        physics = INT(physics), &
        maths_fail_quickly = (p%maths_fail_quickly /= 0))

  END FUNCTION eis_create_parser



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to create a stack associated with a parser
  !> this stack can then be evaluated by ID as required
  !> @param[in] parser_id - ID of parser to associate the stack with
  !> @param[in] expression - String to evaluate to stack
  !> @param[inout] capbits - Capability bits returned from parsing the stack
  !> @param[inout] errcode - Error code
  !> @param[in] params - Pointer to an eis_create_stack_params structure.
  !> NULL gives the default parameters
  !> @return stack_id - Unique ID of the created parser
  FUNCTION eis_create_stack(parser_id, expression, cap_bits, errcode, params) &
      BIND(C) RESULT(stack_id)

    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    TYPE(C_PTR), VALUE :: expression
    INTEGER(eis_bitmask_c), INTENT(INOUT) :: cap_bits
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    INTEGER(C_INT) :: stack_id
    CHARACTER(LEN=:), ALLOCATABLE :: fstring
    CHARACTER(LEN=:), ALLOCATABLE, TARGET :: fname
    CHARACTER(LEN=:), POINTER :: pfname
    LOGICAL, TARGET :: simp, mini, atxt
    LOGICAL, POINTER :: psimp, pmini, patxt
    INTEGER(eis_error) :: f_errcode
    TYPE(eis_create_stack_params), POINTER :: p
    TYPE(eis_create_stack_params), TARGET :: defaults

    errcode = INT(eis_err_none, eis_error_c)

    IF (parser_id < 1 .OR. parser_id > interop_parser_count) THEN
      stack_id = -1
      RETURN
    END IF

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_create_stack_params_init(defaults)
      p => defaults
    END IF

    psimp => NULL()
    pmini => NULL()
    patxt => NULL()
    pfname => NULL()
    IF (p%simplify >= 0) THEN
      simp = (p%simplify /= 0)
      psimp => simp
    END IF
    IF (p%minify >= 0) THEN
      mini = (p%minify /= 0)
      pmini => mini
    END IF
    IF (p%allow_text >= 0) THEN
      atxt = (p%allow_text /= 0)
      patxt => atxt
    END IF
    IF (C_ASSOCIATED(p%filename)) THEN
      CALL eis_c_f_string(p%filename, fname)
      pfname => fname
    END IF

    stack_id = create_new_stack(INT(parser_id))
    IF (C_ASSOCIATED(expression)) THEN
      CALL eis_c_f_string(expression, fstring)
      f_errcode = 0_eis_error
      CALL interop_stacks(stack_id)%parser%tokenize(fstring, &
          interop_stacks(stack_id)%contents, f_errcode, simplify = psimp, &
          minify = pmini, filename = pfname, &
          line_number = INT(p%line_number), char_offset = INT(p%char_offset), &
          append = .FALSE., allow_text = patxt)
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
  !> @param[in] params - Pointer to an eis_append_stack_params structure.
  !> NULL gives the default parameters
  SUBROUTINE eis_append_stack(stack_id, expression, cap_bits, errcode, params) &
      BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: stack_id
    TYPE(C_PTR), VALUE :: expression
    INTEGER(eis_bitmask_c), INTENT(INOUT) :: cap_bits
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    CHARACTER(LEN=:), ALLOCATABLE :: fstring
    CHARACTER(LEN=:), ALLOCATABLE, TARGET :: fname
    CHARACTER(LEN=:), POINTER :: pfname
    LOGICAL, TARGET :: simp, mini, atxt
    LOGICAL, POINTER :: psimp, pmini, patxt
    INTEGER(eis_error) :: f_errcode
    CLASS(eis_parser), POINTER :: parser
    CLASS(eis_stack), POINTER :: stack
    TYPE(eis_append_stack_params), POINTER :: p
    TYPE(eis_append_stack_params), TARGET :: defaults

    errcode = INT(eis_err_none, eis_error_c)

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_append_stack_params_init(defaults)
      p => defaults
    END IF

    psimp => NULL()
    pmini => NULL()
    patxt => NULL()
    pfname => NULL()
    IF (p%simplify >= 0) THEN
      simp = (p%simplify /= 0)
      psimp => simp
    END IF
    IF (p%minify >= 0) THEN
      mini = (p%minify /= 0)
      pmini => mini
    END IF
    IF (p%allow_text >= 0) THEN
      atxt = (p%allow_text /= 0)
      patxt => atxt
    END IF
    IF (C_ASSOCIATED(p%filename)) THEN
      CALL eis_c_f_string(p%filename, fname)
      pfname => fname
    END IF

    CALL eis_c_f_string(expression, fstring)
    f_errcode = eis_err_none
    stack => eis_get_interop_stack(stack_id, parser = parser)
    IF (ASSOCIATED(stack) .AND. ASSOCIATED(parser)) THEN
      CALL parser%tokenize(fstring, stack, f_errcode, simplify = psimp, &
          minify = pmini, filename = pfname, &
          line_number = INT(p%line_number), char_offset = INT(p%char_offset), &
          append = .TRUE., allow_text = patxt)
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
  !> @param[in] params - Pointer to an eis_populate_stack_params structure.
  !> NULL gives the default parameters
  SUBROUTINE eis_populate_stack(stack_id, expression, cap_bits, errcode, &
      params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: stack_id
    TYPE(C_PTR), VALUE :: expression
    INTEGER(eis_bitmask_c), INTENT(INOUT) :: cap_bits
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    CHARACTER(LEN=:), ALLOCATABLE :: fstring
    CHARACTER(LEN=:), ALLOCATABLE, TARGET :: fname
    CHARACTER(LEN=:), POINTER :: pfname
    LOGICAL, TARGET :: simp, mini, atxt
    LOGICAL, POINTER :: psimp, pmini, patxt
    INTEGER(eis_error) :: f_errcode
    CLASS(eis_parser), POINTER :: parser
    CLASS(eis_stack), POINTER :: stack
    TYPE(eis_populate_stack_params), POINTER :: p
    TYPE(eis_populate_stack_params), TARGET :: defaults

    errcode = INT(eis_err_none, eis_error_c)

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_populate_stack_params_init(defaults)
      p => defaults
    END IF

    psimp => NULL()
    pmini => NULL()
    patxt => NULL()
    pfname => NULL()
    IF (p%simplify >= 0) THEN
      simp = (p%simplify /= 0)
      psimp => simp
    END IF
    IF (p%minify >= 0) THEN
      mini = (p%minify /= 0)
      pmini => mini
    END IF
    IF (p%allow_text >= 0) THEN
      atxt = (p%allow_text /= 0)
      patxt => atxt
    END IF
    IF (C_ASSOCIATED(p%filename)) THEN
      CALL eis_c_f_string(p%filename, fname)
      pfname => fname
    END IF

    CALL eis_c_f_string(expression, fstring)
    f_errcode = eis_err_none
    stack => eis_get_interop_stack(stack_id, parser = parser)
    IF (ASSOCIATED(stack) .AND. ASSOCIATED(parser)) THEN
      CALL parser%tokenize(fstring, stack, f_errcode, simplify = psimp, &
          minify = pmini, filename = pfname, &
          line_number = INT(p%line_number), char_offset = INT(p%char_offset), &
          append = .FALSE., allow_text = patxt)
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
  !> @param[in] params - Pointer to an eis_empty_stack_params structure. NULL
  !> gives the default parameters. Currently unused
  SUBROUTINE eis_empty_stack(stack_id, errcode, params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: stack_id
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    CLASS(eis_parser), POINTER :: parser
    CLASS(eis_stack), POINTER :: stack

    IF (C_ASSOCIATED(params)) CONTINUE

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
  !> @param[in] params - Pointer to an eis_bind_result_to_stack_params
  !> structure. NULL gives the default parameters
  SUBROUTINE eis_bind_result_to_stack(stack_id, fn, errcode, params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: stack_id
    TYPE(C_FUNPTR), VALUE, INTENT(IN) :: fn
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    CHARACTER(LEN=:), ALLOCATABLE, TARGET :: fname
    CHARACTER(LEN=:), POINTER :: pfname
    CLASS(eis_parser), POINTER :: parser
    CLASS(eis_stack), POINTER :: stack
    PROCEDURE(parser_result_function), POINTER :: result_fn_ptr
    TYPE(eis_bind_result_to_stack_params), POINTER :: p
    TYPE(eis_bind_result_to_stack_params), TARGET :: defaults

    errcode = INT(eis_err_none, eis_error_c)

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_bind_result_to_stack_params_init(defaults)
      p => defaults
    END IF

    pfname => NULL()
    IF (C_ASSOCIATED(p%filename)) THEN
      CALL eis_c_f_string(p%filename, fname)
      pfname => fname
    END IF

    stack => eis_get_interop_stack(stack_id, parser = parser)
    CALL C_F_PROCPOINTER(fn, result_fn_ptr)
    IF (ASSOCIATED(stack) .AND. ASSOCIATED(parser)) THEN
      CALL parser%set_result_function(result_fn_ptr, stack, errcode, &
          filename = pfname, line_number = INT(p%line_number), &
          char_offset = INT(p%char_offset), &
          cap_bits = INT(p%cap_bits, eis_bitmask), append = (p%append /= 0))
    ELSE
      IF (.NOT. ASSOCIATED(stack)) errcode = IOR(errcode, &
          INT(eis_err_bad_stack, eis_error_c))
      IF (.NOT. ASSOCIATED(parser)) errcode = IOR(errcode, &
          INT(eis_err_bad_parser, eis_error_c))
    END IF

  END SUBROUTINE eis_bind_result_to_stack



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
  !> @param[in] host_params - Pointer to C interoperable structure that is
  !> passed to all function that are called as expression keys are evaluated
  !> @param[inout] errcode - Error code
  !> @param[in] params - Pointer to an eis_evaluate_stack_params structure.
  !> NULL gives the default parameters. Currently unused
  !> @return eis_evaluate_stack - Number of results actually returned
  FUNCTION eis_evaluate_stack(stack_id, res_len, result, host_params, errcode, &
      params) BIND(C)

    INTEGER(C_INT), VALUE :: stack_id, res_len
    REAL(eis_num_c), DIMENSION(res_len), INTENT(OUT) :: result
    TYPE(C_PTR), VALUE :: host_params
    INTEGER(eis_error_c), INTENT(OUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    INTEGER(C_INT) :: eis_evaluate_stack
    REAL(eis_num), DIMENSION(:), ALLOCATABLE :: f_result
    INTEGER(eis_error) :: f_errcode
    INTEGER :: returned_results, copied_results

    IF (C_ASSOCIATED(params)) CONTINUE

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
  !> C interoperable function to add a function associated with a name that can
  !> be called from a parser expression
  !> @param[in] parser_id - ID of parser to add the function to
  !> @param[in] name - Name to associate function with in expression
  !> @param[in] fn - Pointer to function to be called when the name is
  !> encountered. If fn is NULL then the function will be an EIS deferred
  !> function
  !> @param[in] expected_params - Expected number of parameters for the
  !> function. If <0 then the function is variadic
  !> @param[inout] errcode - Error code
  !> @param[in] params - Pointer to an eis_add_function_params structure. NULL
  !> gives the default parameters
  SUBROUTINE eis_add_function(parser_id, name, fn, expected_params, errcode, &
      params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    TYPE(C_PTR), VALUE :: name
    TYPE(C_FUNPTR), VALUE :: fn
    INTEGER(C_INT), VALUE, INTENT(IN) :: expected_params
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    CLASS(eis_parser), POINTER :: parser
    CHARACTER(LEN=:), ALLOCATABLE :: fstring
    CHARACTER(LEN=:), ALLOCATABLE, TARGET :: dstring
    CHARACTER(LEN=:), POINTER :: pdesc
    PROCEDURE(parser_eval_fn), POINTER :: fn_ptr
    TYPE(eis_add_function_params), POINTER :: p
    TYPE(eis_add_function_params), TARGET :: defaults

    errcode = INT(eis_err_none, eis_error_c)

    IF (parser_id < 1 .OR. parser_id > interop_parser_count) RETURN
    parser => interop_parsers(parser_id)%contents

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_add_function_params_init(defaults)
      p => defaults
    END IF

    pdesc => NULL()
    IF (C_ASSOCIATED(p%description)) THEN
      CALL eis_c_f_string(p%description, dstring)
      pdesc => dstring
    END IF

    IF (C_ASSOCIATED(fn)) THEN
      CALL C_F_PROCPOINTER(fn, fn_ptr)
    ELSE
      fn_ptr => eis_dummy
    END IF

    CALL eis_c_f_string(name, fstring)
    CALL parser%add_function(fstring, fn_ptr, errcode, &
        cap_bits = INT(p%cap_bits, eis_bitmask), &
        expected_params = INT(expected_params), &
        can_simplify = (p%can_simplify /= 0), &
        defer = ((p%defer /= 0) .OR. .NOT. C_ASSOCIATED(fn)), &
        global = (p%global /= 0), description = pdesc, &
        hidden = (p%hidden /= 0), text_params = (p%text_params /= 0), &
        is_idempotent = (p%is_idempotent /= 0), &
        text_param_mask = INT(p%text_param_mask, eis_bitmask))
    DEALLOCATE(fstring)

  END SUBROUTINE eis_add_function



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to add an emplaced function
  !> @param[in] parser_id - ID of parser to add the function to
  !> @param[in] name - Name to associate function with in expression
  !> @param[in] fn - Pointer to function to be called for the emplacement
  !> action. Must not be NULL
  !> @param[in] expected_params - Expected number of parameters for the
  !> function. If <0 then the function is variadic
  !> @param[inout] errcode - Error code
  !> @param[in] params - Pointer to an eis_add_emplaced_function_params
  !> structure. NULL gives the default parameters
  SUBROUTINE eis_add_emplaced_function(parser_id, name, fn, expected_params, &
      errcode, params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    TYPE(C_PTR), VALUE, INTENT(IN) :: name
    TYPE(C_FUNPTR), VALUE, INTENT(IN) :: fn
    INTEGER(C_INT), VALUE, INTENT(IN) :: expected_params
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    PROCEDURE(parser_late_bind_interop_fn), POINTER :: late_bind_fn_ptr
    INTEGER(eis_error) :: errcode_f
    CLASS(eis_parser), POINTER :: parser
    CHARACTER(LEN=:), ALLOCATABLE :: name_f
    CHARACTER(LEN=:), ALLOCATABLE, TARGET :: dstring
    CHARACTER(LEN=:), POINTER :: pdesc
    TYPE(eis_add_emplaced_function_params), POINTER :: p
    TYPE(eis_add_emplaced_function_params), TARGET :: defaults

    errcode = INT(eis_err_none, eis_error_c)

    parser => eis_get_interop_parser(parser_id)
    IF (.NOT. ASSOCIATED(parser)) RETURN

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_add_emplaced_function_params_init(defaults)
      p => defaults
    END IF

    pdesc => NULL()
    IF (C_ASSOCIATED(p%description)) THEN
      CALL eis_c_f_string(p%description, dstring)
      pdesc => dstring
    END IF

    CALL eis_c_f_string(name, name_f)
    CALL C_F_PROCPOINTER(fn, late_bind_fn_ptr)
    errcode_f = eis_err_none
    CALL parser%add_emplaced_c_function(name_f, late_bind_fn_ptr, errcode_f, &
        expected_params = INT(expected_params), description = pdesc, &
        hidden = (p%hidden /= 0))

    errcode = IOR(errcode, INT(errcode_f, eis_error_c))

  END SUBROUTINE eis_add_emplaced_function



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to cause emplacement of emplaced functions
  !> @param[in] stack_id - ID of stack to emplace
  !> @param[in] host_params - Host parameters
  !> @param[inout] errcode - Error code
  !> @param[in] params - Pointer to an eis_emplace_stack_params structure. NULL
  !> gives the default parameters
  SUBROUTINE eis_emplace_stack(stack_id, host_params, errcode, params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: stack_id
    TYPE(C_PTR), VALUE, INTENT(IN) :: host_params
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    INTEGER(eis_error) :: errcode_f
    CLASS(eis_parser), POINTER :: parser
    TYPE(eis_stack), POINTER :: stack, stack_dest
    TYPE(eis_emplace_stack_params), POINTER :: p
    TYPE(eis_emplace_stack_params), TARGET :: defaults

    errcode = INT(eis_err_none, eis_error_c)

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_emplace_stack_params_init(defaults)
      p => defaults
    END IF

    stack => eis_get_interop_stack(stack_id, parser = parser)
    stack_dest => eis_get_interop_stack(INT(p%stack_id_out))
    errcode_f = eis_err_none
    IF (ASSOCIATED(parser) .AND. ASSOCIATED(stack)) THEN
      CALL parser%emplace(stack, errcode_f, host_params = host_params, &
          destination = stack_dest)
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
  !> @param[in] expected_params - Expected number of parameters for the
  !> functor. If <0 then the functor is variadic
  !> @param[inout] errcode - Error code
  !> @param[in] params - Pointer to an eis_add_functor_params structure. NULL
  !> gives the default parameters
  SUBROUTINE eis_add_functor(parser_id, name, fn, bound_data, &
      expected_params, errcode, params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    TYPE(C_PTR), VALUE :: name
    TYPE(C_FUNPTR), VALUE :: fn
    TYPE(C_PTR), VALUE :: bound_data
    INTEGER(C_INT), VALUE, INTENT(IN) :: expected_params
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    CLASS(eis_parser), POINTER :: parser
    CHARACTER(LEN=:), ALLOCATABLE :: fstring
    CHARACTER(LEN=:), ALLOCATABLE, TARGET :: dstring
    CHARACTER(LEN=:), POINTER :: pdesc
    PROCEDURE(c_functor_function), POINTER :: functor_fn_ptr
    TYPE(interop_functor) :: ifunc
    TYPE(eis_add_functor_params), POINTER :: p
    TYPE(eis_add_functor_params), TARGET :: defaults

    errcode = INT(eis_err_none, eis_error_c)

    IF (parser_id < 1 .OR. parser_id > interop_parser_count) RETURN
    parser => interop_parsers(parser_id)%contents

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_add_functor_params_init(defaults)
      p => defaults
    END IF

    pdesc => NULL()
    IF (C_ASSOCIATED(p%description)) THEN
      CALL eis_c_f_string(p%description, dstring)
      pdesc => dstring
    END IF

    IF (C_ASSOCIATED(fn)) THEN
      CALL C_F_PROCPOINTER(fn, functor_fn_ptr)
    ELSE
      functor_fn_ptr => NULL()
    END IF

    ifunc%bound_data = bound_data
    ifunc%c_bound_function => functor_fn_ptr

    CALL eis_c_f_string(name, fstring)
    CALL parser%add_functor(fstring, ifunc, errcode, &
        cap_bits = INT(p%cap_bits, eis_bitmask), &
        expected_params = INT(expected_params), &
        can_simplify = (p%can_simplify /= 0), &
        defer = ((p%defer /= 0) .OR. .NOT. C_ASSOCIATED(fn)), &
        global = (p%global /= 0), description = pdesc, &
        hidden = (p%hidden /= 0), text_params = (p%text_params /= 0), &
        is_idempotent = (p%is_idempotent /= 0), &
        text_param_mask = INT(p%text_param_mask, eis_bitmask))
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
  !> @param[inout] errcode - Error code
  !> @param[in] params - Pointer to an eis_add_variable_params structure. NULL
  !> gives the default parameters
  SUBROUTINE eis_add_variable(parser_id, name, fn, errcode, params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    TYPE(C_PTR), VALUE :: name
    TYPE(C_FUNPTR), VALUE :: fn
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    CLASS(eis_parser), POINTER :: parser
    CHARACTER(LEN=:), ALLOCATABLE :: fstring
    CHARACTER(LEN=:), ALLOCATABLE, TARGET :: dstring
    CHARACTER(LEN=:), POINTER :: pdesc
    PROCEDURE(parser_eval_fn), POINTER :: fn_ptr
    TYPE(eis_add_variable_params), POINTER :: p
    TYPE(eis_add_variable_params), TARGET :: defaults

    errcode = INT(eis_err_none, eis_error_c)

    IF (parser_id < 1 .OR. parser_id > interop_parser_count) RETURN
    parser => interop_parsers(parser_id)%contents

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_add_variable_params_init(defaults)
      p => defaults
    END IF

    pdesc => NULL()
    IF (C_ASSOCIATED(p%description)) THEN
      CALL eis_c_f_string(p%description, dstring)
      pdesc => dstring
    END IF

    IF (C_ASSOCIATED(fn)) THEN
      CALL C_F_PROCPOINTER(fn, fn_ptr)
    ELSE
      fn_ptr => eis_dummy
    END IF

    CALL eis_c_f_string(name, fstring)
    CALL parser%add_variable(fstring, fn_ptr, errcode, &
        cap_bits = INT(p%cap_bits, eis_bitmask), &
        can_simplify = (p%can_simplify /= 0), &
        defer = ((p%defer /= 0) .OR. .NOT. C_ASSOCIATED(fn)), &
        global = (p%global /= 0), description = pdesc, &
        hidden = (p%hidden /= 0))
    DEALLOCATE(fstring)

  END SUBROUTINE eis_add_variable



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to add a stack variable
  !> @param[in] parser_id - ID of parser to add the variable to
  !> @param[in] name - Name to associate variable with in expression
  !> @param[in] stack_id - Stack to be returned when the variable is encountered
  !> If -1 then this will be a deferred variable
  !> @param[inout] errcode - Error code
  !> @param[in] params - Pointer to an eis_add_stack_variable_params
  !> structure. NULL gives the default parameters
  SUBROUTINE eis_add_stack_variable(parser_id, name, stack_id, errcode, &
      params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    TYPE(C_PTR), VALUE :: name
    INTEGER(C_INT), VALUE, INTENT(IN) :: stack_id
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    CLASS(eis_parser), POINTER :: parser
    CHARACTER(LEN=:), ALLOCATABLE :: fstring
    CHARACTER(LEN=:), ALLOCATABLE, TARGET :: dstring
    CHARACTER(LEN=:), POINTER :: pdesc
    TYPE(eis_stack), POINTER :: stack
    TYPE(eis_add_stack_variable_params), POINTER :: p
    TYPE(eis_add_stack_variable_params), TARGET :: defaults

    errcode = INT(eis_err_none, eis_error_c)

    IF (parser_id < 1 .OR. parser_id > interop_parser_count) RETURN
    parser => eis_get_interop_parser(parser_id)
    stack => eis_get_interop_stack(stack_id)

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_add_stack_variable_params_init(defaults)
      p => defaults
    END IF

    pdesc => NULL()
    IF (C_ASSOCIATED(p%description)) THEN
      CALL eis_c_f_string(p%description, dstring)
      pdesc => dstring
    END IF

    CALL eis_c_f_string(name, fstring)
    IF (ASSOCIATED(stack)) THEN
      CALL parser%add_stack_variable(fstring, stack, errcode, &
          defer = (p%defer /= 0), global = (p%global /= 0), &
          description = pdesc, hidden = (p%hidden /= 0))
    ELSE
      CALL parser%add_stack_variable(fstring, errcode, &
          global = (p%global /= 0), description = pdesc, &
          hidden = (p%hidden /= 0))
    END IF
    DEALLOCATE(fstring)

  END SUBROUTINE eis_add_stack_variable



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to add a stack variable specified as a string
  !> @param[in] parser_id - ID of parser to add the variable to
  !> @param[in] name - Name to associate variable with in expression
  !> @param[in] stack_text - Stack text to be returned when the variable is
  !> encountered. If NULL then this will be a deferred variable
  !> @param[inout] errcode - Error code
  !> @param[in] params - Pointer to an eis_add_stack_expression_variable_params
  !> structure. NULL gives the default parameters
  SUBROUTINE eis_add_stack_expression_variable(parser_id, name, stack_text, &
      errcode, params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    TYPE(C_PTR), VALUE :: name
    TYPE(C_PTR), VALUE :: stack_text
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    CLASS(eis_parser), POINTER :: parser
    CHARACTER(LEN=:), ALLOCATABLE :: fstring, stack_string
    CHARACTER(LEN=:), ALLOCATABLE, TARGET :: dstring
    CHARACTER(LEN=:), POINTER :: pdesc
    TYPE(eis_add_stack_expression_variable_params), POINTER :: p
    TYPE(eis_add_stack_expression_variable_params), TARGET :: defaults

    errcode = INT(eis_err_none, eis_error_c)

    IF (parser_id < 1 .OR. parser_id > interop_parser_count) RETURN
    parser => eis_get_interop_parser(parser_id)

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_add_stack_expression_variable_params_init(defaults)
      p => defaults
    END IF

    pdesc => NULL()
    IF (C_ASSOCIATED(p%description)) THEN
      CALL eis_c_f_string(p%description, dstring)
      pdesc => dstring
    END IF

    CALL eis_c_f_string(name, fstring)
    IF (C_ASSOCIATED(stack_text)) THEN
      CALL eis_c_f_string(stack_text, stack_string)
      CALL parser%add_stack_variable(fstring, stack_string, errcode, &
          global = (p%global /= 0), description = pdesc, &
          hidden = (p%hidden /= 0))
    ELSE
      CALL parser%add_stack_variable(fstring, errcode, &
          global = (p%global /= 0), description = pdesc, &
          hidden = (p%hidden /= 0))
    END IF
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
  !> @param[inout] errcode - Error code
  !> @param[in] params - Pointer to an eis_add_pointer_variable_params
  !> structure. NULL gives the default parameters
  SUBROUTINE eis_add_pointer_variable(parser_id, name, ptr, ptr_is_integer, &
      ptr_is_64_bit, errcode, params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    TYPE(C_PTR), VALUE :: name
    TYPE(C_PTR), VALUE :: ptr
    INTEGER(C_INT), VALUE, INTENT(IN) :: ptr_is_integer
    INTEGER(C_INT), VALUE, INTENT(IN) :: ptr_is_64_bit
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    CLASS(eis_parser), POINTER :: parser
    CHARACTER(LEN=:), ALLOCATABLE :: fstring
    CHARACTER(LEN=:), ALLOCATABLE, TARGET :: dstring
    CHARACTER(LEN=:), POINTER :: pdesc
    TYPE(eis_add_pointer_variable_params), POINTER :: p
    TYPE(eis_add_pointer_variable_params), TARGET :: defaults

    errcode = INT(eis_err_none, eis_error_c)

    IF (parser_id < 1 .OR. parser_id > interop_parser_count) RETURN
    parser => interop_parsers(parser_id)%contents

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_add_pointer_variable_params_init(defaults)
      p => defaults
    END IF

    pdesc => NULL()
    IF (C_ASSOCIATED(p%description)) THEN
      CALL eis_c_f_string(p%description, dstring)
      pdesc => dstring
    END IF

    CALL eis_c_f_string(name, fstring)
    CALL parser%add_pointer_variable(fstring, ptr, ptr_is_integer /= 0, &
        ptr_is_64_bit /= 0, errcode, &
        cap_bits = INT(p%cap_bits, eis_bitmask), &
        can_simplify = (p%can_simplify /= 0), &
        defer = ((p%defer /= 0) .OR. .NOT. C_ASSOCIATED(ptr)), &
        global = (p%global /= 0), description = pdesc, &
        hidden = (p%hidden /= 0))
    DEALLOCATE(fstring)

  END SUBROUTINE eis_add_pointer_variable



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to add a constant associated with a name that can
  !> be called from a parser expression
  !> @param[in] parser_id - ID of parser to add the variable to
  !> @param[in] name - Name to associate variable with in expression
  !> @param[in] value - Value to associate with name
  !> @param[inout] errcode - Error code
  !> @param[in] params - Pointer to an eis_add_constant_params structure. NULL
  !> gives the default parameters
  SUBROUTINE eis_add_constant(parser_id, name, value, errcode, params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    TYPE(C_PTR), VALUE :: name
    REAL(eis_num_c), VALUE :: value
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    CLASS(eis_parser), POINTER :: parser
    CHARACTER(LEN=:), ALLOCATABLE :: fstring
    CHARACTER(LEN=:), ALLOCATABLE, TARGET :: dstring
    CHARACTER(LEN=:), POINTER :: pdesc
    TYPE(eis_add_constant_params), POINTER :: p
    TYPE(eis_add_constant_params), TARGET :: defaults

    errcode = INT(eis_err_none, eis_error_c)

    IF (parser_id < 1 .OR. parser_id > interop_parser_count) RETURN
    parser => interop_parsers(parser_id)%contents

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_add_constant_params_init(defaults)
      p => defaults
    END IF

    pdesc => NULL()
    IF (C_ASSOCIATED(p%description)) THEN
      CALL eis_c_f_string(p%description, dstring)
      pdesc => dstring
    END IF

    CALL eis_c_f_string(name, fstring)
    CALL parser%add_constant(fstring, value, errcode, &
        cap_bits = INT(p%cap_bits, eis_bitmask), &
        can_simplify = (p%can_simplify /= 0), defer = (p%defer /= 0), &
        global = (p%global /= 0), description = pdesc, &
        hidden = (p%hidden /= 0))
    DEALLOCATE(fstring)

  END SUBROUTINE eis_add_constant



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to add a deferred constant
  !> @param[in] parser_id - ID of parser to add the variable to
  !> @param[in] name - Name to associate variable with in expression
  !> @param[inout] errcode - Error code
  !> @param[in] params - Pointer to an eis_add_deferred_constant_params
  !> structure. NULL gives the default parameters
  SUBROUTINE eis_add_deferred_constant(parser_id, name, errcode, params) &
      BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    TYPE(C_PTR), VALUE :: name
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    CLASS(eis_parser), POINTER :: parser
    CHARACTER(LEN=:), ALLOCATABLE :: fstring
    CHARACTER(LEN=:), ALLOCATABLE, TARGET :: dstring
    CHARACTER(LEN=:), POINTER :: pdesc
    TYPE(eis_add_deferred_constant_params), POINTER :: p
    TYPE(eis_add_deferred_constant_params), TARGET :: defaults

    errcode = INT(eis_err_none, eis_error_c)

    IF (parser_id < 1 .OR. parser_id > interop_parser_count) RETURN
    parser => interop_parsers(parser_id)%contents

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_add_deferred_constant_params_init(defaults)
      p => defaults
    END IF

    pdesc => NULL()
    IF (C_ASSOCIATED(p%description)) THEN
      CALL eis_c_f_string(p%description, dstring)
      pdesc => dstring
    END IF

    CALL eis_c_f_string(name, fstring)
    CALL parser%add_constant(fstring, errcode, &
        cap_bits = INT(p%cap_bits, eis_bitmask), &
        can_simplify = (p%can_simplify /= 0), global = (p%global /= 0), &
        description = pdesc, hidden = (p%hidden /= 0))
    DEALLOCATE(fstring)

  END SUBROUTINE eis_add_deferred_constant



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to get the number of errors caused by evaluating a
  !> stack
  !> @param[in] parser_id - ID of stored parser to count errors in
  !> @param[in] params - Pointer to an eis_parser_get_error_count_params
  !> structure. NULL gives the default parameters. Currently unused
  !> @return eis_get_error_count - Number of errors in the error handler
  FUNCTION eis_parser_get_error_count(parser_id, params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    INTEGER(C_INT) :: eis_parser_get_error_count
    CLASS(eis_parser), POINTER :: parser

    IF (C_ASSOCIATED(params)) CONTINUE

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
  !> @param[in] params - Pointer to an eis_parser_get_error_report_params
  !> structure. NULL gives the default parameters. Currently unused
  !> @return eis_get_error_report - Length of the error report + 1. If you
  !> allocate a string at least this long it will be able to hold the
  !> full error report and the null terminator character
  FUNCTION eis_parser_get_error_report(parser_id, error_id, buflen, &
      string_out, params) BIND(C)
    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id, error_id, buflen
    TYPE(C_PTR), VALUE, INTENT(IN) :: string_out
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    INTEGER(C_INT) :: eis_parser_get_error_report
    CHARACTER(LEN=:), ALLOCATABLE :: errstr
    CLASS(eis_parser), POINTER :: parser

    IF (C_ASSOCIATED(params)) CONTINUE

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
  !> @param[in] params - Pointer to an eis_parser_flush_errors_params
  !> structure. NULL gives the default parameters. Currently unused
  SUBROUTINE eis_parser_flush_errors(parser_id, params) BIND(C)
    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    CLASS(eis_parser), POINTER :: parser

    IF (C_ASSOCIATED(params)) CONTINUE

    parser => eis_get_interop_parser(parser_id)
    IF (.NOT. ASSOCIATED(parser)) RETURN
    CALL parser%flush_errors()

  END SUBROUTINE eis_parser_flush_errors



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Increment the reference count for a stack. Use to indicate
  !> that another part of your code wants to use a given stored stack
  !> @param[in] stack_id - ID of stored stack to increment the reference of
  !> @param[in] params - Pointer to an eis_stack_inc_ref_params structure. NULL
  !> gives the default parameters. Currently unused
  SUBROUTINE eis_stack_inc_ref(stack_id, params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: stack_id
    TYPE(C_PTR), VALUE, INTENT(IN) :: params

    IF (C_ASSOCIATED(params)) CONTINUE

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
  !> @param[in] params - Pointer to an eis_parser_inc_ref_params structure. NULL
  !> gives the default parameters. Currently unused
  SUBROUTINE eis_parser_inc_ref(parser_id, params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    TYPE(C_PTR), VALUE, INTENT(IN) :: params

    IF (C_ASSOCIATED(params)) CONTINUE

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
  !> @param[in] params - Pointer to an eis_stack_dec_ref_params structure. NULL
  !> gives the default parameters. Currently unused
  SUBROUTINE eis_stack_dec_ref(stack_id, params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: stack_id
    TYPE(C_PTR), VALUE, INTENT(IN) :: params

    IF (C_ASSOCIATED(params)) CONTINUE

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
  !> @param[in] params - Pointer to an eis_parser_dec_ref_params structure. NULL
  !> gives the default parameters. Currently unused
  SUBROUTINE eis_parser_dec_ref(parser_id, params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    TYPE(C_PTR), VALUE, INTENT(IN) :: params

    IF (C_ASSOCIATED(params)) CONTINUE

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
  !> @param[in] params - Pointer to an eis_copy_stack_params structure. NULL
  !> gives the default parameters. Currently unused
  !> @return eis_copy_stack - ID of copy of stack
  FUNCTION eis_copy_stack(stack_id, params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: stack_id
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    INTEGER(C_INT) :: eis_copy_stack

    IF (C_ASSOCIATED(params)) CONTINUE

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
  !> @param[in] params - Pointer to an eis_visualise_stack_params structure.
  !> NULL gives the default parameters. The formatstr parameter is a fortran
  !> REAL format string to be used for describing numerical literals. If NULL
  !> then a default format is used
  !> @return eis_visualise_stack - Number of characters actually returned
  !> from the visualisation. If this is less than text_length then
  !> the function should be run again with a buffer large enough to
  !> contain this many characters or the dot structure will be invalid
  FUNCTION eis_visualise_stack(stack_id, text_length, text, params) BIND(C)
    INTEGER(C_INT), VALUE, INTENT(IN) :: stack_id
    INTEGER(C_INT), VALUE, INTENT(IN) :: text_length
    TYPE(C_PTR), VALUE, INTENT(IN) :: text
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    INTEGER(C_INT) :: eis_visualise_stack
    CHARACTER(LEN=:), ALLOCATABLE :: fstr
    CHARACTER(LEN=:), ALLOCATABLE, TARGET :: fform
    CHARACTER(LEN=:), POINTER :: pform
    CLASS(eis_parser), POINTER :: parser
    CLASS(eis_stack), POINTER :: stack
    TYPE(eis_visualise_stack_params), POINTER :: p
    TYPE(eis_visualise_stack_params), TARGET :: defaults

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_visualise_stack_params_init(defaults)
      p => defaults
    END IF

    pform => NULL()
    IF (C_ASSOCIATED(p%formatstr)) THEN
      CALL eis_c_f_string(p%formatstr, fform)
      pform => fform
    END IF

    stack => eis_get_interop_stack(stack_id, parser = parser)
    eis_visualise_stack = -1
    IF (ASSOCIATED(stack) .AND. ASSOCIATED(parser)) THEN
      CALL parser%visualise_stack(stack, fstr, nformat = pform)
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
  !> @param[in] params - Pointer to an eis_get_infix_stack_params structure.
  !> NULL gives the default parameters. The formatstr parameter is a fortran
  !> REAL format string to be used for describing numerical literals. If NULL
  !> then a default format is used
  !> @return eis_get_infix_stack - Number of characters actually returned
  !> from the stack. If this is less than text_length then
  !> the function should be run again with a buffer large enough to
  !> contain this many characters or the dot structure will be invalid
  FUNCTION eis_get_infix_stack(stack_id, text_length, text, params) BIND(C)
    INTEGER(C_INT), VALUE, INTENT(IN) :: stack_id
    INTEGER(C_INT), VALUE, INTENT(IN) :: text_length
    TYPE(C_PTR), VALUE, INTENT(IN) :: text
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    INTEGER(C_INT) :: eis_get_infix_stack
    CHARACTER(LEN=:), ALLOCATABLE :: fstr
    CHARACTER(LEN=:), ALLOCATABLE, TARGET :: fform
    CHARACTER(LEN=:), POINTER :: pform
    CLASS(eis_parser), POINTER :: parser
    TYPE(eis_stack), POINTER :: stack
    TYPE(eis_get_infix_stack_params), POINTER :: p
    TYPE(eis_get_infix_stack_params), TARGET :: defaults

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_get_infix_stack_params_init(defaults)
      p => defaults
    END IF

    pform => NULL()
    IF (C_ASSOCIATED(p%formatstr)) THEN
      CALL eis_c_f_string(p%formatstr, fform)
      pform => fform
    END IF

    stack => eis_get_interop_stack(stack_id, parser = parser)
    eis_get_infix_stack = -1
    IF (ASSOCIATED(stack) .AND. ASSOCIATED(parser)) THEN
      CALL parser%get_infix(stack, fstr, nformat = pform)
      IF (ALLOCATED(fstr)) THEN
        eis_get_infix_stack = LEN(fstr)
        IF (C_ASSOCIATED(text)) CALL eis_f_c_string(fstr, text_length, text)
      END IF
    END IF

  END FUNCTION eis_get_infix_stack


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for eis_get_deriv_stack to
  !> their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_get_deriv_stack_params_init(params) BIND(C)

    TYPE(eis_get_deriv_stack_params), INTENT(OUT) :: params

    params%dummy = 0

  END SUBROUTINE eis_get_deriv_stack_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to calculate a stack that contains the derivative
  !> of another stack
  !> @param[in] stack_id - ID of the stack to take the derivative of
  !> @param[in] output_id - ID of the stack to hold the derivative. This stack
  !> must already exist and its previous contents are replaced
  !> @param[in] wrt - String naming the symbol that the derivative is with
  !> respect to
  !> @param[inout] errcode - Error code
  !> @param[in] params - Pointer to an eis_get_deriv_stack_params structure.
  !> NULL gives the default parameters. Currently unused
  SUBROUTINE eis_get_deriv_stack(stack_id, output_id, wrt, errcode, params) &
      BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: stack_id
    INTEGER(C_INT), VALUE, INTENT(IN) :: output_id
    TYPE(C_PTR), VALUE, INTENT(IN) :: wrt
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    CHARACTER(LEN=:), ALLOCATABLE :: wrt_f
    INTEGER(eis_error) :: errcode_f
    CLASS(eis_parser), POINTER :: parser
    CLASS(eis_stack), POINTER :: stack, output

    IF (C_ASSOCIATED(params)) CONTINUE

    errcode = INT(eis_err_none, eis_error_c)
    stack => eis_get_interop_stack(stack_id, parser = parser)
    output => eis_get_interop_stack(output_id)
    IF (ASSOCIATED(stack) .AND. ASSOCIATED(output) .AND. ASSOCIATED(parser) &
        .AND. C_ASSOCIATED(wrt)) THEN
      CALL eis_c_f_string(wrt, wrt_f)
      errcode_f = eis_err_none
      CALL parser%get_deriv_stack(stack, output, wrt_f, errcode_f)
      errcode = IOR(errcode, INT(errcode_f, eis_error_c))
    ELSE
      IF (.NOT. ASSOCIATED(stack) .OR. .NOT. ASSOCIATED(output)) &
          errcode = IOR(errcode, INT(eis_err_bad_stack, eis_error_c))
      IF (.NOT. ASSOCIATED(parser)) errcode = IOR(errcode, &
          INT(eis_err_bad_parser, eis_error_c))
      IF (.NOT. C_ASSOCIATED(wrt)) errcode = IOR(errcode, &
          INT(eis_err_bad_value, eis_error_c))
    END IF

  END SUBROUTINE eis_get_deriv_stack


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_set_symbol_derivative to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_set_symbol_derivative_params_init(params) BIND(C)

    TYPE(eis_set_symbol_derivative_params), INTENT(OUT) :: params

    params%global = 0

  END SUBROUTINE eis_set_symbol_derivative_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_lock_symbol_derivative to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_lock_symbol_derivative_params_init(params) BIND(C)

    TYPE(eis_lock_symbol_derivative_params), INTENT(OUT) :: params

    params%global = 0

  END SUBROUTINE eis_lock_symbol_derivative_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the derivative of a symbol
  !> @param[in] parser_id - ID of the parser that holds the symbol
  !> @param[in] symbol - Name of the symbol to set the derivative of
  !> @param[in] deriv - Parser expression for the derivative
  !> @param[in] wrt - Name of the symbol that the derivative is with respect to.
  !> If NULL then the derivative applies to any variable
  !> @param[inout] errcode - Error code
  !> @param[in] params - Pointer to an eis_set_symbol_derivative_params
  !> structure. NULL gives the default parameters
  SUBROUTINE eis_set_symbol_derivative(parser_id, symbol, deriv, wrt, errcode, &
      params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    TYPE(C_PTR), VALUE, INTENT(IN) :: symbol
    TYPE(C_PTR), VALUE, INTENT(IN) :: deriv
    TYPE(C_PTR), VALUE, INTENT(IN) :: wrt
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    CLASS(eis_parser), POINTER :: parser
    CHARACTER(LEN=:), ALLOCATABLE :: symbol_f, deriv_f
    CHARACTER(LEN=:), ALLOCATABLE, TARGET :: wrt_f
    CHARACTER(LEN=:), POINTER :: pwrt
    INTEGER(eis_error) :: errcode_f
    TYPE(eis_set_symbol_derivative_params), POINTER :: p
    TYPE(eis_set_symbol_derivative_params), TARGET :: defaults

    errcode = INT(eis_err_none, eis_error_c)

    parser => eis_get_interop_parser(parser_id)
    IF (.NOT. ASSOCIATED(parser)) THEN
      errcode = IOR(errcode, INT(eis_err_bad_parser, eis_error_c))
      RETURN
    END IF
    IF (.NOT. C_ASSOCIATED(symbol) .OR. .NOT. C_ASSOCIATED(deriv)) THEN
      errcode = IOR(errcode, INT(eis_err_bad_value, eis_error_c))
      RETURN
    END IF

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_set_symbol_derivative_params_init(defaults)
      p => defaults
    END IF

    pwrt => NULL()
    IF (C_ASSOCIATED(wrt)) THEN
      CALL eis_c_f_string(wrt, wrt_f)
      pwrt => wrt_f
    END IF

    CALL eis_c_f_string(symbol, symbol_f)
    CALL eis_c_f_string(deriv, deriv_f)
    errcode_f = eis_err_none
    CALL parser%set_symbol_derivative(symbol_f, deriv_f, errcode_f, &
        wrt = pwrt, global = (p%global /= 0))
    errcode = IOR(errcode, INT(errcode_f, eis_error_c))

  END SUBROUTINE eis_set_symbol_derivative



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to lock the derivative of a symbol, marking it as
  !> having no meaningful derivative
  !> @param[in] parser_id - ID of the parser that holds the symbol
  !> @param[in] symbol - Name of the symbol to lock the derivative of
  !> @param[in] wrt - Name of the symbol that the derivative is with respect to.
  !> If NULL then the lock applies to any variable
  !> @param[inout] errcode - Error code
  !> @param[in] params - Pointer to an eis_lock_symbol_derivative_params
  !> structure. NULL gives the default parameters
  SUBROUTINE eis_lock_symbol_derivative(parser_id, symbol, wrt, errcode, &
      params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: parser_id
    TYPE(C_PTR), VALUE, INTENT(IN) :: symbol
    TYPE(C_PTR), VALUE, INTENT(IN) :: wrt
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    CLASS(eis_parser), POINTER :: parser
    CHARACTER(LEN=:), ALLOCATABLE :: symbol_f
    CHARACTER(LEN=:), ALLOCATABLE, TARGET :: wrt_f
    CHARACTER(LEN=:), POINTER :: pwrt
    INTEGER(eis_error) :: errcode_f
    TYPE(eis_lock_symbol_derivative_params), POINTER :: p
    TYPE(eis_lock_symbol_derivative_params), TARGET :: defaults

    errcode = INT(eis_err_none, eis_error_c)

    parser => eis_get_interop_parser(parser_id)
    IF (.NOT. ASSOCIATED(parser)) THEN
      errcode = IOR(errcode, INT(eis_err_bad_parser, eis_error_c))
      RETURN
    END IF
    IF (.NOT. C_ASSOCIATED(symbol)) THEN
      errcode = IOR(errcode, INT(eis_err_bad_value, eis_error_c))
      RETURN
    END IF

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_lock_symbol_derivative_params_init(defaults)
      p => defaults
    END IF

    pwrt => NULL()
    IF (C_ASSOCIATED(wrt)) THEN
      CALL eis_c_f_string(wrt, wrt_f)
      pwrt => wrt_f
    END IF

    CALL eis_c_f_string(symbol, symbol_f)
    errcode_f = eis_err_none
    CALL parser%lock_symbol_derivative(symbol_f, errcode_f, wrt = pwrt, &
        global = (p%global /= 0))
    errcode = IOR(errcode, INT(errcode_f, eis_error_c))

  END SUBROUTINE eis_lock_symbol_derivative


END MODULE eis_parser_interop
#else
#error Unsupported EIS_C_INTEROP_VERSION
#endif
