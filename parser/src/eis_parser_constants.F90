MODULE eis_parser_constants

  USE eis_constants
  USE ISO_C_BINDING
  IMPLICIT NONE

  TYPE, ABSTRACT ::  eis_functor
    CONTAINS
    PROCEDURE(functor_eval_fn), DEFERRED :: operate
  END TYPE eis_functor

  ABSTRACT INTERFACE
    !> Function definition for the parser evaluator function
    FUNCTION parser_eval_fn(nparams, params, host_params, status_code, &
        errcode) BIND(C)
      IMPORT eis_num, eis_i4, C_PTR, eis_error, eis_status
      INTEGER(eis_i4), VALUE, INTENT(IN) :: nparams
      REAL(eis_num), DIMENSION(nparams), INTENT(IN) :: params
      TYPE(C_PTR), VALUE, INTENT(IN) :: host_params
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_error), INTENT(INOUT) :: errcode
      REAL(eis_num) :: parser_eval_fn
    END FUNCTION

    FUNCTION functor_eval_fn(this, nparams, params, host_params, status_code, &
        errcode)
      IMPORT eis_num, eis_i4, C_PTR, eis_error, eis_status, eis_functor
      CLASS(eis_functor), INTENT(INOUT) :: this
      INTEGER(eis_i4), INTENT(IN) :: nparams
      REAL(eis_num), DIMENSION(nparams), INTENT(IN) :: params
      TYPE(C_PTR), INTENT(IN) :: host_params
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_error), INTENT(INOUT) :: errcode
      REAL(eis_num) :: functor_eval_fn
    END FUNCTION

    SUBROUTINE parser_result_function(nresults, results, host_params, &
        status_code, errcode) BIND(C)
      IMPORT eis_num, eis_i4, C_PTR, eis_error, eis_status
      INTEGER(eis_i4), INTENT(INOUT) :: nresults
      REAL(eis_num), DIMENSION(nresults), INTENT(OUT) :: results
      TYPE(C_PTR), VALUE, INTENT(IN) :: host_params
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE

    FUNCTION parser_param_update_fn(user_params) BIND(C)
      IMPORT C_PTR, eis_i4
      TYPE(C_PTR), VALUE, INTENT(IN) :: user_params
      INTEGER(eis_i4) :: parser_param_update_fn
    END FUNCTION

    SUBROUTINE parser_store_data_fn(nresults, results, errcode) BIND(C)
      IMPORT eis_num, eis_i4, eis_error
      INTEGER(eis_i4), VALUE, INTENT(IN) :: nresults
      REAL(eis_num), DIMENSION(nresults) :: results
      INTEGER(eis_error), VALUE, INTENT(IN) :: errcode
    END SUBROUTINE
  END INTERFACE

  PRIVATE :: eis_element_assign, eis_element_array_assign
  INTERFACE ASSIGNMENT(=)
    MODULE PROCEDURE eis_element_assign, eis_element_array_assign
  END INTERFACE

  INTEGER, PARAMETER :: eis_physics_none = 0 !< No physical units specified
  INTEGER, PARAMETER :: eis_physics_si = 1 !< SI system
  INTEGER, PARAMETER :: eis_physics_cgs_gauss = 2 !< CGS Gaussian units

  ! block type constants
  INTEGER, PARAMETER :: eis_pt_variable = 1 !< Type variable
  INTEGER, PARAMETER :: eis_pt_constant = 2 !< Type constant
  INTEGER, PARAMETER :: eis_pt_operator = 3 !< Type operator
  INTEGER, PARAMETER :: eis_pt_function = 4 !< Type function
  INTEGER, PARAMETER :: eis_pt_parenthesis = 5 !< Type parenthesis
  INTEGER, PARAMETER :: eis_pt_separator = 6 !< Type separator
  INTEGER, PARAMETER :: eis_pt_character = 7 !< Type character
  !> Type stack variable (stored variable)
  INTEGER, PARAMETER :: eis_pt_stored_variable = 8
  INTEGER, PARAMETER :: eis_pt_deferred_variable = 9 !< Type deferred variable
  INTEGER, PARAMETER :: eis_pt_deferred_function = 10 !< Type deferred function
  INTEGER, PARAMETER :: eis_pt_emplaced_variable = 11 !< Type emplaced variable
  INTEGER, PARAMETER :: eis_pt_emplaced_function = 12 !< Type emplaced function
  INTEGER, PARAMETER :: eis_pt_pointer_variable = 13 !< Type pointer variable
  INTEGER, PARAMETER :: eis_pt_functor = 14 !< Type functor
  INTEGER, PARAMETER :: eis_pt_param = 15 !< Parameter to a deck function
  INTEGER, PARAMETER :: eis_pt_dparam = 16 !< Derivative parameter to a function
  INTEGER, PARAMETER :: eis_pt_stack_function = 17 !< Stack specified function
  INTEGER, PARAMETER :: eis_pt_zero = 18 !< Actual zero
  INTEGER, PARAMETER :: eis_pt_unity = 19 !< Actual unity
  INTEGER, PARAMETER :: eis_pt_op_plus = 20 !< Addition operator
  INTEGER, PARAMETER :: eis_pt_op_minus = 21 !< Subtraction operator
  INTEGER, PARAMETER :: eis_pt_op_multiply = 22 !< Multiplication operator
  INTEGER, PARAMETER :: eis_pt_op_divide = 23 !< Division operator
  INTEGER, PARAMETER :: eis_pt_op_power = 24 !< Raise to the power of operator
  INTEGER, PARAMETER :: eis_pt_op_unitary_plus = 25 !< Unitary plus
  INTEGER, PARAMETER :: eis_pt_op_unitary_minus = 26 !< Unitary minus
  INTEGER, PARAMETER :: eis_pt_bad = 1024 !< Block is of bad type (invalid)
  INTEGER, PARAMETER :: eis_pt_null = 1025 !< Block is of null (empty) type

  ! Associativity constants
  INTEGER, PARAMETER :: eis_assoc_null = 0 !< No associativity specified
  INTEGER, PARAMETER :: eis_assoc_a = 1 !< Fully associative operator
  INTEGER, PARAMETER :: eis_assoc_la = 2 !< Left associative operator
  INTEGER, PARAMETER :: eis_assoc_ra = 3 !< Right associative operator

  !> Parenthesis is left bracket
  INTEGER, PARAMETER :: eis_paren_left_bracket = 1
  !> Parenthesis is right bracket
  INTEGER, PARAMETER :: eis_paren_right_bracket = 2

  !> Symbol display information
  INTEGER, PARAMETER :: eis_fsn_auto = 0
  INTEGER, PARAMETER :: eis_fsn_always = 1
  INTEGER, PARAMETER :: eis_fsn_never = 2

  !> Information about a stack element that is needed during parsing but is
  !> not essential for evaluation
  TYPE eis_stack_co_element
    INTEGER :: associativity, precedence
    INTEGER :: expected_params = -1
    LOGICAL :: can_have_string_params = .FALSE.
    INTEGER :: charindex = -1
    INTEGER :: full_line_pos = -1
    LOGICAL :: defer = .FALSE.
    INTEGER(eis_bitmask) :: cap_bits = 0_eis_bitmask
    CHARACTER(LEN=:), ALLOCATABLE :: text, full_line, filename
    INTEGER :: line_number = -1
    TYPE(eis_stack), POINTER :: origin => NULL()
  END TYPE eis_stack_co_element

  !> Information about a stack element that is always needed for the entire
  !> stack lifetime
  TYPE eis_stack_element
    INTEGER :: ptype
    INTEGER :: rtype
    INTEGER :: value
    INTEGER :: actual_params = -1
    LOGICAL :: has_string_params = .FALSE.
    REAL(eis_num) :: numerical_data
    INTEGER(INT32), POINTER :: i32data => NULL()
    INTEGER(INT64), POINTER :: i64data => NULL()
    REAL(REAL32), POINTER :: r32data => NULL()
    REAL(REAL64), POINTER :: r64data => NULL()
    LOGICAL :: can_simplify = .TRUE.
    PROCEDURE(parser_eval_fn), POINTER, NOPASS :: eval_fn => NULL()
    LOGICAL :: per_stack_functor = .FALSE.
    CLASS(eis_functor), POINTER :: functor => NULL()
  END TYPE eis_stack_element

  !> Entire tokenized stack
  TYPE eis_stack
    TYPE(eis_stack_element), ALLOCATABLE :: entries(:)
    TYPE(eis_stack_co_element), ALLOCATABLE :: co_entries(:)
    PROCEDURE(parser_result_function), POINTER, NOPASS :: eval_fn => NULL()
    CHARACTER(LEN=:), ALLOCATABLE :: eval_string, full_line
    CHARACTER(LEN=:), ALLOCATABLE :: filename
    INTEGER :: interop_id = -1
    INTEGER :: line_number = -1
    INTEGER :: char_offset = 0
    INTEGER :: params = 0
    INTEGER(eis_bitmask) :: cap_bits = 0_eis_bitmask
    INTEGER :: stack_point = 0, stack_size = 0
    LOGICAL :: init = .FALSE.
    LOGICAL :: sanity_checked = .FALSE.
    LOGICAL :: has_emplaced = .FALSE.
    LOGICAL :: has_deferred = .FALSE.
  END TYPE eis_stack

  INTERFACE
    !> description of the function that is used to emplace a function
    SUBROUTINE parser_late_bind_fn(orig_string, nparams, params, host_params, &
        stack_out, status_code, errcode)
      IMPORT eis_num, eis_i4, C_PTR, eis_stack, eis_error, eis_status
      CHARACTER(LEN=*), INTENT(IN) :: orig_string
      INTEGER, INTENT(IN) :: nparams
      REAL(eis_num), DIMENSION(nparams), INTENT(IN) :: params
      TYPE(C_PTR), INTENT(IN) :: host_params
      TYPE(eis_stack), INTENT(INOUT) :: stack_out
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE parser_late_bind_fn
    SUBROUTINE parser_late_bind_stack_fn(orig_string, nparams, params, &
            host_params, stack_out, status_code, errcode)
      IMPORT eis_num, eis_i4, C_PTR, eis_stack, eis_error, eis_status
      CHARACTER(LEN=*), INTENT(IN) :: orig_string
      INTEGER, INTENT(IN) :: nparams
      CLASS(eis_stack), DIMENSION(:), INTENT(IN) :: params
      TYPE(C_PTR), INTENT(IN) :: host_params
      TYPE(eis_stack), INTENT(INOUT) :: stack_out
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE parser_late_bind_stack_fn
  END INTERFACE
  !> description of the function that is used to emplace a function through the
  !> interoperability interface
  INTERFACE
    SUBROUTINE parser_late_bind_interop_fn(orig_string, nparams, params, &
        host_params, stack_id_out, status_code, errcode) BIND(C)
      IMPORT eis_num_c, C_PTR, eis_error_c, eis_status_c, C_INT
      TYPE(C_PTR), INTENT(IN) :: orig_string
      INTEGER(C_INT), VALUE, INTENT(IN) :: nparams
      REAL(eis_num_c), DIMENSION(nparams), INTENT(IN) :: params
      TYPE(C_PTR), INTENT(IN) :: host_params
      INTEGER(C_INT), INTENT(INOUT) :: stack_id_out
      INTEGER(eis_status_c), INTENT(INOUT) :: status_code
      INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    END SUBROUTINE parser_late_bind_interop_fn
    SUBROUTINE parser_late_bind_stack_interop_fn(orig_string, nparams, params, &
        host_params, stack_id_out, params_status, status_code, errcode) BIND(C)
      IMPORT eis_num_c, C_PTR, eis_error_c, eis_status_c, C_INT
      TYPE(C_PTR), INTENT(IN) :: orig_string
      INTEGER(C_INT), VALUE, INTENT(IN) :: nparams
      INTEGER(C_INT), DIMENSION(nparams), INTENT(IN) :: params
      TYPE(C_PTR), INTENT(IN) :: host_params
      INTEGER(C_INT), INTENT(INOUT) :: stack_id_out
      INTEGER(eis_status_c), DIMENSION(nparams), INTENT(INOUT) :: params_status
      INTEGER(eis_status_c), INTENT(INOUT) :: status_code
      INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    END SUBROUTINE parser_late_bind_stack_interop_fn
  END INTERFACE

  CONTAINS

  SUBROUTINE eis_element_assign(dest, src)
    TYPE(eis_stack_element), INTENT(OUT) :: dest
    TYPE(eis_stack_element), INTENT(IN) :: src

    dest%ptype = src%ptype
    dest%rtype = src%rtype
    dest%value = src%value
    dest%actual_params = src%actual_params
    dest%numerical_data = src%numerical_data
    dest%i32data => src%i32data
    dest%i64data => src%i64data
    dest%r32data => src%r32data
    dest%r64data => src%r64data
    dest%can_simplify = src%can_simplify
    dest%eval_fn => src%eval_fn
    dest%functor => src%functor

  END SUBROUTINE eis_element_assign



  SUBROUTINE eis_element_array_assign(dest, src)
    TYPE(eis_stack_element), DIMENSION(:), INTENT(OUT) :: dest
    TYPE(eis_stack_element), DIMENSION(:), INTENT(IN) :: src
    INTEGER :: i

    DO i = 1, SIZE(dest)
      dest(i)%ptype = src(i)%ptype
      dest(i)%rtype = src(i)%rtype
      dest(i)%value = src(i)%value
      dest(i)%actual_params = src(i)%actual_params
      dest(i)%numerical_data = src(i)%numerical_data
      dest(i)%i32data => src(i)%i32data
      dest(i)%i64data => src(i)%i64data
      dest(i)%r32data => src(i)%r32data
      dest(i)%r64data => src(i)%r64data
      dest(i)%can_simplify = src(i)%can_simplify
      dest(i)%eval_fn => src(i)%eval_fn
      IF (src(i)%per_stack_functor) THEN
        IF (ASSOCIATED(dest(i)%functor) .AND. dest(i)%per_stack_functor) &
            DEALLOCATE(dest(i)%functor)
        ALLOCATE(dest(i)%functor, SOURCE = src(i)%functor)
      ELSE
        dest(i)%functor => src(i)%functor
      END IF
      dest(i)%per_stack_functor = src(i)%per_stack_functor
    END DO

  END SUBROUTINE eis_element_array_assign

END MODULE eis_parser_constants
