MODULE eis_parser_header

  USE eis_constants
  USE ISO_C_BINDING

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

    SUBROUTINE parser_result_function(nresults, results, host_params, &
        status_code, errcode) BIND(C)
      IMPORT eis_num, eis_i4, C_PTR, eis_error, eis_status
      INTEGER, INTENT(INOUT) :: nresults
      REAL(eis_num), DIMENSION(nresults), INTENT(OUT) :: results
      TYPE(C_PTR), VALUE, INTENT(IN) :: host_params
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_error), INTENT(INOUT) :: errcode
      REAL(eis_num) :: parser_eval_fn
    END SUBROUTINE

    FUNCTION parser_param_update_fn(user_params) BIND(C)
      IMPORT C_PTR
      TYPE(C_PTR), VALUE, INTENT(IN) :: user_params
      INTEGER :: parser_param_update_fn
    END FUNCTION

    SUBROUTINE parser_store_data_fn(nresults, results, errcode) BIND(C)
      IMPORT eis_num, eis_i4, eis_error
      INTEGER(eis_i4), VALUE, INTENT(IN) :: nresults
      REAL(eis_num), DIMENSION(nresults) :: results
      INTEGER(eis_error), VALUE, INTENT(IN) :: errcode
    END SUBROUTINE
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

  !> This key in a stack should not be simplified through
  INTEGER(eis_status), PARAMETER :: eis_status_no_simplify = 2**1
  !> This key in a stack is an emplaced value that should not be emplaced yet
  INTEGER(eis_status), PARAMETER :: eis_status_no_emplace = 2**2

  !> Information about a stack element that is needed during parsing but is
  !> not essential for evaluation
  TYPE eis_stack_co_element
    INTEGER :: associativity, precedence
    INTEGER :: expected_params = -1
    INTEGER :: charindex = -1
    LOGICAL :: defer = .FALSE.
    CHARACTER(LEN=:), ALLOCATABLE :: text
  END TYPE eis_stack_co_element

  !> Information about a stack element that is always needed for the entire
  !> stack lifetime
  TYPE eis_stack_element
    INTEGER :: ptype
    INTEGER :: value
    INTEGER :: actual_params = -1
    REAL(eis_num) :: numerical_data
    LOGICAL :: can_simplify = .TRUE.
    PROCEDURE(parser_eval_fn), POINTER, NOPASS :: eval_fn => NULL()
  END TYPE eis_stack_element

  !> Entire tokenized stack
  TYPE eis_stack
    TYPE(eis_stack_element), ALLOCATABLE :: entries(:)
    TYPE(eis_stack_co_element), ALLOCATABLE :: co_entries(:)
    PROCEDURE(parser_result_function), POINTER, NOPASS :: eval_fn => NULL()
    CHARACTER(LEN=:), ALLOCATABLE :: eval_string
    CHARACTER(LEN=:), ALLOCATABLE :: filename
    INTEGER :: line_number = 0
    INTEGER :: char_offset = 0
    INTEGER(eis_bitmask) :: cap_bits = 0_eis_bitmask
    INTEGER :: stack_point, stack_size
    LOGICAL :: init = .FALSE.
    LOGICAL :: sanity_checked = .FALSE.
    LOGICAL :: has_emplaced = .FALSE.
    LOGICAL :: has_deferred = .FALSE.
    LOGICAL :: where_stack = .FALSE.
  END TYPE eis_stack

  INTERFACE
    !> description of the function that is used to emplace a function
    SUBROUTINE parser_late_bind_fn(nparams, params, parameters, stack_out, &
        status_code, errcode)
      IMPORT eis_num, eis_i4, C_PTR, eis_stack, eis_error, eis_status
      INTEGER, INTENT(IN) :: nparams
      REAL(eis_num), DIMENSION(nparams), INTENT(IN) :: params
      TYPE(C_PTR), INTENT(IN) :: parameters
      TYPE(eis_stack), INTENT(INOUT) :: stack_out
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE parser_late_bind_fn
  END INTERFACE
  !> description of the function that is used to emplace a function through the
  !> interoperability interface
  INTERFACE
    SUBROUTINE parser_late_bind_interop_fn(nparams, params, parameters, &
        stack_id_out, status_code, errcode) BIND(C)
      IMPORT eis_num_c, C_PTR, eis_error_c, eis_status_c, C_INT
      INTEGER(C_INT), VALUE, INTENT(IN) :: nparams
      REAL(eis_num_c), DIMENSION(nparams), INTENT(IN) :: params
      TYPE(C_PTR), INTENT(IN) :: parameters
      INTEGER(C_INT), INTENT(INOUT) :: stack_id_out
      INTEGER(eis_status_c), INTENT(INOUT) :: status_code
      INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    END SUBROUTINE parser_late_bind_interop_fn
  END INTERFACE

END MODULE eis_parser_header
