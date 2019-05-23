MODULE eis_parser_header

  USE eis_constants
  USE ISO_C_BINDING

  INTERFACE
    !> Function definition for the parser evaluator function
    FUNCTION parser_eval_fn(nparams, params, user_params, status_code, &
        errcode) BIND(C)
      IMPORT eis_num, eis_i4, C_PTR, eis_error, eis_status
      INTEGER(eis_i4), VALUE, INTENT(IN) :: nparams
      REAL(eis_num), DIMENSION(nparams), INTENT(IN) :: params
      TYPE(C_PTR), VALUE, INTENT(IN) :: user_params
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_error), INTENT(INOUT) :: errcode
      REAL(eis_num) :: parser_eval_fn
    END FUNCTION
  END INTERFACE

  INTEGER, PARAMETER :: eis_physics_none = 0 !< No physical units specified
  INTEGER, PARAMETER :: eis_physics_si = 1 !< SI system
  INTEGER, PARAMETER :: eis_physics_cgs_gauss = 2 !< CGS Gaussian units

  !> No status specified
  INTEGER(eis_status), PARAMETER :: eis_status_none = 0
  !> This key in a stack should not be simplified through
  INTEGER(eis_status), PARAMETER :: eis_status_no_simplify = 2**0
  !> This key in a stack is an emplaced value that should not be emplaced yet
  INTEGER(eis_status), PARAMETER :: eis_status_no_emplace = 2**1

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
    INTEGER(eis_bitmask) :: cap_bits = 0_eis_bitmask
    INTEGER :: stack_point, stack_size
    LOGICAL :: init = .FALSE.
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