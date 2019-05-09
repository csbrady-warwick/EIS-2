MODULE eis_header

  USE eis_constants
  USE ISO_C_BINDING

  INTERFACE
    FUNCTION parser_eval_fn(nparams, params, user_params, status_code, &
        errcode) BIND(C)
      IMPORT eis_num, eis_i4, C_PTR, eis_error, eis_status
      INTEGER(eis_i4), INTENT(IN) :: nparams
      REAL(eis_num), DIMENSION(nparams), INTENT(IN) :: params
      TYPE(C_PTR), INTENT(IN) :: user_params
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_error), INTENT(INOUT) :: errcode
      REAL(eis_num) :: parser_eval_fn
    END FUNCTION
  END INTERFACE

  INTEGER(eis_error), PARAMETER :: eis_err_none = 0 !< No error
  INTEGER(eis_error), PARAMETER :: eis_err_parser = 2**0 !< Error in parser
  INTEGER(eis_error), PARAMETER :: eis_err_simplifier = 2**1 !< Error in simplify
  INTEGER(eis_error), PARAMETER :: eis_err_emplacer = 2**2 !< Error in emplace
  INTEGER(eis_error), PARAMETER :: eis_err_evaluator = 2**3 !< Error in evaluate
  INTEGER(eis_error), PARAMETER :: eis_err_not_found = 2**4 !< Name not found
  INTEGER(eis_error), PARAMETER :: eis_err_malformed = 2**5 !< Malformed expression
  INTEGER(eis_error), PARAMETER :: eis_err_wrong_parameters = 2**6 !< Wrong number of parameters
  INTEGER(eis_error), PARAMETER :: eis_err_maths_domain = 2**7 !< Mathematically invalid
  INTEGER(eis_error), PARAMETER :: eis_err_bad_value = 2**8 !< Value that makes no sense
  INTEGER(eis_error), PARAMETER :: eis_err_has_deferred = 2**9
  INTEGER(eis_error), PARAMETER :: eis_err_has_emplaced = 2**10
  INTEGER(eis_error), PARAMETER :: eis_err_where = 2**11

  INTEGER(eis_status), PARAMETER :: eis_status_none = 0
  INTEGER(eis_status), PARAMETER :: eis_status_no_simplify = 2**0
  INTEGER(eis_status), PARAMETER :: eis_status_no_emplace = 2**1

  TYPE :: eis_indirection
    PROCEDURE(parser_eval_fn), POINTER, NOPASS :: eval_fn => NULL()
  END TYPE eis_indirection

  TYPE eis_stack_co_element
    INTEGER :: associativity, precedence
    INTEGER :: expected_params = -1
    INTEGER :: charindex = -1
    CHARACTER(LEN=:), ALLOCATABLE :: text
  END TYPE eis_stack_co_element

  TYPE eis_stack_element
    INTEGER :: ptype
    INTEGER :: value
    INTEGER :: actual_params = 0
    REAL(eis_num) :: numerical_data
    LOGICAL :: can_simplify = .TRUE.
    PROCEDURE(parser_eval_fn), POINTER, NOPASS :: eval_fn => NULL()
  END TYPE eis_stack_element

  TYPE eis_stack
    TYPE(eis_stack_element), ALLOCATABLE :: entries(:)
    TYPE(eis_stack_co_element), ALLOCATABLE :: co_entries(:)
    INTEGER(eis_bitmask) :: cap_bits = 0_eis_bitmask
    INTEGER :: stack_point, stack_size
    LOGICAL :: init = .FALSE.
    LOGICAL :: has_emplaced = .FALSE.
    LOGICAL :: where_stack = .FALSE.
  END TYPE eis_stack

  INTERFACE
    SUBROUTINE parser_late_bind_fn(nparams, params, parameters, stack_out, &
        status_code, errcode)
      IMPORT eis_num, eis_i4, C_PTR, eis_stack, eis_error, eis_status
      INTEGER(eis_i4), INTENT(IN) :: nparams
      REAL(eis_num), DIMENSION(nparams), INTENT(IN) :: params
      TYPE(C_PTR), INTENT(IN) :: parameters
      TYPE(eis_stack), INTENT(INOUT) :: stack_out
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE parser_late_bind_fn
  END INTERFACE

END MODULE eis_header
