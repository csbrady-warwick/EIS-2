MODULE eis_header

  USE eis_constants
  USE ISO_C_BINDING

  INTERFACE
    FUNCTION parser_eval_fn(nparams, params, parameters, errcode) BIND(C)
      IMPORT eis_num, eis_i8, eis_i4, C_PTR
      INTEGER(eis_i4), INTENT(IN) :: nparams
      REAL(eis_num), DIMENSION(nparams), INTENT(IN) :: params
      TYPE(C_PTR), INTENT(IN) :: parameters
      INTEGER(eis_i8), INTENT(INOUT) :: errcode
      REAL(eis_num) :: parser_eval_fn
    END FUNCTION
  END INTERFACE

  INTEGER(eis_i8), PARAMETER :: eis_err_none = 0
  INTEGER(eis_i8), PARAMETER :: eis_err_bad_value = 2**0
  INTEGER(eis_i8), PARAMETER :: eis_err_wrong_parameters = 2**1
  INTEGER(eis_i8), PARAMETER :: eis_err_maths_domain = 2**2
  INTEGER(eis_i8), PARAMETER :: eis_err_no_simplify = 2**3

  TYPE :: eis_indirection
    PROCEDURE(parser_eval_fn), POINTER, NOPASS :: eval_fn => NULL()
  END TYPE eis_indirection

  TYPE eis_stack_element
    INTEGER :: ptype
    INTEGER :: associativity, precedence
    INTEGER :: value
    INTEGER :: params = -1
    INTEGER :: actual_params = 0
    INTEGER :: output_params = 1
    REAL(eis_num) :: numerical_data
    CHARACTER(LEN=:), ALLOCATABLE :: text
    LOGICAL :: can_simplify = .TRUE.
    PROCEDURE(parser_eval_fn), POINTER, NOPASS :: eval_fn => NULL()
    TYPE(eis_indirection), POINTER :: indirect_fn => NULL()
    CONTAINS
    FINAL :: ese_destructor
  END TYPE eis_stack_element

  TYPE eis_stack
    TYPE(eis_stack_element), ALLOCATABLE :: entries(:)
    INTEGER :: stack_point, stack_size
    LOGICAL :: init = .FALSE.
    CONTAINS
    FINAL :: es_destructor
  END TYPE eis_stack

  CONTAINS
    PURE ELEMENTAL SUBROUTINE ese_destructor(this)
      TYPE(eis_stack_element), INTENT(INOUT) :: this
!      IF (ALLOCATED(this%text)) DEALLOCATE(this%text)
    END SUBROUTINE ese_destructor

    PURE ELEMENTAL SUBROUTINE es_destructor(this)
      TYPE(eis_stack), INTENT(INOUT) :: this
!      IF (ASSOCIATED(this%entries)) DEALLOCATE(this%entries)
    END SUBROUTINE es_destructor

END MODULE eis_header
