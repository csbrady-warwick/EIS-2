MODULE eis_header

  USE eis_constants
  USE ISO_C_BINDING

  INTERFACE
!    SUBROUTINE stack_get_fn(nitems, items) BIND(C)
!      IMPORT eis_num
!      INTEGER, INTENT(IN) :: nitems
!      REAL(eis_num), DIMENSION(nitems), INTENT(OUT) :: items
!    END SUBROUTINE stack_get_fn

!    SUBROUTINE stack_set_fn(item) BIND(C)
!      IMPORT eis_num
!      REAL(eis_num), INTENT(IN) :: item
!    END SUBROUTINE stack_set_fn

    FUNCTION parser_eval_fn(nparams, params, errcode) BIND(C)
      IMPORT eis_num, eis_i8, eis_i4
      INTEGER(eis_i4) :: nparams
      REAL(eis_num), DIMENSION(nparams) :: params
      INTEGER(eis_i8) :: errcode
      REAL(eis_num) :: parser_eval_fn
    END FUNCTION
  END INTERFACE

  INTEGER, PARAMETER :: eis_err_none = 0
  INTEGER, PARAMETER :: eis_err_bad_value = 2**0
  INTEGER, PARAMETER :: eis_err_wrong_parameters = 2**1

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
