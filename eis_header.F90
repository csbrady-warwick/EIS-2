MODULE eis_header

  USE eis_constants
  USE ISO_C_BINDING

  INTERFACE
    SUBROUTINE stack_get_fn(nitems, items) BIND(C)
      IMPORT eis_num
      INTEGER, INTENT(IN) :: nitems
      REAL(eis_num), DIMENSION(nitems), INTENT(OUT) :: items
    END SUBROUTINE stack_get_fn

    SUBROUTINE stack_set_fn(item) BIND(C)
      IMPORT eis_num
      REAL(eis_num), INTENT(IN) :: item
    END SUBROUTINE stack_set_fn

!    SUBROUTINE parser_eval_fn(getter, setter, errcode)
!      IMPORT stack_set_fn, stack_get_fn, eis_i8
!      PROCEDURE(stack_get_fn) :: getter
!      PROCEDURE(stack_set_fn) :: setter
!      INTEGER(eis_i8) :: errcode
!    END SUBROUTINE
    SUBROUTINE parser_eval_fn(errcode)
      IMPORT stack_set_fn, stack_get_fn, eis_i8
      INTEGER(eis_i8), INTENT(INOUT) :: errcode
    END SUBROUTINE
  END INTERFACE

  INTEGER, PARAMETER :: eis_err_none = 0
  INTEGER, PARAMETER :: eis_err_unknown_block = 2**0
  INTEGER, PARAMETER :: eis_err_unknown_element = 2**1
  INTEGER, PARAMETER :: eis_err_preset_element = 2**2
  INTEGER, PARAMETER :: eis_err_preset_element_use_later = 2**3
  INTEGER, PARAMETER :: eis_err_bad_value = 2**4
  INTEGER, PARAMETER :: eis_err_missing_elements = 2**5
  INTEGER, PARAMETER :: eis_err_terminate = 2**6
  INTEGER, PARAMETER :: eis_err_required_element_not_set = 2**7
  INTEGER, PARAMETER :: eis_err_pp_options_missing = 2**8
  INTEGER, PARAMETER :: eis_err_bad_array_length = 2**9
  INTEGER, PARAMETER :: eis_err_other = 2**10
  INTEGER, PARAMETER :: eis_err_warn_bad_value = 2**11
  INTEGER, PARAMETER :: eis_err_generic_warning = 2**12
  INTEGER, PARAMETER :: eis_err_generic_error = 2**13
  INTEGER, PARAMETER :: eis_err_pp_options_wrong = 2**14
  INTEGER, PARAMETER :: eis_err_io_error = 2**15
  INTEGER, PARAMETER :: eis_err_bad_setup = 2**16
  INTEGER, PARAMETER :: eis_err_window = 2**17

  TYPE eis_stack_element
    INTEGER :: ptype
    INTEGER :: associativity, precedence
    INTEGER :: value
    REAL(eis_num) :: numerical_data
    CHARACTER(LEN=:), ALLOCATABLE :: text
    PROCEDURE(parser_eval_fn), POINTER, NOPASS :: eval_fn => NULL()
  END TYPE eis_stack_element

  TYPE eis_stack
    TYPE(eis_stack_element), POINTER :: entries(:)
    INTEGER :: stack_point, stack_size
    LOGICAL :: init = .FALSE.
  END TYPE eis_stack

END MODULE eis_header
