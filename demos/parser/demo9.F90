MODULE mymod

  USE eis_header
  USE eis_parser_mod
  IMPLICIT NONE

  TYPE(eis_stack), DIMENSION(2) :: stacks

  CONTAINS

    SUBROUTINE emplace_fn(orig_string, nparams, params, host_params, &
        stack_out, status_code, errcode)
      CHARACTER(LEN=*), INTENT(IN) :: orig_string
      INTEGER, INTENT(IN) :: nparams
      REAL(eis_num), DIMENSION(nparams), INTENT(IN) :: params
      TYPE(C_PTR), INTENT(IN) :: host_params
      TYPE(eis_stack), INTENT(INOUT) :: stack_out
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_error), INTENT(INOUT) :: errcode

      IF (INT(params(1)) > 2 .OR. INT(params(1))<1) THEN
        errcode = eis_err_bad_value
        RETURN
      END IF

      stack_out = stacks(INT(params(1)))
    END SUBROUTINE emplace_fn

END MODULE mymod


PROGRAM test

  USE mymod
  USE eis_parser_mod
  USE eis_header
  TYPE(eis_parser) :: parser
  CHARACTER(LEN=1000) :: input
  INTEGER(eis_error) :: errcode
  REAL(eis_num), DIMENSION(:), ALLOCATABLE :: result
  INTEGER :: ct

  WRITE(*,'(A)', ADVANCE = 'NO') "Input expression for stack 1 :"
  READ(*,'(A)') input
  CALL parser%tokenize(input, stacks(1), errcode)
  IF (errcode /= eis_err_none) THEN
    CALL parser%print_errors()
    STOP
  END IF

  WRITE(*,'(A)', ADVANCE = 'NO') "Input expression for stack 2 :"
  READ(*,'(A)') input
  CALL parser%tokenize(input, stacks(2), errcode)
  IF (errcode /= eis_err_none) THEN
    CALL parser%print_errors()
    STOP
  END IF

  CALL parser%add_emplaced_function('emfunc', emplace_fn, errcode)
  IF (errcode /= eis_err_none) THEN
    CALL parser%print_errors()
    STOP
  END IF

  DO WHILE(.TRUE.)
    WRITE(*,'(A)', ADVANCE = 'NO') "Please input a mathematical expression :"
    READ(*,'(A)') input
    ct = parser%evaluate(input, result, errcode)
    IF (errcode == eis_err_none) THEN
      PRINT *,'Result is ', result(1:ct)
    ELSE
      CALL parser%print_errors()
    END IF
  END DO

END PROGRAM test
