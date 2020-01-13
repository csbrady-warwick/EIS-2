MODULE mymod

  USE eis_parser_header
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
  TYPE(eis_parser) :: parser
  CHARACTER(LEN=1000) :: input
  INTEGER(eis_error) :: errcode
  REAL(eis_num), DIMENSION(:), ALLOCATABLE :: result
  INTEGER :: ct

  PRINT *,'This example shows the use of emplaced functions. Emplaced &
      &functions are functions that take numerical values and return &
      &a stack based on those values. These stacks are then used in &
      &the same way as stack variables.'

  PRINT *,'This example asks you to input two mathematical expressions &
      &that are tokenized to stacks and stored. It then asks you to &
      &specify a mathematical expression that can include the `emfunc` &
      &function. `emfunc` takes one parameter that must be either 1 or 2 &
      &and returns either the first or the second stack that you specified.'

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
