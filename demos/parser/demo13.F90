! Copyright (c) 2019-2026, C. S. Brady and H. Ratcliffe
! SPDX-License-Identifier: BSD-3-Clause

MODULE testmod
  USE eis_parser_header
  IMPLICIT NONE

  TYPE, BIND(C) :: data_item
    REAL(eis_num) :: x = 0.0_eis_num
  END TYPE data_item

  CONTAINS

  FUNCTION get_x(nparams, params, host_params, status_code, errcode) &
      RESULT(res) BIND(C)
    INTEGER(eis_i4), VALUE, INTENT(IN) :: nparams
    REAL(eis_num), DIMENSION(nparams), INTENT(IN) :: params
    TYPE(C_PTR), VALUE, INTENT(IN) :: host_params
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    REAL(eis_num) :: res
    TYPE(data_item), POINTER :: dat

    res = 0.0_eis_num
    IF (.NOT. C_ASSOCIATED(host_params)) RETURN
    CALL C_F_POINTER(host_params, dat)
    res = dat%x

  END FUNCTION get_x

END MODULE testmod

PROGRAM test

  USE eis_parser_header
  USE testmod
  IMPLICIT NONE
  TYPE(eis_parser) :: parser
  CHARACTER(LEN=1000) :: input
  INTEGER(eis_error) :: errcode
  REAL(eis_num), DIMENSION(:), ALLOCATABLE :: result
  TYPE(eis_stack) :: fstack, dstack
  INTEGER :: ct, iter
  CHARACTER(LEN=:), ALLOCATABLE :: str
  TYPE(data_item), TARGET :: item
  REAL(eis_num) :: fval, dval
  REAL(eis_num), PARAMETER :: tolerance = 1.0e-12_eis_num
  INTEGER, PARAMETER :: max_iter = 50

  PRINT *,'This example shows the use of a derivative stack. It takes an &
      &expression in the variable x, uses the parser to get a stack that &
      &calculates the derivative of the expression with respect to x and then &
      &uses both stacks to find a root of the expression using Newton-Raphson &
      &iteration'

  CALL parser%init(errcode, physics = eis_physics_si)

  !Add the variable x and tell the parser that the derivative of x with
  !respect to x is 1
  CALL parser%add_variable('x', get_x, errcode)
  CALL parser%set_symbol_derivative('x', '1', errcode, wrt = 'x')
  IF (errcode /= eis_err_none) THEN
    CALL parser%print_errors()
    STOP
  END IF

  WRITE(*,'(A)', ADVANCE = 'NO') "Please input a mathematical expression :"
  READ(*,'(A)') input
  CALL parser%tokenize(input, fstack, errcode)
  IF (errcode /= eis_err_none) THEN
    CALL parser%print_errors()
    STOP
  END IF
  CALL parser%get_infix(fstack, str)
  PRINT *, 'Specified Function :', str

  !Get a stack that calculates the derivative of the function with respect to x
  CALL parser%get_deriv_stack(fstack, dstack, 'x', errcode)
  IF (errcode /= eis_err_none) THEN
    CALL parser%print_errors()
    STOP
  END IF
  CALL parser%get_infix(dstack, str)
  PRINT *, 'df/dx : ', str

  WRITE(*,'(A)', ADVANCE = 'NO') "Please input a starting guess for x :"
  READ(*,*) item%x

  DO iter = 1, max_iter
    ct = parser%evaluate(fstack, result, errcode, host_params = C_LOC(item))
    fval = result(1)
    ct = parser%evaluate(dstack, result, errcode, host_params = C_LOC(item))
    dval = result(1)
    IF (errcode /= eis_err_none) THEN
      CALL parser%print_errors()
      STOP
    END IF

    PRINT *, 'Iteration ', iter, ' x = ', item%x, ' f(x) = ', fval
    IF (ABS(fval) < tolerance) THEN
      PRINT *, 'Root found at x = ', item%x
      EXIT
    END IF
    IF (dval == 0.0_eis_num) THEN
      PRINT *, 'The derivative is zero at x = ', item%x, '. Cannot continue'
      EXIT
    END IF
    item%x = item%x - fval / dval
  END DO

  IF (iter > max_iter) PRINT *, 'No root found in ', max_iter, ' iterations'

END PROGRAM test
