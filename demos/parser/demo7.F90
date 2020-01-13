MODULE mymod

  USE ISO_C_BINDING
  USE eis_parser_header

  TYPE, BIND(C) :: data_item
    REAL(eis_num) :: x = 0.0_eis_num
    REAL(eis_num) :: y = 0.0_eis_num
  END TYPE data_item

  CONTAINS

  !Function to implement the Cauchy distribution
  !https://en.wikipedia.org/wiki/Cauchy_distribution
  FUNCTION cauchy_dist(nparams, params, host_params, status_code, errcode) &
      RESULT(res) BIND(C)
    INTEGER(eis_i4), VALUE, INTENT(IN) :: nparams
    REAL(eis_num), DIMENSION(nparams), INTENT(IN) :: params
    TYPE(C_PTR), VALUE, INTENT(IN) :: host_params
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    REAL(eis_num) :: res
    REAL(eis_num), PARAMETER :: pi = 4.0_eis_num * ATAN(1.0_eis_num)

    !params(1) - x, dependent variable
    !params(2) - x0, location parameter
    !params(3) - gamma, scale parameter

    res = 1.0/(pi * params(3)) * (params(3)**2 / (params(1) - params(2))**2 &
        + params(3)**2)

  END FUNCTION cauchy_dist



  FUNCTION get_x(nparams, params, host_params, status_code, errcode) &
      RESULT(res) BIND(C)
    INTEGER(eis_i4), VALUE, INTENT(IN) :: nparams
    REAL(eis_num), DIMENSION(nparams), INTENT(IN) :: params
    TYPE(C_PTR), VALUE, INTENT(IN) :: host_params
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    REAL(eis_num) :: res
    TYPE(data_item), POINTER :: dat

    IF (.NOT. C_ASSOCIATED(host_params)) RETURN
    CALL C_F_POINTER(host_params, dat)
    res = dat%x

  END FUNCTION get_x


  FUNCTION get_y(nparams, params, host_params, status_code, errcode) &
      RESULT(res) BIND(C)
    INTEGER(eis_i4), VALUE, INTENT(IN) :: nparams
    REAL(eis_num), DIMENSION(nparams), INTENT(IN) :: params
    TYPE(C_PTR), VALUE, INTENT(IN) :: host_params
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    REAL(eis_num) :: res
    TYPE(data_item), POINTER :: dat

    IF (.NOT. C_ASSOCIATED(host_params)) RETURN
    CALL C_F_POINTER(host_params, dat)
    res = dat%y

  END FUNCTION get_y

END MODULE mymod


PROGRAM test

  USE mymod
  TYPE(eis_parser) :: parser
  TYPE(eis_stack) :: stack
  CHARACTER(LEN=1000) :: input
  INTEGER(eis_error) :: errcode
  REAL(eis_num), DIMENSION(:), ALLOCATABLE :: result
  INTEGER :: ct, ix, iy, it
  TYPE(data_item), TARGET :: item
  CHARACTER(LEN=:), ALLOCATABLE :: str

  PRINT *,'This is a version of demo6 but implements a custom function called &
      &`cauchy` that calculates the cauchy distributiong. It takes 3 &
      &parameters. The first parameter is the location to evaluate the &
      &function at, the second is the x origin of the ray and the third is &
      &the gamma parameter'

  CALL parser%add_variable('x', get_x, errcode)
  CALL parser%add_variable('y', get_y, errcode)
  CALL parser%add_function('cauchy', cauchy_dist, errcode, expected_params = 3)

  WRITE(*,'(A)', ADVANCE = 'NO') "Please input a mathematical expression :"
  READ(*,'(A)') input
  CALL parser%tokenize(input, stack, errcode)
  IF (errcode /= eis_err_none) THEN
    CALL parser%print_errors()
    STOP
  END IF
  ct = parser%evaluate(stack, result, errcode, host_params = C_LOC(item))
  IF (errcode /= eis_err_none) THEN
    CALL parser%print_errors()
    STOP
  END IF
  DO iy = 1, 100
    item%y = REAL(iy-1, eis_num)/99.0_eis_num
    DO ix = 1 , 100
      item%x = REAL(ix-1, eis_num)/99.0_eis_num
      ct = parser%evaluate(stack, result, errcode, host_params = C_LOC(item))
      WRITE(10,*) result(1)
    END DO
  END DO

  DEALLOCATE(result)

END PROGRAM test
