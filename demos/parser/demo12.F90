MODULE testmod
  USE eis_parser_header

  TYPE, BIND(C) :: data_item
    REAL(eis_num) :: x = 0.0_eis_num
    REAL(eis_num) :: y = 0.0_eis_num
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

END MODULE testmod

PROGRAM test

  USE eis_parser_header
  USE testmod
  TYPE(eis_parser) :: parser
  CHARACTER(LEN=1000) :: input
  INTEGER(eis_error) :: errcode
  REAL(eis_num), DIMENSION(:), ALLOCATABLE :: result
  TYPE(eis_stack) :: fstack, dstackdx, dstackdy, dstackdx2, dstackdy2, &
      dstackdxdy
  INTEGER :: ct, ix, iy
  CHARACTER(LEN=:), ALLOCATABLE :: str
  TYPE(data_item), TARGET :: item

  PRINT *,'This example takes a stack expression parameterised by two &
      &variables, x and y. It then calculates all of the first and second &
      &partial derivatives of the expression'

  CALL parser%init(errcode, physics = eis_physics_si)

  CALL parser%add_variable('x', get_x, errcode)
  CALL parser%set_symbol_derivative('x','1', errcode, wrt = 'x')

  CALL parser%add_variable('y', get_y, errcode)
  CALL parser%set_symbol_derivative('y','1', errcode, wrt = 'y')
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

  CALL parser%get_deriv_stack(fstack, dstackdx, 'x', errcode)
  IF (errcode /= eis_err_none) THEN
    CALL parser%print_errors()
      STOP
  END IF
  CALL parser%get_infix(dstackdx, str)
  PRINT *, 'df/dx : ', str


  CALL parser%get_deriv_stack(fstack, dstackdy, 'y', errcode)
  IF (errcode /= eis_err_none) THEN
    CALL parser%print_errors()
      STOP
  END IF
  CALL parser%get_infix(dstackdy, str)
  PRINT *, 'df/dy : ', str

  CALL parser%get_deriv_stack(dstackdx, dstackdx2, 'x', errcode)
  IF (errcode /= eis_err_none) THEN
    CALL parser%print_errors()
      STOP
  END IF
  CALL parser%get_infix(dstackdx2, str)
  PRINT *, 'd2f/dx2 : ', str

  CALL parser%get_deriv_stack(dstackdy, dstackdy2, 'y', errcode)
  IF (errcode /= eis_err_none) THEN
    CALL parser%print_errors()
      STOP
  END IF
  CALL parser%get_infix(dstackdy, str)
  PRINT *, 'df2/dy2 : ', str

  CALL parser%get_deriv_stack(dstackdx, dstackdxdy, 'y', errcode)
  IF (errcode /= eis_err_none) THEN
    CALL parser%print_errors()
      STOP
  END IF
  CALL parser%get_infix(dstackdxdy, str)
  PRINT *, 'd2f/dxdy : ', str

  PRINT *,'Writing expressions evaluated on [0,1],[0,1] to "fort.10"'

  DO iy = 1, 100
    item%y = REAL(iy-1, eis_num)/99.0_eis_num
    DO ix = 1 , 100
      item%x = REAL(ix-1, eis_num)/99.0_eis_num
      ct = parser%evaluate(fstack, result, errcode, host_params = C_LOC(item))
      WRITE(10,*) result(1)
    END DO
  END DO

  DO iy = 1, 100
    item%y = REAL(iy-1, eis_num)/99.0_eis_num
    DO ix = 1 , 100
      item%x = REAL(ix-1, eis_num)/99.0_eis_num
      ct = parser%evaluate(dstackdx, result, errcode, host_params = C_LOC(item))
      WRITE(10,*) result(1)
    END DO
  END DO

  DO iy = 1, 100
    item%y = REAL(iy-1, eis_num)/99.0_eis_num
    DO ix = 1 , 100
      item%x = REAL(ix-1, eis_num)/99.0_eis_num
      ct = parser%evaluate(dstackdy, result, errcode, host_params = C_LOC(item))
      WRITE(10,*) result(1)
    END DO
  END DO

  DO iy = 1, 100
    item%y = REAL(iy-1, eis_num)/99.0_eis_num
    DO ix = 1 , 100
      item%x = REAL(ix-1, eis_num)/99.0_eis_num
      ct = parser%evaluate(dstackdx2, result, errcode, &
          &host_params = C_LOC(item))
      WRITE(10,*) result(1)
    END DO
  END DO

  DO iy = 1, 100
    item%y = REAL(iy-1, eis_num)/99.0_eis_num
    DO ix = 1 , 100
      item%x = REAL(ix-1, eis_num)/99.0_eis_num
      ct = parser%evaluate(dstackdy2, result, errcode, &
          &host_params = C_LOC(item))
      WRITE(10,*) result(1)
    END DO
  END DO

  DO iy = 1, 100
    item%y = REAL(iy-1, eis_num)/99.0_eis_num
    DO ix = 1 , 100
      item%x = REAL(ix-1, eis_num)/99.0_eis_num
      ct = parser%evaluate(dstackdxdy, result, errcode, &
          &host_params = C_LOC(item))
      WRITE(10,*) result(1)
    END DO
  END DO

END PROGRAM test
