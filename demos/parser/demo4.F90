MODULE mymod

  USE ISO_C_BINDING
  USE eis_header

  CONTAINS

  FUNCTION get_var(nparams, params, host_params, status_code, errcode) &
      RESULT(res) BIND(C)
    INTEGER(eis_i4), VALUE, INTENT(IN) :: nparams
    REAL(eis_num), DIMENSION(nparams), INTENT(IN) :: params
    TYPE(C_PTR), VALUE, INTENT(IN) :: host_params
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    REAL(eis_num) :: res
    REAL(eis_num) :: store = 0.0_eis_num

    store = store + 1.0_eis_num

  END FUNCTION get_var

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

  CALL parser%add_variable('myvar', get_var, errcode, can_simplify = .FALSE.)
  IF (errcode /= eis_err_none) CALL parser%print_errors()

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