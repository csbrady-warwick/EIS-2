PROGRAM test

  USE eis_parser_header
  IMPLICIT NONE
  TYPE(eis_parser) :: parser
  CHARACTER(LEN=1000) :: input
  INTEGER(eis_error) :: errcode
  REAL(eis_num), DIMENSION(:), ALLOCATABLE :: result
  INTEGER :: ct

  CALL parser%init(errcode, physics = eis_physics_si)
  IF (errcode /= eis_err_none) CALL parser%print_errors()

  PRINT *,'This example sets the parser to use SI physics. Physical constants &
      &like kb (Boltzmann`s Constant) are given in SI units by default'
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
