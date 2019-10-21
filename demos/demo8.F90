PROGRAM test

  USE mymod
  USE eis_parser_mod
  USE eis_header
  TYPE(eis_parser) :: parser
  CHARACTER(LEN=1000) :: input
  INTEGER(eis_error) :: errcode
  REAL(eis_num), DIMENSION(:), ALLOCATABLE :: result
  INTEGER :: ct

  WRITE(*,'(A)', ADVANCE = 'NO') "Input expression for storage :"
  READ(*,'(A)') input

  CALL parser%add_stack_variable('stackvar', input, errcode)
  IF (errcode /= eis_err_none) THEN
    CALL parser%print_errors()
    STOP
  END IF
  PRINT *,'Expression is stored as "stackvar"'

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
