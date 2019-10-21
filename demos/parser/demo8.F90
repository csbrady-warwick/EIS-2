PROGRAM test

  USE eis_parser_header
  TYPE(eis_parser) :: parser
  CHARACTER(LEN=1000) :: input
  INTEGER(eis_error) :: errcode
  REAL(eis_num), DIMENSION(:), ALLOCATABLE :: result
  INTEGER :: ct

  PRINT *,'This example demonstrates using stack variables to store a value &
      &that a user provides and making it available for later use. First &
      &specify an expression that evaluates to a single value. That expression &
      &is then stored under the name `stackvar` and can be used in the second &
      &part of the code. NB stackvar is NOT just storing the value of the &
      &expression and returning it but storing the stack from your expression &
      &and putting it in place when you use it by name. Functions and &
      &variables that have changing values will change as expected'

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
