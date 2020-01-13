PROGRAM test

  USE eis_parser_header
  TYPE(eis_parser) :: parser
  CHARACTER(LEN=1000) :: input
  INTEGER(eis_error) :: errcode
  REAL(eis_num), DIMENSION(:), ALLOCATABLE :: result
  TYPE(eis_stack) :: stored_value
  INTEGER :: ct

  PRINT *,'This is the same problems as demo1 but tokenizes the expression to &
      &a stack so that it can be evaluated multiple times (although it isn`t &
      &in this example)'
  DO WHILE(.TRUE.)
    WRITE(*,'(A)', ADVANCE = 'NO') "Please input a mathematical expression :"
    READ(*,'(A)') input
    CALL parser%tokenize(input, stored_value, errcode)
    IF (errcode /= eis_err_none) THEN
      CALL parser%print_errors()
      STOP
    END IF
    ct = parser%evaluate(stored_value, result, errcode)
    IF (errcode == eis_err_none) THEN
      PRINT *,'Result is ', result(1:ct)
    ELSE
      CALL parser%print_errors()
    END IF
  END DO

END PROGRAM test
