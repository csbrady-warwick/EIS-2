PROGRAM test

  USE eis_parser_mod
  !USE eis_parser_header
  USE eis_header
  TYPE(eis_parser) :: parser
  CHARACTER(LEN=1000) :: input
  INTEGER(eis_error) :: errcode
  REAL(eis_num), DIMENSION(:), ALLOCATABLE :: result
  TYPE(eis_stack) :: stored_value
  INTEGER :: ct

  DO WHILE(.TRUE.)
    WRITE(*,'(A)', ADVANCE = 'NO') "Please input a mathematical expression :"
    READ(*,'(A)') input
    CALL parser%tokenize(input, stored_value, errcode)
    ct = parser%evaluate(stored_value, result, errcode)
    IF (errcode == eis_err_none) THEN
      PRINT *,'Result is ', result(1:ct)
    ELSE
      CALL parser%print_errors()
    END IF
  END DO

END PROGRAM test
