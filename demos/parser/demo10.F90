PROGRAM test

  USE eis_parser_mod
  USE eis_parser_header
  USE eis_header
  IMPLICIT NONE
  TYPE(eis_parser) :: parser
  CHARACTER(LEN=1000) :: input
  INTEGER(eis_error) :: errcode
  REAL(eis_num), DIMENSION(:), ALLOCATABLE :: result
  INTEGER :: ct
  INTEGER, POINTER :: ptrvar
  INTEGER(eis_bitmask) :: cbits

  ALLOCATE(ptrvar)
  ptrvar = 100
  errcode = eis_err_none

  CALL parser%init(errcode, physics = eis_physics_si)
  IF (errcode /= eis_err_none) CALL parser%print_errors()
  !Create a parser pointer variable and set it to have a
  !capability bit value of 1
  CALL parser%add_pointer_variable('ptrvar', ptrvar, errcode, &
      cap_bits = 1_eis_bitmask)

  DO WHILE(.TRUE.)
    WRITE(*,'(A)', ADVANCE = 'NO') "Please input a mathematical expression :"
    READ(*,'(A)') input
    IF (input == 'exit') EXIT
    ct = parser%evaluate(input, result, errcode, cap_bits = cbits)
    IF (errcode == eis_err_none) THEN
      PRINT *,'Result is ', result(1:ct)
      !If expression used ptrvar (and hence has capability bit of 1)
      !increment ptrvar
      IF (cbits == 1_eis_bitmask) ptrvar = ptrvar + 1
    ELSE
      CALL parser%print_errors()
    END IF
  END DO

END PROGRAM test
