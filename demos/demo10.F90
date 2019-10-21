MODULE mymod

  USE ISO_C_BINDING
  USE eis_header

  TYPE, BIND(C) :: data_item
    REAL(eis_num) :: x = 0.0_eis_num
    REAL(eis_num) :: y = 0.0_eis_num
  END TYPE data_item

  CONTAINS

  SUBROUTINE get_values(nvalues, values, host_params, status_code, errcode) &
      BIND(C)

    INTEGER(eis_i4), INTENT(INOUT) :: nvalues
    REAL(eis_num), DIMENSION(nvalues), INTENT(OUT) :: values
    TYPE(C_PTR), VALUE, INTENT(IN) :: host_params
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    TYPE(data_item), POINTER :: dat
    REAL(eis_num), PARAMETER :: pi = 4.0_eis_num * ATAN(1.0_eis_num)

    CALL C_F_POINTER(host_params, dat)
    values(1) = SIN(4.0_eis_num * pi * (dat%x-0.5_eis_num)) * COS(6.0_eis_num*pi*(dat%y-0.5_eis_num))
    nvalues = 1

  END SUBROUTINE get_values

END MODULE mymod


PROGRAM test

  USE eis_parser_mod
  USE eis_header
  USE eis_parser_header
  USE mymod
  TYPE(eis_parser) :: parser
  TYPE(eis_stack) :: stack
  CHARACTER(LEN=1000) :: input
  INTEGER(eis_error) :: errcode
  REAL(eis_num), DIMENSION(:), ALLOCATABLE :: result
  INTEGER :: ct, ix, iy, it
  TYPE(data_item), TARGET :: item
  CHARACTER(LEN=:), ALLOCATABLE :: str

  CALL parser%set_result_function(get_values, stack, errcode)
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
