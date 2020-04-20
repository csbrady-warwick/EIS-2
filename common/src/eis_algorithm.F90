MODULE eis_algorithm

  USE eis_constants
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: eis_interpolate1d, eis_interpolate2d
  PUBLIC :: eis_deriv_interpolate1d, eis_bisect_axis

  CONTAINS

  SUBROUTINE eis_bisect_axis(point, axis, cell, frac)
    REAL(eis_num), INTENT(IN) :: point
    REAL(eis_num), DIMENSION(:), INTENT(IN) :: axis
    INTEGER(eis_i8), INTENT(OUT) :: cell
    REAL(eis_num), INTENT(OUT) :: frac
    INTEGER(eis_i8) :: upper, lower, mid

    IF (point <= axis(1)) THEN
      cell = 1
      frac = 0.0_eis_num
      RETURN
    ELSE IF (point >= axis(SIZE(axis, KIND = eis_i8))) THEN
      cell = SIZE(axis, KIND = eis_i8) - 1
      frac = 1.0_eis_num
      RETURN
    END IF

    lower = 1_eis_i8
    upper = SIZE(axis, KIND = eis_i8)
    mid = (lower + upper)/2
    DO
      IF (point >= axis(mid) .AND. point <= axis(mid+1)) THEN
        frac = (point - axis(mid))/(axis(mid+1)-axis(mid))
        cell = mid
        EXIT
      ELSE IF (point < axis(mid)) THEN
        upper = mid
        mid = (lower + upper)/2_eis_i8
      ELSE
        lower = mid
        mid = (lower + upper)/2_eis_i8
      END IF
    END DO

  END SUBROUTINE eis_bisect_axis



  FUNCTION eis_interpolate1d(point, x, y, errcode)
    REAL(eis_num), INTENT(IN) :: point
    REAL(eis_num), DIMENSION(:), INTENT(IN) :: x, y
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    REAL(eis_num) :: eis_interpolate1d
    INTEGER(eis_i8) :: cell
    REAL(eis_num) :: frac

    errcode = eis_err_none
    CALL eis_bisect_axis(point, x, cell, frac)

    eis_interpolate1d = y(cell) + frac * (y(cell+1) - y(cell))
  END FUNCTION eis_interpolate1d



  FUNCTION eis_deriv_interpolate1d(point, x, y, errcode)
    REAL(eis_num), INTENT(IN) :: point
    REAL(eis_num), DIMENSION(:), INTENT(IN) :: x, y
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    REAL(eis_num) :: eis_deriv_interpolate1d
    INTEGER(eis_i8) :: cell
    REAL(eis_num) :: frac

    errcode = eis_err_none
    CALL eis_bisect_axis(point, x, cell, frac)

    eis_deriv_interpolate1d = (y(cell+1) - y(cell))/(x(cell+1) - x(cell))
  END FUNCTION eis_deriv_interpolate1d



  FUNCTION eis_interpolate2d(point, x, y, z, errcode)
    REAL(eis_num), DIMENSION(2), INTENT(IN) :: point
    REAL(eis_num), DIMENSION(:), INTENT(IN) :: x, y
    REAL(eis_num), DIMENSION(:,:), INTENT(IN) :: z
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    REAL(eis_num) :: eis_interpolate2d
    INTEGER(eis_i8) :: cell_x, cell_y
    REAL(eis_num) :: frac_x, frac_y, val1, val2

    errcode = eis_err_none

    CALL eis_bisect_axis(point(1), x, cell_x, frac_x)
    CALL eis_bisect_axis(point(2), y, cell_y, frac_y)
    val1 = z(cell_x, cell_y) + frac_x * (z(cell_x+1, cell_y) &
        - z(cell_x, cell_y))
    val2 = z(cell_x, cell_y+1) + frac_x * (z(cell_x+1, cell_y+1) &
        - z(cell_x, cell_y+1))

    eis_interpolate2d = val1 + frac_y *(val2 - val1)

  END FUNCTION eis_interpolate2d

END MODULE eis_algorithm
