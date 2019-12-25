MODULE eis_algorithm

  USE eis_constants
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: eis_interpolate

  CONTAINS

  FUNCTION eis_interpolate(point, x, y, errcode)
    REAL(eis_num), INTENT(IN) :: point
    REAL(eis_num), DIMENSION(:), INTENT(IN) :: x, y
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    REAL(eis_num) :: eis_interpolate
    INTEGER :: upper, lower, mid
    REAL(eis_num) :: frac

    errcode = eis_err_none

    IF (point <= x(1)) THEN
      eis_interpolate = y(1)
      RETURN
    ELSE IF (point >= x(SIZE(x))) THEN
      eis_interpolate = y(SIZE(x))
      RETURN
    END IF

    lower = 1
    upper = SIZE(x)
    mid = (lower + upper)/2
    DO
      IF (point >= x(mid) .AND. point <= x(mid+1)) THEN
        frac = (point - x(mid))/(x(mid+1)-x(mid))
        eis_interpolate = y(mid) + frac * (y(mid+1) - y(mid))
        EXIT
      ELSE IF (point < x(mid)) THEN
        upper = mid
        mid = (lower + upper)/2
      ELSE
        lower = mid
        mid = (lower + upper)/2
      END IF
    END DO

  END FUNCTION eis_interpolate

END MODULE eis_algorithm
