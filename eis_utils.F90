MODULE eis_utils

  IMPLICIT NONE

  CONTAINS

  SUBROUTINE eis_append_string(str_old, str_new, newline)
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: str_old
    CHARACTER(LEN=*), INTENT(IN) :: str_new
    LOGICAL, INTENT(IN), OPTIONAL :: newline
    CHARACTER(LEN=:), ALLOCATABLE :: temp
    LOGICAL :: use_newline

    use_newline = .TRUE.
    IF (PRESENT(newline)) use_newline = newline

    IF (ALLOCATED(str_old)) THEN
      ALLOCATE(temp, SOURCE = str_old)
      DEALLOCATE(str_old)
      IF (use_newline) THEN
        ALLOCATE(str_old, SOURCE = temp // NEW_LINE(str_new) // str_new)
      ELSE
        ALLOCATE(str_old, SOURCE = temp // str_new)
      END IF
      DEALLOCATE(temp)
    ELSE
      ALLOCATE(str_old, SOURCE = str_new)
    END IF

  END SUBROUTINE eis_append_string

END MODULE eis_utils
