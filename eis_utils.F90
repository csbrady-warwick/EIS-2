MODULE eis_utils

  USE eis_constants
  IMPLICIT NONE

  PRIVATE

  PUBLIC :: eis_append_string

  INTERFACE eis_append_string
    MODULE PROCEDURE  eis_append_string_aa
#ifdef UNICODE
    MODULE PROCEDURE  eis_append_string_uu
    MODULE PROCEDURE  eis_append_string_ua
    MODULE PROCEDURE  eis_append_string_au
#endif
  END INTERFACE eis_append_string

  CONTAINS

  SUBROUTINE eis_append_string_aa(str_old, str_new, newline)
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE, INTENT(INOUT) :: str_old
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN) :: str_new
    LOGICAL, INTENT(IN), OPTIONAL :: newline
    CHARACTER(KIND=ASCII) :: prototype
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE :: temp
    LOGICAL :: use_newline

    use_newline = .TRUE.
    IF (PRESENT(newline)) use_newline = newline

    IF (ALLOCATED(str_old)) THEN
      ALLOCATE(temp, SOURCE = str_old)
      DEALLOCATE(str_old)
      IF (use_newline) THEN
        ALLOCATE(str_old, SOURCE = temp // NEW_LINE(prototype) // str_new)
      ELSE
        ALLOCATE(str_old, SOURCE = temp // str_new)
      END IF
      DEALLOCATE(temp)
    ELSE
      ALLOCATE(str_old, SOURCE = str_new)
    END IF

  END SUBROUTINE eis_append_string_aa



#ifdef UNICODE
  SUBROUTINE eis_append_string_uu(str_old, str_new, newline)
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE, INTENT(INOUT) :: str_old
    CHARACTER(LEN=*, KIND=UCS4), INTENT(IN) :: str_new
    LOGICAL, INTENT(IN), OPTIONAL :: newline
    CHARACTER(KIND=UCS4) :: prototype
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE :: temp
    LOGICAL :: use_newline

    use_newline = .TRUE.
    IF (PRESENT(newline)) use_newline = newline

    IF (ALLOCATED(str_old)) THEN
      ALLOCATE(temp, SOURCE = str_old)
      DEALLOCATE(str_old)
      IF (use_newline) THEN
        ALLOCATE(str_old, SOURCE = temp // NEW_LINE(prototype) // str_new)
      ELSE
        ALLOCATE(str_old, SOURCE = temp // str_new)
      END IF
      DEALLOCATE(temp)
    ELSE
      ALLOCATE(str_old, SOURCE = str_new)
    END IF

  END SUBROUTINE eis_append_string_uu



  SUBROUTINE eis_append_string_ua(str_old, str_new, newline)
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE, INTENT(INOUT) :: str_old
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN) :: str_new
    LOGICAL, INTENT(IN), OPTIONAL :: newline
    CHARACTER(KIND=UCS4) :: prototype
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE :: temp, temp_src
    LOGICAL :: use_newline

    use_newline = .TRUE.
    ALLOCATE(CHARACTER(LEN=LEN(str_new), KIND=UCS4)::temp_src)
    temp_src = str_new
    IF (PRESENT(newline)) use_newline = newline

    IF (ALLOCATED(str_old)) THEN
      ALLOCATE(temp, SOURCE = str_old)
      DEALLOCATE(str_old)
      IF (use_newline) THEN
        ALLOCATE(str_old, SOURCE = temp // NEW_LINE(prototype) // temp_src)
      ELSE
        ALLOCATE(str_old, SOURCE = temp // temp_src)
      END IF
      DEALLOCATE(temp)
    ELSE
      ALLOCATE(str_old, SOURCE = temp_src)
    END IF
    DEALLOCATE(temp_src)

  END SUBROUTINE eis_append_string_ua



  SUBROUTINE eis_append_string_au(str_old, str_new, newline)
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE, INTENT(INOUT) :: str_old
    CHARACTER(LEN=*, KIND=UCS4), INTENT(IN) :: str_new
    LOGICAL, INTENT(IN), OPTIONAL :: newline
    CHARACTER(KIND=ASCII) :: prototype
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE :: temp, temp_src
    LOGICAL :: use_newline

    use_newline = .TRUE.
    ALLOCATE(CHARACTER(LEN=LEN(str_new), KIND=ASCII)::temp_src)
    temp_src = str_new
    IF (PRESENT(newline)) use_newline = newline

    IF (ALLOCATED(str_old)) THEN
      ALLOCATE(temp, SOURCE = str_old)
      DEALLOCATE(str_old)
      IF (use_newline) THEN
        ALLOCATE(str_old, SOURCE = temp // NEW_LINE(prototype) // temp_src)
      ELSE
        ALLOCATE(str_old, SOURCE = temp // temp_src)
      END IF
      DEALLOCATE(temp)
    ELSE
      ALLOCATE(str_old, SOURCE = temp_src)
    END IF
    DEALLOCATE(temp_src)

  END SUBROUTINE eis_append_string_au
#endif

END MODULE eis_utils
