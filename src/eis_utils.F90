MODULE eis_utils

  USE, INTRINSIC :: ISO_C_BINDING
  USE eis_constants
  IMPLICIT NONE

  PRIVATE

  PUBLIC :: eis_append_string, c_f_string, f_c_string

  INTERFACE eis_append_string
    MODULE PROCEDURE  eis_append_string_aa
#ifdef UNICODE
    MODULE PROCEDURE  eis_append_string_uu
    MODULE PROCEDURE  eis_append_string_ua
    MODULE PROCEDURE  eis_append_string_au
#endif
  END INTERFACE eis_append_string

  INTERFACE c_f_string
    MODULE PROCEDURE c_f_string_array, c_f_string_ptr
  END INTERFACE c_f_string

  INTERFACE f_c_string
    MODULE PROCEDURE f_c_string_array, f_c_string_ptr
  END INTERFACE f_c_string

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

  !>Function to copy a C string into a Fortran string
  !>If the destination string passed is too short then it truncates
  PURE SUBROUTINE c_f_copy_string(c_string, c_str_size, f_string)

    CHARACTER(LEN=1, KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: c_string
    INTEGER, INTENT(IN) :: c_str_size
    CHARACTER(LEN=*), INTENT(OUT) :: f_string
    INTEGER :: to_copy, istr

    to_copy = MIN(LEN(f_string), c_str_size)
    DO istr = 1, to_copy
      f_string(istr:istr) = c_string(istr)
    END DO

  END SUBROUTINE c_f_copy_string



  !>Function to produce a Fortran string from a C string
  SUBROUTINE c_f_string_array(c_string, f_string)
    CHARACTER(LEN=1, KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: c_string
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: f_string
    INTEGER :: c_end, istr

    IF (ALLOCATED(f_string)) DEALLOCATE(f_string)

    istr = 1
    DO
      IF (c_string(istr) == C_NULL_CHAR) THEN
        c_end = istr - 1
        EXIT
      END IF
      istr = istr + 1
    END DO
    ALLOCATE(CHARACTER(c_end)::f_string)

    CALL c_f_copy_string(c_string, c_end, f_string)

  END SUBROUTINE c_f_string_array



  !>Function to produce a Fortran string from a C string
  SUBROUTINE c_f_string_ptr(c_string, f_string)
    TYPE(C_PTR), INTENT(IN) :: c_string
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: f_string
    CHARACTER(LEN=1, KIND=C_CHAR), DIMENSION(:), POINTER :: c_string_arr
    INTEGER :: c_end, istr

    IF (ALLOCATED(f_string)) DEALLOCATE(f_string)

    istr = 1
    DO
      CALL C_F_POINTER(c_string, c_string_arr, [istr])
      IF (c_string_arr(istr) == C_NULL_CHAR) THEN
        c_end = istr - 1
        EXIT
      END IF
      istr = istr + 1
    END DO
    ALLOCATE(CHARACTER(c_end)::f_string)

    CALL c_f_copy_string(c_string_arr, c_end, f_string)

  END SUBROUTINE c_f_string_ptr



  SUBROUTINE f_c_string_array(f_string, len_c_string, c_string)
    CHARACTER(LEN=*), INTENT(IN) :: f_string
    INTEGER, INTENT(IN) :: len_c_string
    CHARACTER(LEN=1, KIND = C_CHAR), DIMENSION(len_c_string), INTENT(OUT) :: &
        c_string
    INTEGER :: copylen, istr

    copylen = MIN(len_c_string - 1, LEN(f_string))
    DO istr = 1, copylen
      c_string(istr) = f_string(istr:istr)
    END DO
    c_string(copylen+1) = CHAR(0, KIND = C_CHAR)

  END SUBROUTINE f_c_string_array



  SUBROUTINE f_c_string_ptr(f_string, len_c_string, c_string)
    CHARACTER(LEN=*), INTENT(IN) :: f_string
    INTEGER, INTENT(IN) :: len_c_string
    TYPE(C_PTR), INTENT(IN) :: c_string
    CHARACTER(LEN=1, KIND=C_CHAR), DIMENSION(:), POINTER :: c_string_arr

    CALL C_F_POINTER(c_string, c_string_arr, [len_c_string])
    CALL f_c_string(f_string, len_c_string, c_string_arr)

  END SUBROUTINE f_c_string_ptr

END MODULE eis_utils
