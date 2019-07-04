MODULE eis_utils

  USE, INTRINSIC :: ISO_C_BINDING
  USE eis_constants
  IMPLICIT NONE

  PRIVATE

  PUBLIC :: eis_append_string, c_f_string, f_c_string
  PUBLIC :: eis_get_lun, eis_load_file_to_string

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

  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to append a new string to the end of an existing string
  !> optionally with a suitable newline sequence between between them.
  !> Both strings must be ASCII kind
  !> @details
  !> The old string is reallocated to be exactly as LEN(str_old) + LEN(str_new)
  !> If str_old is not allocated then it becomes a copy of str_new.
  !> @param[inout] str_old
  !> @param[in] str_new
  !> @param[in] newline
  SUBROUTINE eis_append_string_aa(str_old, str_new, newline)
    !> Existing string. Must be ALLOCATABLE, but can be non-allocated
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE, INTENT(INOUT) :: str_old
    !> New string to append to the existing string
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN) :: str_new
    !> Logical flag. If .TRUE. then the strings are combined with a newline
    !> sequence between them. If .FALSE. then they are directly combined.
    !> Optional, default .TRUE.
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
  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to append a new string to the end of an existing string
  !> optionally with a suitable newline sequence between between them.
  !> Both strings must be UCS4 kind
  !> @details
  !> The old string is reallocated to be exactly as LEN(str_old) + LEN(str_new)
  !> If str_old is not allocated then it becomes a copy of str_new.
  !> @param[inout] str_old
  !> @param[in] str_new
  !> @param[in] newline
  SUBROUTINE eis_append_string_uu(str_old, str_new, newline)
    !> Existing string. Must be ALLOCATABLE, but can be non-allocated
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE, INTENT(INOUT) :: str_old
    !> New string to append to the existing string
    CHARACTER(LEN=*, KIND=UCS4), INTENT(IN) :: str_new
    !> Logical flag. If .TRUE. then the strings are combined with a newline
    !> sequence between them. If .FALSE. then they are directly combined.
    !> Optional, default .TRUE.
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



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to append a new string to the end of an existing string
  !> optionally with a suitable newline sequence between between them.
  !> str_old is UCS4, str_new is ASCII
  !> @details
  !> The old string is reallocated to be exactly as LEN(str_old) + LEN(str_new)
  !> If str_old is not allocated then it becomes a copy of str_new.
  !> @param[inout] str_old
  !> @param[in] str_new
  !> @param[in] newline
  SUBROUTINE eis_append_string_ua(str_old, str_new, newline)
    !> Existing string. Must be ALLOCATABLE, but can be non-allocated
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE, INTENT(INOUT) :: str_old
    !> New string to append to the existing string
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN) :: str_new
    !> Logical flag. If .TRUE. then the strings are combined with a newline
    !> sequence between them. If .FALSE. then they are directly combined.
    !> Optional, default .TRUE.
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


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to append a new string to the end of an existing string
  !> optionally with a suitable newline sequence between between them.
  !> str_old is ASCII, str_new is UCS4
  !> @details
  !> The old string is reallocated to be exactly as LEN(str_old) + LEN(str_new)
  !> If str_old is not allocated then it becomes a copy of str_new.
  !> @param[inout] str_old
  !> @param[in] str_new
  !> @param[in] newline
  SUBROUTINE eis_append_string_au(str_old, str_new, newline)
    !> Existing string. Must be ALLOCATABLE, but can be non-allocated
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE, INTENT(INOUT) :: str_old
    !> New string to append to the existing string
    CHARACTER(LEN=*, KIND=UCS4), INTENT(IN) :: str_new
    !> Logical flag. If .TRUE. then the strings are combined with a newline
    !> sequence between them. If .FALSE. then they are directly combined.
    !> Optional, default .TRUE.
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

  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to copy a C string into a Fortran string
  !> If the destination string passed is too short then it truncates
  !> @param[in] c_string
  !> @param[in] c_str_size
  !> @param[out] f_string
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



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Take a C string as a simple character array and copy it into a normal
  !> Fortran string. Fortran output string must be allocatable and will be
  !> allocated to be exactly large enough to hold the input C string.
  !> Malformed C strings that lack a null terminator will cause this function
  !> to hang
  !> @param[in] c_string
  !> @param[out] f_string
  SUBROUTINE c_f_string_array(c_string, f_string)
    !> Input C string as an array of character
    CHARACTER(LEN=1, KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: c_string
    !> Output string. Must be allocatable
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



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Take a C string as a C pointer and copy it into a normal
  !> Fortran string. Fortran output string must be allocatable and will be
  !> allocated to be exactly large enough to hold the input C string.
  !> Malformed C strings that lack a null terminator will cause this function
  !> to hang
  !> @param[in] c_string
  !> @param[out] f_string
  SUBROUTINE c_f_string_ptr(c_string, f_string)
    !> Input C string as an array of character
    TYPE(C_PTR), INTENT(IN) :: c_string
    !> Output string. Must be allocatable
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



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Copy a Fortran string into a C string, specifying the maximum length of
  !> the C string. Up to "len_c_string-1" characters are copied from f_string
  !> to c_string and then appended with a null terminator character
  !> @param[in] f_string
  !> @param[in] len_c_string
  !> @param[out] c_string
  SUBROUTINE f_c_string_array(f_string, len_c_string, c_string)
    !> Fortran source string
    CHARACTER(LEN=*), INTENT(IN) :: f_string
    !> Maximum length of the C string including the null terminator
    INTEGER, INTENT(IN) :: len_c_string
    !> C output string
    CHARACTER(LEN=1, KIND = C_CHAR), DIMENSION(len_c_string), INTENT(OUT) :: &
        c_string
    INTEGER :: copylen, istr

    copylen = MIN(len_c_string - 1, LEN(f_string))
    DO istr = 1, copylen
      c_string(istr) = f_string(istr:istr)
    END DO
    c_string(copylen+1) = CHAR(0, KIND = C_CHAR)

  END SUBROUTINE f_c_string_array



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Copy a Fortran string into a C pointer, specifying the maximum length of
  !> the C string. Up to "len_c_string-1" characters are copied from f_string
  !> to c_string and then appended with a null terminator character
  !> @param[in] f_string
  !> @param[in] len_c_string
  !> @param[out] c_string
  SUBROUTINE f_c_string_ptr(f_string, len_c_string, c_string)
    !> Fortran source string
    CHARACTER(LEN=*), INTENT(IN) :: f_string
    !> @param[in] len_c_string
    INTEGER, INTENT(IN) :: len_c_string
    !> @param[out] c_string
    TYPE(C_PTR), INTENT(IN) :: c_string
    CHARACTER(LEN=1, KIND=C_CHAR), DIMENSION(:), POINTER :: c_string_arr

    CALL C_F_POINTER(c_string, c_string_arr, [len_c_string])
    CALL f_c_string(f_string, len_c_string, c_string_arr)

  END SUBROUTINE f_c_string_ptr



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get the lowest free logical unit number in the range 100-1000
  !> This function is rendered obsolete by the F2008 NEWUNIT function
  !> Returns -1 if no free lun can be found
  !> @return eis_get_lun
  FUNCTION eis_get_lun()
    !> Lowest LUN found
    INTEGER :: eis_get_lun
    INTEGER, PARAMETER :: lun_min = 100, lun_max = 1000
    LOGICAL :: is_open
    INTEGER :: ilun
    eis_get_lun = -1
    DO ilun = lun_min, lun_max
      INQUIRE(UNIT = ilun, OPENED = is_open)
      IF (.NOT. is_open) THEN
        eis_get_lun = ilun
        EXIT
      END IF
    END DO
 END FUNCTION eis_get_lun



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Opens a file and loads the entire contents into a string
  !> @param[in] filename
  !> @param[out] string
  SUBROUTINE eis_load_file_to_string(filename, string)
    CHARACTER(LEN=*), INTENT(IN) :: filename !< File to open
    !> String variable to hold the contents of the file
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: string
    INTEGER :: lun
    LOGICAL :: exists
    INTEGER(eis_i8) :: fsize

    IF (ALLOCATED(string)) DEALLOCATE(string)
    lun = eis_get_lun()
    IF (lun < 0) RETURN
    INQUIRE(FILE=filename, EXIST=exists, SIZE = fsize)
    IF (.NOT. exists .OR. fsize < 0) RETURN

    ALLOCATE(CHARACTER(LEN=fsize)::string)
    OPEN(FILE=filename, UNIT=lun, STATUS='OLD', ACCESS='STREAM')
    READ(lun) string
    CLOSE(lun)

 END SUBROUTINE eis_load_file_to_string

END MODULE eis_utils
