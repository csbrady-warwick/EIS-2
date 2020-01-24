MODULE eis_utils

  USE, INTRINSIC :: ISO_C_BINDING
  USE eis_constants
  IMPLICIT NONE

  PRIVATE

  PUBLIC :: eis_default_status
  PUBLIC :: eis_append_string, eis_c_f_string, eis_f_c_string
  PUBLIC :: eis_get_lun, eis_load_file_to_string
  PUBLIC :: eis_remove_string_section, eis_copy_string
  PUBLIC :: eis_compare_string, eis_present_and_alloc
  PUBLIC :: eis_integer_as_string, eis_uid_generator
  PUBLIC :: eis_allocate_string

  INTERFACE eis_copy_string
    MODULE PROCEDURE eis_copy_string_aa
#ifdef UNICODE
    MODULE PROCEDURE eis_copy_string_uu
    MODULE PROCEDURE eis_copy_string_ua
    MODULE PROCEDURE eis_copy_string_au
#endif
  END INTERFACE eis_copy_string

  INTERFACE eis_append_string
    MODULE PROCEDURE  eis_append_string_aa
#ifdef UNICODE
    MODULE PROCEDURE  eis_append_string_uu
    MODULE PROCEDURE  eis_append_string_ua
    MODULE PROCEDURE  eis_append_string_au
#endif
  END INTERFACE eis_append_string

  INTERFACE eis_remove_string_section
    MODULE PROCEDURE eis_remove_string_section_ascii
    MODULE PROCEDURE eis_remove_string_section_ascii_char
#ifdef UNICODE
    MODULE PROCEDURE eis_remove_string_section_ucs4
#endif
  END INTERFACE eis_remove_string_section

  INTERFACE eis_uppercase_character
    MODULE PROCEDURE eis_uppercase_character_ascii
  END INTERFACE eis_uppercase_character

  INTERFACE eis_compare_string
    MODULE PROCEDURE eis_compare_string_aa
#ifdef UNICODE
    !MODULE PROCEDURE eis_compare_string_uu
    !MODULE PROCEDURE eis_compare_string_ua
    !MODULE PROCEDURE eis_compare_string_au
#endif
  END INTERFACE eis_compare_string

  INTERFACE eis_c_f_string
    MODULE PROCEDURE c_f_string_array, c_f_string_ptr
  END INTERFACE eis_c_f_string

  INTERFACE eis_f_c_string
    MODULE PROCEDURE f_c_string_array, f_c_string_ptr
  END INTERFACE eis_f_c_string

  INTERFACE eis_integer_as_string
    MODULE PROCEDURE integer4_as_string, integer8_as_string
  END INTERFACE eis_integer_as_string

  TYPE :: eis_uid_generator
    INTEGER(uid_kind) :: new_uid = 0_uid_kind

  CONTAINS
    PROCEDURE :: get_next_uid => eug_get_next_uid
    PROCEDURE :: reset => eug_reset_next_uid
  END TYPE eis_uid_generator    

  CONTAINS


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Sets null defaults for error codes, status codes and or bitmasks
  !> @param[out] errcode
  !> @param[out] status
  !> @param[out] bitmask
  PURE SUBROUTINE eis_default_status(errcode, status, bitmask)
    !> Error code value to be set to default
    INTEGER(eis_error), INTENT(OUT), OPTIONAL :: errcode
    !> Status code value to be set to default
    INTEGER(eis_status), INTENT(OUT), OPTIONAL :: status
    !> Bitmask value to be set to default
    INTEGER(eis_bitmask), INTENT(OUT), OPTIONAL :: bitmask

    IF (PRESENT(errcode)) errcode = eis_err_none
    IF (PRESENT(status)) status = eis_status_none
    IF (PRESENT(bitmask)) bitmask = 0
  END SUBROUTINE eis_default_status



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to copy the new string into the old string. The old string is 
  !> allocated or deallocated as needed. If the new string is not allocated
  !> the old string is guaranteed to not be allocated after this function call
  !> @details
  !> str_dest is only reallocated if it is too short. Both strings must be of
  !> kind ASCII
  !> @param[in] str_src
  !> @param[inout] str_dest
  SUBROUTINE eis_copy_string_aa(str_src, str_dest)
    !> Existing string. Must be ALLOCATABLE, but can be non-allocated
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE, INTENT(IN) :: str_src
    !> String to be copied into. Must be allocatable
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE, INTENT(INOUT) :: str_dest

    IF (.NOT. ALLOCATED(str_src)) THEN
      IF (ALLOCATED(str_dest)) DEALLOCATE(str_dest)
      RETURN
    END IF

    IF (ALLOCATED(str_dest)) THEN
      IF (LEN(str_dest) >= LEN(str_src)) THEN
        str_dest = str_src
        RETURN
      ELSE
        DEALLOCATE(str_dest)
      END IF
    END IF
    ALLOCATE(str_dest, SOURCE = str_src)

  END SUBROUTINE eis_copy_string_aa



#ifdef UNICODE
  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to copy the new string into the old string. The old string is 
  !> allocated or deallocated as needed. If the new string is not allocated
  !> the old string is guaranteed to not be allocated after this function call
  !> @details
  !> str_dest is only reallocated if it is too short. Both strings must be of
  !> kind UCS4
  !> @param[in] str_src
  !> @param[inout] str_dest
  SUBROUTINE eis_copy_string_uu(str_src, str_dest)
    !> Existing string. Must be ALLOCATABLE, but can be non-allocated
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE, INTENT(IN) :: str_src
    !> String to be copied into. Must be allocatable
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE, INTENT(INOUT) :: str_dest

    IF (.NOT. ALLOCATED(str_src)) THEN
      IF (ALLOCATED(str_dest)) DEALLOCATE(str_dest)
      RETURN
    END IF

    IF (ALLOCATED(str_dest)) THEN
      IF (LEN(str_dest) >= LEN(str_src)) THEN
        str_dest = str_src
        RETURN
      ELSE
        DEALLOCATE(str_dest)
      END IF
    END IF
    ALLOCATE(str_dest, SOURCE = str_src)


  END SUBROUTINE eis_copy_string_uu



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to copy the new string into the old string. The old string is 
  !> allocated or deallocated as needed. If the new string is not allocated
  !> the old string is guaranteed to not be allocated after this function call
  !> @details
  !> str_dest is only reallocated if it is too short. str_src must be of kind
  !> UCS4, str_dest must be of kind ASCII
  !> @param[in] str_src
  !> @param[inout] str_dest
  SUBROUTINE eis_copy_string_ua(str_src, str_dest)
    !> Existing string. Must be ALLOCATABLE, but can be non-allocated
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE, INTENT(IN) :: str_src
    !> String to be copied into. Must be allocatable
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE, INTENT(INOUT) :: str_dest

    IF (.NOT. ALLOCATED(str_src)) THEN
      IF (ALLOCATED(str_dest)) DEALLOCATE(str_dest)
      RETURN
    END IF

    IF (ALLOCATED(str_dest)) THEN
      IF (LEN(str_dest) >= LEN(str_src)) THEN
        str_dest = str_src
        RETURN
      ELSE
        DEALLOCATE(str_dest)
      END IF
    END IF
    ALLOCATE(str_dest, SOURCE = str_src)


  END SUBROUTINE eis_copy_string_ua



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to copy the new string into the old string. The old string is 
  !> allocated or deallocated as needed. If the new string is not allocated
  !> the old string is guaranteed to not be allocated after this function call
  !> @details
  !> str_dest is only reallocated if it is too short. str_src must be of kind
  !> ASCII, str_dest must be of kind UCS4
  !> @param[in] str_src
  !> @param[inout] str_dest
  SUBROUTINE eis_copy_string_au(str_src, str_dest)
    !> Existing string. Must be ALLOCATABLE, but can be non-allocated
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE, INTENT(IN) :: str_src
    !> String to be copied into. Must be allocatable
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE, INTENT(INOUT) :: str_dest
  
    IF (.NOT. ALLOCATED(str_src)) THEN
      IF (ALLOCATED(str_dest)) DEALLOCATE(str_dest)
      RETURN
    END IF

    IF (ALLOCATED(str_dest)) THEN
      IF (LEN(str_dest) >= LEN(str_src)) THEN
        str_dest = str_src
        RETURN
      ELSE
        DEALLOCATE(str_dest)
      END IF
    END IF
    ALLOCATE(str_dest, SOURCE = str_src)


  END SUBROUTINE eis_copy_string_au
#endif


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
  !> Remove all characters between init and final inclusive.
  !> If init is < 1 then init becomes 1
  !> If final > LEN(str) then final becomes LEN(str)
  !> @param[inout] str
  !> @param[in] init
  !> @param[in] final
  SUBROUTINE eis_remove_string_section_ascii(str, init, final)
    !> Existing string. Must be ALLOCATABLE, must be allocated
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE, INTENT(INOUT) :: str
    !> Initial character to be removed, inclusive
    INTEGER, INTENT(IN) :: init
    !> Final character to be removed, inclusive
    INTEGER, INTENT(IN) :: final
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE :: temp
    INTEGER :: iind, find, rlen

    IF (.NOT. ALLOCATED(str)) RETURN

    iind = MAX(1, init)
    find = MIN(LEN(str), final)
    rlen = find - iind + 1

    ALLOCATE(CHARACTER(LEN=LEN(str) - rlen) :: temp)
    IF (iind > 1) temp(1:iind-1) = str(1:iind-1)
    IF (find < LEN(str)) temp(iind:) = str(find+1:)
    DEALLOCATE(str)
    CALL MOVE_ALLOC(temp, str)

  END SUBROUTINE eis_remove_string_section_ascii



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Remove all characters between init and final inclusive.
  !> If init is < 1 then init becomes 1
  !> If final > LEN(str) then final becomes LEN(str)
  !> @param[inout] str
  !> @param[in] init_s
  !> @param[in] final_s
  !> @param[in] init_exclude
  !> @param[in] final_exclude
  !> @param[in] first_only
  SUBROUTINE eis_remove_string_section_ascii_char(str, init_s, final_s, &
      init_exclude, final_exclude, first_only)
    !> Existing string. Must be ALLOCATABLE, must be allocated
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE, INTENT(INOUT) :: str
    !> Sequence of characters to start the removal section
    CHARACTER(LEN=*), INTENT(IN) :: init_s
    !> Sequence of characters to end the removal section
    CHARACTER(LEN=*), INTENT(IN) :: final_s
    !> Should removal be exclusive of the init sequence
    !> Optional. Default inclusive removal
    LOGICAL, INTENT(IN), OPTIONAL :: init_exclude
    !> should removal be exclusive of the final sequence
    !> Optional. Default inclusive removal
    LOGICAL, INTENT(IN), OPTIONAL :: final_exclude
    !> Should the removal only be of the first instance of the sequence
    !> Optional. Default remove all instances
    LOGICAL, INTENT(IN), OPTIONAL :: first_only
    LOGICAL :: init_e, final_e, fo, iter_on
    INTEGER :: ipos, fpos


    IF (.NOT. ALLOCATED(str)) RETURN
    init_e = .FALSE.;final_e = .FALSE.;fo=.FALSE.
    IF (PRESENT(init_exclude)) init_e = init_exclude
    IF (PRESENT(final_exclude)) final_e = final_exclude
    IF (PRESENT(first_only)) fo = first_only

    iter_on = .TRUE.
    DO WHILE(iter_on)
      ipos = INDEX(str, init_s)
      IF (ipos > 0) THEN
        IF (init_e) THEN
          ipos = ipos + LEN(init_s)
        END IF
      END IF
      fpos = INDEX(str(ipos + LEN(init_s):), final_s)
      IF (fpos > 0) THEN
        fpos = fpos + ipos + LEN(init_s) - 1
        IF (final_e) THEN
          fpos = fpos - 1
        ELSE
          fpos = fpos + LEN(final_s) - 1
        END IF
      END IF

      IF (ipos > 0 .AND. fpos <= 0) fpos = LEN(str)
      iter_on = ipos > 0 .AND. fpos > 0

      IF (iter_on) THEN
        CALL eis_remove_string_section_ascii(str, ipos, fpos)
      END IF
      iter_on = iter_on .AND. .NOT. fo
    END DO

  END SUBROUTINE eis_remove_string_section_ascii_char



#ifdef UNICODE
  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Remove all characters between init and final inclusive.
  !> If init is < 1 then init becomes 1
  !> If final > LEN(str) then final becomes LEN(str)
  !> @param[inout] str
  !> @param[in] init
  !> @param[in] final
  SUBROUTINE eis_remove_string_section_ucs4(str, init, final)
    !> Existing string. Must be ALLOCATABLE, must be allocated
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE, INTENT(INOUT) :: str
    !> Initial character to be removed, inclusive
    INTEGER, INTENT(IN) :: init
    !> Final character to be removed, inclusive
    INTEGER, INTENT(IN) :: final
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE :: temp
    INTEGER :: iind, find, rlen

    IF (.NOT. ASSOCIATED(str)) RETURN

    iind = MAX(1, init)
    find = MIN(LEN(str), final)
    rlen = find - iind + 1

    ALLOCATE(CHARACTER(LEN=rlen) :: temp)
    IF (iind > 1) temp(1:iind-1) = str(1:iind-1)
    IF (find < LEN(str)) temp(iind:) = str(find+1:)
    DEALLOCATE(str)
    CALL MOVE_ALLOC(temp, str)

  END SUBROUTINE eis_remove_string_section_ucs4
#endif


  !> @brief
  !> Produces the upper case equivalent of a single character
  !> @param[in] c
  !> @return uppercase
  FUNCTION eis_uppercase_character_ascii(c) RESULT(uppercase)
    CHARACTER(LEN=1, KIND=ASCII), INTENT(IN) :: c
    CHARACTER(LEN=1, KIND=ASCII) :: uppercase

    IF (IACHAR(c) >= IACHAR('a') .AND. IACHAR(c) <= IACHAR('z')) THEN
      uppercase = ACHAR(IACHAR(c) - IACHAR('a') + IACHAR('A'))
    ELSE
      uppercase = c
    END IF

  END FUNCTION eis_uppercase_character_ascii



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to test if two strings are equal
  !> @details
  !> Both strings must be of kind ASCII
  !> @param[in] str1
  !> @param[in] str2
  !> @param[in] case_sensitive
  !> @return same
  FUNCTION eis_compare_string_aa(str1, str2, case_sensitive) RESULT(same)
    !> First string to compare
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN) :: str1
    !> Second string to compare
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN) :: str2
    !> Whether to perform case sensitive comparison or not
    !> Optional, if not present default .FALSE.
    LOGICAL, INTENT(IN), OPTIONAL :: case_sensitive
    !> The result of the comparison operation
    LOGICAL :: same
    LOGICAL :: cs
    CHARACTER(LEN=1, KIND=ASCII) :: c1, c2
    INTEGER :: istr

    cs = .FALSE.
    IF (PRESENT(case_sensitive)) cs = case_sensitive
    same = .FALSE.
    IF (LEN(str1) /= LEN(str2)) RETURN
    DO istr = 1, LEN(str1)
      c1 = str1(istr:istr); c2 = str2(istr:istr)
      IF (.NOT. cs) THEN
        c1 = eis_uppercase_character(c1)
        c2 = eis_uppercase_character(c2)
      END IF
      IF (c1 /= c2) RETURN
    END DO
    same = .TRUE.

  END FUNCTION eis_compare_string_aa




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
    CALL eis_f_c_string(f_string, len_c_string, c_string_arr)

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



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Tests if an optional string parameters is both present and allocated
  !> @param[in] string
  !> @return eis_present_and_alloc
  FUNCTION eis_present_and_alloc(string)
    !> Allocatable optional string to test
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(IN) :: string
    !Is this character string both present and allocated if present
    LOGICAL :: eis_present_and_alloc

    eis_present_and_alloc = PRESENT(string)
    IF (eis_present_and_alloc) eis_present_and_alloc = eis_present_and_alloc &
        .AND. ALLOCATED(string)

  END FUNCTION eis_present_and_alloc


  !> @author Keith Bennett <K.Bennett@warwick.ac.uk>
  !> @brief
  !> Returns an integer as the smallest string containing it
  !> taken from EPOCH
  !> @param[in] int_in
  !> @param[out] string
  SUBROUTINE integer4_as_string(int_in, string)

    INTEGER(INT32), INTENT(IN) :: int_in
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: string

    INTEGER :: n_nums
    CHARACTER(LEN=9) :: numfmt

    IF (int_in == 0) THEN
      n_nums = 1
    ELSE
      n_nums = 1 + INT(LOG10(REAL(ABS(int_in), eis_num)))
    END IF
    IF (int_in < 0) n_nums = n_nums + 1
    ALLOCATE(CHARACTER(LEN=n_nums) :: string)
    WRITE(numfmt, '(''(I'', I6.6, '')'')') n_nums
    WRITE(string, numfmt) int_in

  END SUBROUTINE integer4_as_string



  !> @author Keith Bennett <K.Bennett@warwick.ac.uk>
  !> @brief
  !> Returns a long integer as the smallest string containing it
  !> taken from EPOCH
  !> @param[in] int_in
  !> @param[out] string
  SUBROUTINE integer8_as_string(int_in, string)

    INTEGER(INT64), INTENT(IN) :: int_in
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: string

    INTEGER :: n_nums
    CHARACTER(LEN=12) :: numfmt

    IF (int_in == 0) THEN
      n_nums = 1
    ELSE
      n_nums = 1 + INT(LOG10(REAL(ABS(int_in), eis_num)))
    END IF
    IF (int_in < 0) n_nums = n_nums + 1
    ALLOCATE(CHARACTER(LEN=n_nums)::string)
    WRITE(numfmt, '(''(I'', I9.9, '')'')') n_nums
    WRITE(string, numfmt) int_in

  END SUBROUTINE integer8_as_string



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Gets the next uid from this UID generator
  !> @param[inout] this
  !> @return eug_get_next_uid
  FUNCTION eug_get_next_uid(this)
    CLASS(eis_uid_generator), INTENT(INOUT) :: this
    INTEGER(uid_kind) :: new_uid
    INTEGER(uid_kind) :: eug_get_next_uid

    eug_get_next_uid = INT(this%new_uid)
    this%new_uid = this%new_uid + 1

  END FUNCTION eug_get_next_uid



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Resets the UID generator
  !> @param[inout] this
  !> @param[in]
  !> @return eug_get_next_uid
  SUBROUTINE eug_reset_next_uid(this, value)
    CLASS(eis_uid_generator), INTENT(INOUT) :: this
    INTEGER(uid_kind), INTENT(IN), OPTIONAL :: value
    INTEGER(uid_kind) :: eug_get_next_uid
    INTEGER(uid_kind) :: val

    val = 0
    IF (PRESENT(value)) val = value
    this%new_uid = val

  END SUBROUTINE eug_reset_next_uid



  !>@brief
  !>Routine to allocate a string to either a zero length string
  !>or an optionally supplied string. If string is allocated
  !>then it is deallocated on entry
  SUBROUTINE eis_allocate_string(dest, src)
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: dest
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: src

    IF (PRESENT(src)) THEN
      ALLOCATE(dest, SOURCE = src)
    ELSE
      ALLOCATE(dest, SOURCE = "")
    END IF
  END SUBROUTINE eis_allocate_string

END MODULE eis_utils
