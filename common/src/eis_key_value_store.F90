MODULE eis_key_value_store_mod

  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE eis_constants
  USE eis_named_store_mod
  USE eis_utils
  USE eis_string_store_mod

  IMPLICIT NONE

  !>@class
  !>Class holding the string within the string store
  !>If compiled with support is UCS4 string
  TYPE :: string_holder
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE :: text
    CONTAINS
    FINAL :: sh_destructor
  END TYPE string_holder

  !>@class
  !>Key_value store. Maps a key string to a stored string
  !> key string is always ASCII, stored string is UCS4 if available
  TYPE :: eis_key_value_store
    PRIVATE
    TYPE(named_store) :: strings

    CONTAINS
    PROCEDURE :: parse_key_string => ekv_parse_key_string
#ifdef UNICODE
    PROCEDURE :: store_string_ucs4 => ekv_store_string_ucs4
    PROCEDURE :: get_string_ucs4 => ekv_get_string_ucs4
    PROCEDURE :: store_string_ascii => ekv_store_string_ascii
    PROCEDURE :: get_string_ascii => ekv_get_string_ascii
    PROCEDURE :: append_string_ucs4 => ekv_append_string_ucs4
    PROCEDURE :: append_string_ascii => ekv_append_string_ascii
    PROCEDURE :: format_fill_ucs4 => ekv_format_fill_ucs4
    PROCEDURE :: format_fill_ascii => ekv_format_fill_ascii

    GENERIC, PUBLIC :: store => store_string_ucs4, store_string_ascii
    GENERIC, PUBLIC :: get => get_string_ucs4, get_string_ascii
    GENERIC, PUBLIC :: append => append_string_ucs4, append_string_ascii
    GENERIC, PUBLIC :: format_fill => format_fill_uu, format_fill_aa
#else
    PROCEDURE, PUBLIC :: store => ekv_store_string_ascii
    PROCEDURE, PUBLIC :: get => ekv_get_string_ascii
    PROCEDURE, PUBLIC :: append => ekv_append_string_ascii
    PROCEDURE, PUBLIC :: format_fill => ekv_format_fill_aa
#endif

    PROCEDURE, PUBLIC :: load_from_ascii_file => ekv_load_from_ascii_file
    
  END TYPE eis_key_value_store

  PRIVATE
  PUBLIC :: eis_key_value_store

CONTAINS

  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Deallocates the item held in the store
  !> Not strictly needed per standard but here for
  !> future use and clarity
  !> @param[inout] this
  SUBROUTINE sh_destructor(this)
    TYPE(string_holder), INTENT(INOUT) :: this
    IF (ALLOCATED(this%text)) DEALLOCATE(this%text)
  END SUBROUTINE sh_destructor



#ifdef UNICODE
  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Stores a string using a named key. String is UCS4
  !> @param[inout] this
  !> @param[in] key
  !> @param[in] text
  SUBROUTINE ekv_store_string_ucs4(this, key, text)
    CLASS(eis_key_value_store), INTENT(INOUT) :: this
    !> Key associated with string
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN) :: key
    !> String to store
    CHARACTER(LEN=*, KIND=UCS4), INTENT(IN) :: text
    TYPE(string_holder) :: temp

    ALLOCATE(temp%text, SOURCE = text)
    CALL this%strings%store(key, temp)

  END SUBROUTINE ekv_store_string_ucs4
#endif



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Stores a string using a named key. String is ASCII
  !> @param[inout] this
  !> @param[in] key
  !> @param[in] text
  SUBROUTINE ekv_store_string_ascii(this, key, text)
    CLASS(eis_key_value_store), INTENT(INOUT) :: this
    !> Key associated with string
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN) :: key
    !> String to store
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN) :: text
    TYPE(string_holder) :: temp

    ALLOCATE(CHARACTER(LEN=LEN(text), KIND=UCS4)::temp%text)
    temp%text = text
    CALL this%strings%store(key, temp)

  END SUBROUTINE ekv_store_string_ascii


#ifdef UNICODE
  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Retrieves a string from the store and returns it
  !> in a UCS4 string. The string must be allocatable.
  !> If the string is unallocated on entry then it is allocated to be large
  !> enough to hold the returned string. If the string is allocated and large
  !> enough to hold the returned string then it is set to the returned string
  !> but the size is not changed. If the string is allocated but is too small
  !> to hold the returned string then it is reallocated large enough to hold
  !> the returned string.
  !> @param[inout] this
  !> @param[in] key
  !> @param[inout] text
  !> @return ekv_get_string_ucs4
  FUNCTION ekv_get_string_ucs4(this, key, text)
    CLASS(eis_key_value_store), INTENT(IN) :: this
    !> Key to retrieve
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN) :: key
    !> Variable to hold returned string
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE, INTENT(INOUT) :: text
    LOGICAL :: ekv_get_string_ucs4 !< Was key found in store
    CLASS(*), POINTER :: gptr
    TYPE(string_holder), POINTER :: temp

    gptr => this%strings%get(key)
    IF (ASSOCIATED(gptr)) THEN
      SELECT TYPE (co=>gptr)
        CLASS IS (string_holder)
          temp => co
        CLASS DEFAULT
          temp => NULL()
      END SELECT
    END IF

    ekv_get_string_ucs4 = ASSOCIATED(temp)
    IF (ekv_get_string_ucs4) THEN
      IF (.NOT. ALLOCATED(text)) THEN
        ALLOCATE(text, SOURCE = temp%text)
      ELSE
        IF (LEN(text) >= LEN(temp%text)) THEN
          text = temp%text
        ELSE
          DEALLOCATE(text)
          ALLOCATE(text, SOURCE = temp%text)
        END IF
      END IF
    END IF

  END FUNCTION ekv_get_string_ucs4
#endif



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Retrieves a string from the store and returns it
  !> in an ASCII string. The string must be allocatable.
  !> If the string is unallocated on entry then it is allocated to be large
  !> enough to hold the returned string. If the string is allocated and large
  !> enough to hold the returned string then it is set to the returned string
  !> but the size is not changed. If the string is allocated but is too small
  !> to hold the returned string then it is reallocated large enough to hold
  !> the returned string.
  !> @param[inout] this
  !> @param[in] key
  !> @param[inout] text
  !> @return ekv_get_string_ascii
  FUNCTION ekv_get_string_ascii(this,key, text)
    CLASS(eis_key_value_store), INTENT(IN) :: this
    !> Key to retrieve
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN) :: key
    !> Variable to hold returned string
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE, INTENT(INOUT) :: text
    LOGICAL :: ekv_get_string_ascii !< Was key found in store
    CLASS(*), POINTER :: gptr
    TYPE(string_holder), POINTER :: temp

    gptr => this%strings%get(key)
    IF (ASSOCIATED(gptr)) THEN
      SELECT TYPE (co=>gptr)
        CLASS IS (string_holder)
          temp => co
        CLASS DEFAULT
          temp => NULL()
      END SELECT
    END IF

    ekv_get_string_ascii = ASSOCIATED(temp)
    IF (ekv_get_string_ascii) THEN
      IF (.NOT. ALLOCATED(text)) THEN
        ALLOCATE(CHARACTER(LEN=LEN(temp%text), KIND = ASCII)::text)
        text = temp%text
      ELSE
        IF (LEN(text) >= LEN(temp%text)) THEN
          text = temp%text
        ELSE
          DEALLOCATE(text)
          IF(ALLOCATED(temp%text)) THEN
            ALLOCATE(CHARACTER(LEN=LEN(temp%text), KIND = ASCII)::text)
            text = temp%text
          END IF
        END IF
      END IF
    END IF

  END FUNCTION ekv_get_string_ascii



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Retrieves a string from the store and appends it to an existing UCS4
  !> string. The string must be allocatable.
  !> If the string is unallocated on entry then it is allocated to be large
  !> enough to hold the returned string. If the string is allocated then it will
  !> always be reallocated to be large enough to hold exactly the combination of
  !> the existing string and the new string. No TRIM or ADJUST operation is
  !> applied so if the existing content of "text" is smaller than the length of
  !> "text" the rest of the original "text" will appear as spaces in the 
  !> combined string.
  !> So for example combining 'test   ' with 'test2' will yield 'test   test2'
  !> @param[inout] this
  !> @param[in] key
  !> @param[inout] text
  !> @param[in] newline
  FUNCTION ekv_append_string_ucs4(this, key, text, newline)
    CLASS(eis_key_value_store), INTENT(IN) :: this
    !> Key to retrieve
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN) :: key
    !> Text variable to append with output
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE, INTENT(INOUT) :: text
    !> Logical flag to combine strings with a newline character between them.
    !> Optional, default .TRUE. (put newline between strings)
    LOGICAL, INTENT(IN), OPTIONAL :: newline
    LOGICAL :: ekv_append_string_ucs4
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE :: retreived

    ekv_append_string_ucs4 = this%get(key, retreived)
    IF (.NOT. ekv_append_string_ucs4) RETURN

    CALL eis_append_string(text, retreived, newline = newline)

  END FUNCTION ekv_append_string_ucs4


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Retrieves a string from the store and appends it to an existing ASCII
  !> string. The string must be allocatable.
  !> If the string is unallocated on entry then it is allocated to be large
  !> enough to hold the returned string. If the string is allocated then it will
  !> always be reallocated to be large enough to hold exactly the combination of
  !> the existing string and the new string. No TRIM or ADJUST operation is
  !> applied so if the existing content of "text" is smaller than the length of
  !> "text" the rest of the original "text" will appear as spaces in the 
  !> combined string.
  !> So for example combining 'test   ' with 'test2' will yield 'test   test2'
  !> @param[inout] this
  !> @param[in] key
  !> @param[inout] text
  !> @param[in] newline
  FUNCTION ekv_append_string_ascii(this, key, text, newline)
    CLASS(eis_key_value_store), INTENT(IN) :: this
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN) :: key
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE, INTENT(INOUT) :: text
    LOGICAL, INTENT(IN), OPTIONAL :: newline
    LOGICAL :: ekv_append_string_ascii
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE :: retreived

    ekv_append_string_ascii = this%get(key, retreived)
    IF (.NOT. ekv_append_string_ascii) RETURN

    CALL eis_append_string(text, retreived, newline = newline)

  END FUNCTION ekv_append_string_ascii


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to do formatted replacement of text based on keys stored in this
  !> store
  !> @details
  !> This routine takes a string that contains replacement keys of the form
  !> "{key}" and replaces them with the text associated with they key "key"
  !> stored in this store
  !> @param[in] this
  !> @param[inout] text
  SUBROUTINE ekv_format_fill_aa(this, text)
    CLASS(eis_key_value_store), INTENT(IN) :: this
    !> String containing the text with replacement keys in. Returns completed 
    !> text
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE, INTENT(INOUT) :: text
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE :: temp, temp_o
    INTEGER :: istr, rstr
    LOGICAL :: recording_name, ok

    IF (.NOT. ALLOCATED(text)) RETURN

    ALLOCATE(CHARACTER(LEN=LEN(text), KIND=ASCII) :: temp)
    recording_name = .FALSE.
    rstr = 1
    DO istr = 1, LEN(text)
      IF (.NOT. recording_name) THEN
        IF (text(istr:istr) /= '{') THEN
          temp(rstr:rstr) = text(istr:istr)
          rstr = rstr + 1
        ELSE
          CALL eis_append_string(temp_o, temp(1:rstr-1), newline = .FALSE.)
          recording_name = .TRUE.
          rstr = 1
        END IF
      ELSE
        IF (text(istr:istr) /= '}') THEN
          temp(rstr:rstr) = text(istr:istr)
          rstr = rstr+1
        ELSE
          ok = this%append(temp(1:rstr-1), temp_o, newline = .FALSE.)
          recording_name = .FALSE.
          rstr = 1
        END IF
      END IF
    END DO
    CALL eis_append_string(temp_o, temp(1:rstr-1), newline = .FALSE.)
    DEALLOCATE(text)
    ALLOCATE(text, SOURCE = temp_o)
    IF (ALLOCATED(temp_o)) DEALLOCATE(temp_o)
    IF (ALLOCATED(temp)) DEALLOCATE(temp)

  END SUBROUTINE ekv_format_fill_aa



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Routine to add keys to a store from an external file
  !> data should be stored in the form "key=value"
  !> @details
  !> Lines can optionally be comments by having the first non whitespace
  !> character be a hash "#" character. Lines can also have a comment appended
  !> to the end of them by putting in a # character. Lines can be continued
  !> by adding a "\" character as the last character on the line
  !> @param[in] this
  !> @param[in] filename
  !> @param[inout] errcode
  SUBROUTINE ekv_load_from_ascii_file(this, filename, errcode)
    CLASS(eis_key_value_store), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: filename
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER, DIMENSION(2) :: ranges
    TYPE(eis_string_store) :: raw_strings
    CHARACTER(LEN=:), ALLOCATABLE :: str
    INTEGER :: istr
    LOGICAL :: ok

    ranges = raw_strings%load_from_ascii_file(filename, errcode)
    CALL raw_strings%remove_blank_lines()
    CALL raw_strings%combine_split_lines("\")
    DO istr = 1, raw_strings%get_size()
      ok = raw_strings%get(istr, str)
      CALL this%parse_key_string(str)
    END DO

  END SUBROUTINE ekv_load_from_ascii_file



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Routine to take a single string of the form
  !> "key=value", split it into key and value components
  !> and store them in the store
  !> @param[in] this
  !> @param[in] string
  SUBROUTINE ekv_parse_key_string(this, string)
    CLASS(eis_key_value_store), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: string
    CHARACTER(LEN=:), ALLOCATABLE :: trstr
    INTEGER :: eqpos, hashpos

    CALL eis_append_string(trstr, TRIM(ADJUSTL(string)))

    IF (trstr(1:1) == '#') RETURN

    eqpos = SCAN(trstr, '=')
    hashpos = SCAN(trstr, '#')

    !No line terminal comment
    IF (hashpos < 1) hashpos = LEN(trstr) + 1
    !Malformed string without an equals. Not much we can do
    IF (eqpos < 1) RETURN
    !Malformed string with a comment before the equals sign. Not much we can do
    IF (hashpos < eqpos) RETURN
    !Everything before the equals is a key, everything between the equals and 
    !an optional #comment is the value
    CALL this%store(trstr(1:eqpos-1), trstr(eqpos+1:hashpos-1))

    DEALLOCATE(trstr)

  END SUBROUTINE ekv_parse_key_string

END MODULE eis_key_value_store_mod
