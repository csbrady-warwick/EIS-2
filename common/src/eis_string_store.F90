MODULE eis_string_store_mod

  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE eis_constants
  USE eis_header
  USE eis_ordered_store_mod
  USE eis_utils

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
  !>String line store. Stores a list of strings by index number
  !>stored strings are UCS4 if available
  TYPE :: eis_string_store
    PRIVATE
    TYPE(ordered_store) :: strings

    CONTAINS
#ifdef UNICODE
    PROCEDURE :: store_string_ucs4 => ess_store_string_ucs4
    PROCEDURE :: get_string_ucs4 => ess_get_string_ucs4
    PROCEDURE :: store_string_ascii => ess_store_string_ascii
    PROCEDURE :: get_string_ascii => ess_get_string_ascii
    PROCEDURE :: append_string_ucs4 => ess_append_string_ucs4
    PROCEDURE :: append_string_ascii => ess_append_string_ascii
    PROCEDURE :: insert_string_ucs4 => ess_insert_string_ucs4
    PROCEDURE :: insert_string_ascii => ess_insert_string_ascii
    PROCEDURE :: populate_from_ascii => ess_populate_from_ascii
    PROCEDURE :: populate_from_ucs4 => ess_popuate_from_ucs4

    GENERIC, PUBLIC :: store => store_string_ucs4, store_string_ascii
    GENERIC, PUBLIC :: get => get_string_ucs4, get_string_ascii
    GENERIC, PUBLIC :: append => append_string_ucs4, append_string_ascii
    GENERIC, PUBLIC :: insert => insert_string_ucs4, insert_string_ascii
    GENERIC, PUBLIC :: populate => populate_from_ascii, populate_from_ucs4
#else
    PROCEDURE, PUBLIC :: store => ess_store_string_ascii
    PROCEDURE, PUBLIC :: get => ess_get_string_ascii
    PROCEDURE, PUBLIC :: append => ess_append_string_ascii
    PROCEDURE, PUBLIC :: insert => ess_insert_string_ascii
    PROCEDURE, PUBLIC :: populate => ess_populate_from_ascii
#endif
    PROCEDURE, PUBLIC :: delete => ess_delete
    PROCEDURE, PUBLIC :: get_size => ess_get_size
    PROCEDURE, PUBLIC :: load_from_ascii_file => ess_load_from_ascii_file
    PROCEDURE, PUBLIC :: remove_blank_lines => ess_remove_blank_lines
    PROCEDURE, PUBLIC :: combine_split_lines => ess_combine_split_lines
    
  END TYPE eis_string_store

  PRIVATE
  PUBLIC :: eis_string_store

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



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Returns the number of stored strings
  !> @param[inout] this
  !> @return count
  FUNCTION ess_get_size(this) RESULT(size)
    CLASS(eis_string_store), INTENT(INOUT) :: this
    !> Number of strings in the store
    INTEGER(eis_i4) :: size

    size = this%strings%get_size()

  END FUNCTION ess_get_size



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Deletes an item from the store
  !> @param[inout] this
  !> @param[in] index
  !> @return success
  FUNCTION ess_delete(this, index) RESULT(success)
    CLASS(eis_string_store), INTENT(INOUT) :: this
    !> Index of item to delete
    INTEGER, INTENT(IN) :: index
    !> Whether or not the deletion succeded
    LOGICAL :: success
    LOGICAL :: disorder

    disorder = this%strings%can_disorder
    this%strings%can_disorder = .TRUE.
    success = this%strings%delete(index)
    this%strings%can_disorder = disorder

  END FUNCTION ess_delete



#ifdef UNICODE
  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Stores a string to a numbered item. String is UCS4
  !> @param[inout] this
  !> @param[in] text
  !> @param[in] index
  !> @return index_out
  FUNCTION ess_store_string_ucs4(this, text, index) RESULT(index_out)
    CLASS(eis_string_store), INTENT(INOUT) :: this
    !> String to store
    CHARACTER(LEN=*, KIND=UCS4), INTENT(IN) :: text
    !> Index to store the string in. Optional, default store at end
    INTEGER, INTENT(IN), OPTIONAL :: index
    !> Index of stored string
    INTEGER(eis_i4) :: index_out
    TYPE(string_holder) :: temp

    ALLOCATE(temp%text, SOURCE = text)
    index_out = this%strings%store(temp, index)

  END FUNCTION ess_store_string_ucs4
#endif


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Stores a string to a numbered item. String is ASCII
  !> @param[inout] this
  !> @param[in] text
  !> @param[in] index
  !> @return index_out
  FUNCTION ess_store_string_ascii(this, text, index) RESULT(index_out)
    CLASS(eis_string_store), INTENT(INOUT) :: this
    !> String to store
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN) :: text
    !> Index to store the string in. Optional, default store at end
    INTEGER, INTENT(IN), OPTIONAL :: index
    !> Index of stored string
    INTEGER(eis_i4) :: index_out
    TYPE(string_holder) :: temp

    ALLOCATE(temp%text, SOURCE = text)
    index_out = this%strings%store(temp, index)

  END FUNCTION ess_store_string_ascii



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
  !> @param[in] index
  !> @param[inout] text
  !> @return ess_get_string_ucs4
  FUNCTION ess_get_string_ucs4(this, index, text)
    CLASS(eis_string_store), INTENT(IN) :: this
    !> Index to retrieve
    INTEGER(eis_i4), INTENT(IN) :: index
    !> Variable to hold returned string
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE, INTENT(INOUT) :: text
    LOGICAL :: ess_get_string_ucs4 !< Was index found in store
    CLASS(*), POINTER :: gptr
    TYPE(string_holder), POINTER :: temp

    gptr => this%strings%get(index)
    IF (ASSOCIATED(gptr)) THEN
      SELECT TYPE (co=>gptr)
        CLASS IS (string_holder)
          temp => co
        CLASS DEFAULT
          temp => NULL()
      END SELECT
    END IF

    ess_get_string_ucs4 = ASSOCIATED(temp)
    IF (ess_get_string_ucs4) THEN
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

  END FUNCTION ess_get_string_ucs4
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
  !> @param[in] index
  !> @param[inout] text
  !> @return ess_get_string_ascii
  FUNCTION ess_get_string_ascii(this, index, text)
    CLASS(eis_string_store), INTENT(IN) :: this
    !> Key to retrieve
    INTEGER(eis_i4), INTENT(IN) :: index
    !> Variable to hold returned string
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE, INTENT(INOUT) :: text
    LOGICAL :: ess_get_string_ascii !< Was index found in store
    CLASS(*), POINTER :: gptr
    TYPE(string_holder), POINTER :: temp

    gptr => this%strings%get(index)
    IF (ASSOCIATED(gptr)) THEN
      SELECT TYPE (co=>gptr)
        CLASS IS (string_holder)
          temp => co
        CLASS DEFAULT
          temp => NULL()
      END SELECT
    END IF

    ess_get_string_ascii = ASSOCIATED(temp)
    IF (ess_get_string_ascii) THEN
      IF (.NOT. ALLOCATED(text)) THEN
        ALLOCATE(CHARACTER(LEN=LEN(temp%text), KIND = ASCII)::text)
        text = temp%text
      ELSE
        IF (LEN(text) >= LEN(temp%text)) THEN
          text = temp%text
        ELSE
          DEALLOCATE(text)
          ALLOCATE(CHARACTER(LEN=LEN(temp%text), KIND = ASCII)::text)
          text = temp%text
        END IF
      END IF
    END IF

  END FUNCTION ess_get_string_ascii



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
  !> @param[in] index
  !> @param[inout] text
  !> @param[in] newline
  FUNCTION ess_append_string_ucs4(this, index, text, newline)
    CLASS(eis_string_store), INTENT(IN) :: this
    !> Index to retrieve
    INTEGER(eis_i4), INTENT(IN) :: index
    !> Text variable to append with output
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE, INTENT(INOUT) :: text
    !> Logical flag to combine strings with a newline character between them.
    !> Optional, default .TRUE. (put newline between strings)
    LOGICAL, INTENT(IN), OPTIONAL :: newline
    LOGICAL :: ess_append_string_ucs4
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE :: retreived

    ess_append_string_ucs4 = this%get(index, retreived)
    IF (.NOT. ess_append_string_ucs4) RETURN

    CALL eis_append_string(text, retreived, newline = newline)

  END FUNCTION ess_append_string_ucs4


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
  !> @param[in] index
  !> @param[inout] text
  !> @param[in] newline
  FUNCTION ess_append_string_ascii(this, index, text, newline)
    CLASS(eis_string_store), INTENT(IN) :: this
    INTEGER(eis_i4), INTENT(IN) :: index
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE, INTENT(INOUT) :: text
    LOGICAL, INTENT(IN), OPTIONAL :: newline
    LOGICAL :: ess_append_string_ascii
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE :: retreived

    ess_append_string_ascii = this%get(index, retreived)
    IF (.NOT. ess_append_string_ascii) RETURN

    CALL eis_append_string(text, retreived, newline = newline)

  END FUNCTION ess_append_string_ascii




#ifdef UNICODE
  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Stores a string to a numbered item. String is UCS4
  !> @param[inout] this
  !> @param[in] text
  !> @return index
  FUNCTION ess_insert_string_ucs4(this, text) RESULT(index)
    CLASS(eis_string_store), INTENT(INOUT) :: this
    !> String to store
    CHARACTER(LEN=*, KIND=UCS4), INTENT(IN) :: text
    !> Index of stored string
    INTEGER(eis_i4) :: index
    TYPE(string_holder) :: temp

    ALLOCATE(temp%text, SOURCE = text)
    index = this%strings%store(temp)

  END FUNCTION ess_insert_string_ucs4
#endif


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Stores a string to a numbered item. String is ASCII
  !> @param[inout] this
  !> @param[in] text
  !> @return index
  FUNCTION ess_insert_string_ascii(this, dest_index, text) RESULT(index)
    CLASS(eis_string_store), INTENT(INOUT) :: this
    !> Index at which to insert text
    INTEGER, INTENT(IN) :: dest_index
    !> String to store
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN) :: text
    !> Index of stored string
    INTEGER(eis_i4) :: index
    TYPE(string_holder) :: temp
    LOGICAL :: disorder

    ALLOCATE(temp%text, SOURCE = text)
    disorder = this%strings%can_disorder
    this%strings%can_disorder = .TRUE.
    index = this%strings%insert(temp, dest_index)
    this%strings%can_disorder = disorder

  END FUNCTION ess_insert_string_ascii



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Routine to add lines from a single string
  !> @details
  !> Lines can be continued by adding a "\" character as the last character
  !> on the line. Blank lines will still be added as blank lines
  !> @param[in] this
  !> @param[in] str_in
  !> @param[inout] errcode
  !> @result index_range
  FUNCTION ess_populate_from_ascii(this, str, errcode, index_start) &
      RESULT(index_range)
    CLASS(eis_string_store), INTENT(INOUT) :: this
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN) :: str
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER, INTENT(IN), OPTIONAL :: index_start
    INTEGER, DIMENSION(2) :: index_range
    INTEGER :: newline_pos, newline_offset, last_pos
    INTEGER :: min_index, max_index, indx
    CHARACTER(LEN=:), ALLOCATABLE :: str_compose

    errcode = eis_err_none

    IF (PRESENT(index_start)) indx = index_start

    newline_pos = INDEX(str, NEW_LINE(str))
    newline_offset = newline_pos
    last_pos = 1
    min_index = HUGE(min_index)
    max_index = 0
    DO WHILE(newline_offset > 0)
      IF (last_pos /= newline_pos) THEN
        IF (PRESENT(index_start)) THEN
          indx = this%insert(indx, str(last_pos:newline_pos - 1))
          indx = indx + 1
        ELSE
          indx = this%store(str(last_pos:newline_pos - 1))
        END IF
      ELSE
        IF (PRESENT(index_start)) THEN
          indx = this%insert(indx, "")
          indx = indx + 1
        ELSE
          indx = this%store("")
        END IF
      END IF
      min_index = MIN(indx, min_index)
      max_index = MAX(indx, max_index)
      last_pos = newline_pos + LEN(NEW_LINE(str))
      newline_offset = INDEX(str(last_pos:), NEW_LINE(str))
      newline_pos = newline_offset + (last_pos - 1)
    END DO

    index_range = [min_index, max_index]

  END FUNCTION ess_populate_from_ascii



#ifdef UNICODE
  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Routine to add lines from a single string
  !> @details
  !> Lines can be continued by adding a "\" character as the last character
  !> on the line. Blank lines will still be added as blank lines
  !> @param[in] this
  !> @param[in] str_in
  !> @param[inout] errcode
  !> @result index_range
  FUNCTION ess_populate_from_ucs4(this, str, errcode, index_start) &
      RESULT(index_range)
    CLASS(eis_string_store), INTENT(INOUT) :: this
    CHARACTER(LEN=*, KIND=UCS4), INTENT(IN) :: str
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER, INTENT(IN), OPTIONAL :: index_start
    INTEGER, DIMENSION(2) :: index_range
    INTEGER :: newline_pos, newline_offset, last_pos
    INTEGER :: min_index, max_index, indx

    errcode = eis_err_none

    IF (PRESENT(index_start)) indx = index_start

    newline_pos = INDEX(str, NEW_LINE(str))
    newline_offset = newline_pos
    last_pos = 1
    min_index = HUGE(min_index)
    max_index = 0
    DO WHILE(newline_offset > 0)
      IF (last_pos /= newline_pos) THEN
        IF (PRESENT(index_start)) THEN
          indx = this%insert(indx, str(last_pos:newline_pos - 1))
          indx = indx + 1
        ELSE
          indx = this%store(str(last_pos:newline_pos - 1))
        END IF
      ELSE
        IF (PRESENT(index_start)) THEN
          indx = this%insert(indx, "")
          indx = indx + 1
        ELSE
          indx = this%store("")
        END IF
      END IF
      min_index = MIN(indx, min_index)
      max_index = MAX(indx, max_index)
      last_pos = newline_pos + LEN(NEW_LINE(str))
      newline_offset = INDEX(str(last_pos:), NEW_LINE(str))
      newline_pos = newline_offset + (last_pos - 1)
    END DO

    index_range = [min_index, max_index]

  END FUNCTION ess_populate_from_ucs4
#endif



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Routine to add keys to a store from an external file
  !> data should be stored in the form "key=value"
  !> @details
  !> Lines can be continued by adding a "\" character as the last character
  !> on the line. Blank lines will still be added as blank lines
  !> @param[in] this
  !> @param[in] filename
  !> @param[inout] errcode
  !> @result index_range
  FUNCTION ess_load_from_ascii_file(this, filename, errcode, index_start) &
      RESULT(index_range)
    CLASS(eis_string_store), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: filename
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER, INTENT(IN), OPTIONAL :: index_start
    INTEGER, DIMENSION(2) :: index_range
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE :: str
    INTEGER :: newline_pos, newline_offset, last_pos
    INTEGER :: min_index, max_index, indx

    errcode = eis_err_none

    CALL eis_load_file_to_string(filename, str)
    index_range = [min_index, max_index]
    IF (.NOT. ALLOCATED(str)) THEN
      errcode = IOR(errcode, eis_err_no_file)
      RETURN
    END IF

    index_range = this%populate(str, errcode, index_start)
    DEALLOCATE(str)

  END FUNCTION ess_load_from_ascii_file



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Remove blank lines from the list of lines
  !> @param[in] this
  SUBROUTINE ess_remove_blank_lines(this)
    CLASS(eis_string_store), INTENT(INOUT) :: this
    INTEGER :: index
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE :: str
    LOGICAL :: ok, disorder

    index = this%get_size()
    disorder = this%strings%can_disorder
    this%strings%can_disorder = .TRUE.
    DO WHILE (index > 0)
      ok = this%get(index, str)
      IF (str == "") THEN
        ok = this%delete(index)
      END IF
      index = index - 1
    END DO
    this%strings%can_disorder = disorder

  END SUBROUTINE ess_remove_blank_lines



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Combine lines when the last characters of a line
  !> is a specified character sequence
  !> @param[inout] this
  !> @param[in] eol_sequence
  SUBROUTINE ess_combine_split_lines(this, eol_sequence)
    CLASS(eis_string_store), INTENT(INOUT) :: this
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN) :: eol_sequence
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE :: str, str2
    LOGICAL :: ok, loop, disorder
    INTEGER :: eol_length, str_length, index, sindex

    disorder = this%strings%can_disorder
    this%strings%can_disorder = .TRUE.
    loop = .TRUE.
    eol_length = LEN(eol_sequence)
    DO WHILE (loop)
      loop = .FALSE.
      DO index = 1, this%get_size()
        ok = this%get(index, str)
        str_length = LEN(str)
        IF (str_length >= eol_length .AND. index < this%get_size()) THEN
          IF (str(str_length - eol_length+1:str_length) == eol_sequence) THEN
            ok = this%get(index + 1, str2)
            sindex = this%store(str(1:str_length-eol_length) // str2, index)
            ok = this%delete(index + 1)
            loop = .TRUE.
            EXIT
          END IF
        END IF
      END DO
    END DO
    this%strings%can_disorder = disorder

  END SUBROUTINE ess_combine_split_lines

END MODULE eis_string_store_mod
