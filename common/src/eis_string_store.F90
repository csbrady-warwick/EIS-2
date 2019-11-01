MODULE eis_string_store_mod

  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE eis_constants
  USE eis_header
  USE eis_ordered_store_mod
  USE eis_utils

  IMPLICIT NONE

  CHARACTER(LEN=10), PRIVATE :: serial_header = 'EISDSER001'

  !>@class
  !>Class holding the string within the string store
  !>If compiled with support is UCS4 string
  TYPE :: string_holder
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE :: text
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE :: filename
    INTEGER :: minline = -1, maxline = -1
    INTEGER :: whitespace_len = 0
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
    PROCEDURE :: to_string_ascii => ess_to_string_ascii
    PROCEDURE :: to_string_ucs4 => ess_to_string_ucs4

    GENERIC, PUBLIC :: store => store_string_ucs4, store_string_ascii
    GENERIC, PUBLIC :: get => get_string_ucs4, get_string_ascii
    GENERIC, PUBLIC :: append => append_string_ucs4, append_string_ascii
    GENERIC, PUBLIC :: insert => insert_string_ucs4, insert_string_ascii
    GENERIC, PUBLIC :: populate => populate_from_ascii, populate_from_ucs4
    GENERIC, PUBLIC :: to_string => to_string_ascii, to_string_ucs4
#else
    PROCEDURE, PUBLIC :: store => ess_store_string_ascii
    PROCEDURE, PUBLIC :: get => ess_get_string_ascii
    PROCEDURE, PUBLIC :: append => ess_append_string_ascii
    PROCEDURE, PUBLIC :: insert => ess_insert_string_ascii
    PROCEDURE, PUBLIC :: populate => ess_populate_from_ascii
    PROCEDURE, PUBLIC :: to_string => ess_to_string_ascii
#endif
    PROCEDURE, PUBLIC :: clear => ess_clear
    PROCEDURE, PUBLIC :: delete => ess_delete
    PROCEDURE, PUBLIC :: combine_lines => ess_combine_lines
    PROCEDURE, PUBLIC :: get_size => ess_get_size
    PROCEDURE, PUBLIC :: load_from_ascii_file => ess_load_from_ascii_file
    PROCEDURE, PUBLIC :: remove_blank_lines => ess_remove_blank_lines
    PROCEDURE, PUBLIC :: combine_split_lines => ess_combine_split_lines
    PROCEDURE, PUBLIC :: remove_comments => ess_remove_comments
    PROCEDURE, PUBLIC :: remove_whitespace => ess_remove_whitespace
    PROCEDURE, PUBLIC :: serialise => ess_serialise
    PROCEDURE, PUBLIC :: deserialise => ess_deserialise
    
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
    IF (ALLOCATED(this%filename)) DEALLOCATE(this%filename)
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
  !> Deletes all items from the store
  !> @param[inout] this
  !> @return success
  FUNCTION ess_clear(this) RESULT(success)
    CLASS(eis_string_store), INTENT(INOUT) :: this
    !> Whether or not the deletion succeded
    LOGICAL :: success
    LOGICAL :: disorder

    success = this%strings%clear()

  END FUNCTION ess_clear



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Deletes an item from the store
  !> @param[inout] this
  !> @param[in] delete_index
  !> @return success
  FUNCTION ess_delete(this, delete_index) RESULT(success)
    CLASS(eis_string_store), INTENT(INOUT) :: this
    !> Index of item to delete
    INTEGER, INTENT(IN) :: delete_index
    !> Whether or not the deletion succeded
    LOGICAL :: success
    LOGICAL :: disorder

    disorder = this%strings%can_disorder
    this%strings%can_disorder = .TRUE.
    success = this%strings%delete(delete_index)
    this%strings%can_disorder = disorder

  END FUNCTION ess_delete



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Combines all lines within a specified range into a single line
  !> @param[inout] this
  !> @param[in] line_start
  !> @param[in] line_end
  SUBROUTINE ess_combine_lines(this, line_start, line_end)
    CLASS(eis_string_store), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: line_start, line_end
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE :: str, str2, fname, fname0
    LOGICAL :: ok, disorder, changed_fname
    INTEGER :: istr, sindex, flmin, flmax, ln, lnm

    IF (line_end <= line_start) RETURN
    IF (line_start < 1 .OR. line_end > this%get_size()) RETURN

    disorder = this%strings%can_disorder
    this%strings%can_disorder = .TRUE.

    flmin = HUGE(flmin)
    flmax = -1
    changed_fname = .FALSE.

    DO istr = line_start, line_end
      ok = this%get(istr, str, filename=fname, line_number = ln, &
          line_number_max = lnm)
      IF (.NOT. ALLOCATED(fname0)) THEN
        ALLOCATE(fname0, SOURCE = fname)
      ELSE
        IF (.NOT. changed_fname) THEN
          IF (fname /= fname0) THEN
            DEALLOCATE(fname0)
            ALLOCATE(fname0, SOURCE='{MULTIPLE SOURCES}')
          END IF
        END IF
      END IF
      flmin = MIN(flmin, ln)
      flmax = MAX(flmax, lnm)
      CALL eis_append_string(str2, str, newline=.FALSE.)
    END DO

    sindex = this%store(str2, line_start, filename = fname0, &
        line_number = flmin, line_number_max = flmax)
    DO istr = line_start + 1, line_end
      ok = this%delete(istr)
    END DO

    this%strings%can_disorder = disorder

  END SUBROUTINE ess_combine_lines



#ifdef UNICODE
  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Stores a string to a numbered item. String is UCS4
  !> @param[inout] this
  !> @param[in] text
  !> @param[in] store_index
  !> @param[in] filename
  !> @param[in] line_number
  !> @param[in] line_number_max
  !> @param[in] default_line_info
  !> @return index_out
  FUNCTION ess_store_string_ucs4(this, text, store_index, filename, &
      line_number, line_number_max, default_line_info) RESULT(index_out)
    CLASS(eis_string_store), INTENT(INOUT) :: this
    !> String to store
    CHARACTER(LEN=*, KIND=UCS4), INTENT(IN) :: text
    !> Index to store the string in. Optional, default store at end
    INTEGER, INTENT(IN), OPTIONAL :: store_index
    !> Filename for a file that originally held the data
    !> Optional. Default no filename is stored
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: filename
    !> Line number for the line
    !> Optional, default no line number is stored
    INTEGER, INTENT(IN), OPTIONAL :: line_number
    !> Maximum line number for a line that is originally split over several 
    !> lines
    !> Optional, default no maximum line number is stored
    INTEGER, INTENT(IN), OPTIONAL :: line_number_max
    !> Logical flag. If true then even when overwriting an existing
    !> item the line number and filename metadata will not be recovered
    !> Index of stored string
    !> Optional. Default .FALSE.
    LOGICAL, INTENT(IN), OPTIONAL :: default_line_info
    INTEGER(eis_i4) :: index_out
    TYPE(string_holder) :: temp
    LOGICAL :: ok, recover
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE :: str_old, fname
    INTEGER :: ln, lnm

    ALLOCATE(temp%text, SOURCE = text)
    IF (PRESENT(store_index)) THEN
      recover = .FALSE.
      IF (PRESENT(default_line_info)) recover = .NOT. default_line_info
      IF (recover) THEN
        ok = this%get(store_index, str_old, filename = fname, &
            line_number = ln, line_number_max = lnm)
        IF (ok) THEN
          CALL eis_copy_string(fname, temp%filename)
          IF (ALLOCATED(fname)) DEALLOCATE(fname)
          temp%minline = ln
          temp%maxline = lnm
        END IF
      END IF
    END IF

    IF (PRESENT(filename)) THEN
      ALLOCATE(fname, SOURCE = filename)
      CALL eis_copy_string(fname, temp%filename)
      IF (ALLOCATED(fname)) DEALLOCATE(fname)
    END IF
    IF (PRESENT(line_number)) THEN
      temp%minline = line_number
      temp%maxline = line_number
    END IF
    IF (PRESENT(line_number_max)) temp%maxline = line_number_max
    index_out = this%strings%store(temp, store_index)

  END FUNCTION ess_store_string_ucs4
#endif



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Stores a string to a numbered item. String is ASCII
  !> @param[inout] this
  !> @param[in] text
  !> @param[in] store_index
  !> @param[in] filename
  !> @param[in] line_number
  !> @param[in] line_number_max
  !> @param[in] trimmed_white_space_length
  !> @param[in] default_line_info
  !> @return index_out
  FUNCTION ess_store_string_ascii(this, text, store_index, filename, &
      line_number, line_number_max, trimmed_white_space_length, &
      default_line_info) RESULT(index_out)
    CLASS(eis_string_store), INTENT(INOUT) :: this
    !> String to store
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN) :: text
    !> Index to store the string in. Optional, default store at end
    INTEGER, INTENT(IN), OPTIONAL :: store_index
    !> Filename for a file that originally held the data
    !> Optional. Default no filename is stored
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN), OPTIONAL :: filename
    !> Line number for the line
    !> Optional, default no line number is stored
    INTEGER, INTENT(IN), OPTIONAL :: line_number
    !> Maximum line number for a line that is originally split over several 
    !> lines
    !> Optional, default no maximum line number is stored
    INTEGER, INTENT(IN), OPTIONAL :: line_number_max
    !> Number of white space characters removed from the left of the string
    !> when `remove_whitespace` is called.
    !> Optional, default stores 0
    INTEGER, INTENT(IN), OPTIONAL :: trimmed_white_space_length
    !> Logical flag. If true then even when overwriting an existing
    !> item the line number and filename metadata will not be recovered
    !> Index of stored string
    !> Optional. Default .FALSE.
    LOGICAL, INTENT(IN), OPTIONAL :: default_line_info
    !> Index of stored string
    INTEGER(eis_i4) :: index_out
    TYPE(string_holder) :: temp
    LOGICAL :: ok, recover
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE :: str_old, fname
    INTEGER :: ln, lnm, wsl

    ALLOCATE(temp%text, SOURCE = text)
    IF (PRESENT(store_index)) THEN
      recover = .TRUE.
      IF (PRESENT(default_line_info)) recover = .NOT. default_line_info
      IF (recover) THEN
        ok = this%get(store_index, str_old, filename = fname, &
            line_number = ln, line_number_max = lnm, &
            trimmed_white_space_length = wsl)
        IF (ok) THEN
          CALL eis_copy_string(fname, temp%filename)
          IF (ALLOCATED(fname)) DEALLOCATE(fname)
          temp%minline = ln
          temp%maxline = lnm
          temp%whitespace_len = wsl
        END IF
      END IF
    END IF

    IF (PRESENT(filename)) THEN
      ALLOCATE(fname, SOURCE = filename)
      CALL eis_copy_string(fname, temp%filename)
      IF (ALLOCATED(fname)) DEALLOCATE(fname)
    END IF
    IF (PRESENT(line_number)) THEN
      temp%minline = line_number
      temp%maxline = line_number
    END IF
    IF (PRESENT(line_number_max)) temp%maxline = line_number_max
    IF (PRESENT(trimmed_white_space_length)) temp%whitespace_len = &
        trimmed_white_space_length
    index_out = this%strings%store(temp, store_index)

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
  !> @param[in] get_index
  !> @param[inout] text
  !> @param[out] filename
  !> @param[out] line_number
  !> @param[out] line_number_max
  !> @return ess_get_string_ucs4
  FUNCTION ess_get_string_ucs4(this, get_index, text, filename, line_number, &
      line_number_max)
    CLASS(eis_string_store), INTENT(IN) :: this
    !> Index to retrieve
    INTEGER(eis_i4), INTENT(IN) :: get_index
    !> Variable to hold returned string
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE, INTENT(INOUT) :: text
    !> Filename associated with line when it was created
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE, INTENT(INOUT), OPTIONAL :: &
        filename
    !> Minimum line number in original source associated with line
    INTEGER, INTENT(OUT), OPTIONAL :: line_number
    !> Maximum line number in original source associated with line
    INTEGER, INTENT(OUT), OPTIONAL :: line_number_max
    LOGICAL :: ess_get_string_ucs4 !< Was index found in store
    CLASS(*), POINTER :: gptr
    TYPE(string_holder), POINTER :: temp

    gptr => this%strings%get(get_index)
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
      IF (PRESENT(filename)) CALL eis_copy_string(temp%filename, filename)
      IF (PRESENT(line_number)) line_number = temp%minline
      IF (PRESENT(line_number_max)) line_number_max = temp%maxline
      CALL eis_copy_string(temp%text, text)
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
  !> @param[in] get_index
  !> @param[inout] text
  !> @param[out] filename
  !> @param[out] line_number
  !> @param[out] line_number_max
  !> @param[out] trimmed_white_space_length
  !> @return ess_get_string_ascii
  FUNCTION ess_get_string_ascii(this, get_index, text, filename, line_number, &
      line_number_max, trimmed_white_space_length)
    CLASS(eis_string_store), INTENT(IN) :: this
    !> Key to retrieve
    INTEGER(eis_i4), INTENT(IN) :: get_index
    !> Variable to hold returned string
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE, INTENT(INOUT) :: text
    !> Filename associated with line when it was created
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE, INTENT(INOUT), OPTIONAL :: &
        filename
    !> Minimum line number in original source associated with line
    INTEGER, INTENT(OUT), OPTIONAL :: line_number
    !> Maximum line number in original source associated with line
    INTEGER, INTENT(OUT), OPTIONAL :: line_number_max
    !> Number of removed whitespace characters on left of string
    INTEGER, INTENT(OUT), OPTIONAL :: trimmed_white_space_length
    LOGICAL :: ess_get_string_ascii !< Was index found in store
    CLASS(*), POINTER :: gptr
    TYPE(string_holder), POINTER :: temp

    gptr => this%strings%get(get_index)
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
      IF (PRESENT(filename) .AND. ALLOCATED(temp%filename)) &
          CALL eis_copy_string(temp%filename, filename)
      IF (PRESENT(line_number)) line_number = temp%minline
      IF (PRESENT(line_number_max)) line_number_max = temp%maxline
      IF (PRESENT(trimmed_white_space_length)) trimmed_white_space_length = &
          temp%whitespace_len
      CALL eis_copy_string(temp%text, text)
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
  !> @param[in] get_index
  !> @param[inout] text
  !> @param[in] newline
  FUNCTION ess_append_string_ucs4(this, get_index, text, newline)
    CLASS(eis_string_store), INTENT(IN) :: this
    !> Index to retrieve
    INTEGER(eis_i4), INTENT(IN) :: get_index
    !> Text variable to append with output
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE, INTENT(INOUT) :: text
    !> Logical flag to combine strings with a newline character between them.
    !> Optional, default .TRUE. (put newline between strings)
    LOGICAL, INTENT(IN), OPTIONAL :: newline
    LOGICAL :: ess_append_string_ucs4
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE :: retreived

    ess_append_string_ucs4 = this%get(get_index, retreived)
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
  !> @param[in] get_index
  !> @param[inout] text
  !> @param[in] newline
  FUNCTION ess_append_string_ascii(this, get_index, text, newline)
    CLASS(eis_string_store), INTENT(IN) :: this
    INTEGER(eis_i4), INTENT(IN) :: get_index
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE, INTENT(INOUT) :: text
    LOGICAL, INTENT(IN), OPTIONAL :: newline
    LOGICAL :: ess_append_string_ascii
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE :: retreived

    ess_append_string_ascii = this%get(get_index, retreived)
    IF (.NOT. ess_append_string_ascii) RETURN

    CALL eis_append_string(text, retreived, newline = newline)

  END FUNCTION ess_append_string_ascii




#ifdef UNICODE
  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Stores a string to a numbered item. String is UCS4
  !> @param[inout] this
  !> @param[in] dest_index
  !> @param[in] text
  !> @param[in] filename
  !> @param[in] line_number
  !> @param[in] line_number_max
  !> @return index
  FUNCTION ess_insert_string_ucs4(this, dest_index, text, filename, &
      line_number, line_number_max) RESULT(in_index)

    CLASS(eis_string_store), INTENT(INOUT) :: this
    !> Index at which to insert text
    INTEGER, INTENT(IN) :: dest_index
    !> String to store
    CHARACTER(LEN=*, KIND=UCS4), INTENT(IN) :: text
    !> Filename associated with line when it was created
    CHARACTER(LEN=:, KIND=UCS4), INTENT(IN), OPTIONAL :: filename
    !> Minimum line number in original source associated with line
    INTEGER, INTENT(IN), OPTIONAL :: line_number
    !> Maximum line number in original source associated with line
    INTEGER, INTENT(IN), OPTIONAL :: line_number_max
    LOGICAL :: ess_get_string_ascii !< Was index found in store
    !> Index of stored string
    INTEGER(eis_i4) :: in_index
    TYPE(string_holder) :: temp
    LOGICAL :: disorder

    ALLOCATE(temp%text, SOURCE = text)
    IF (PRESENT(filename)) ALLOCATE(temp%filename, SOURCE = filename)
    IF (PRESENT(line_number)) THEN
      temp%minline = line_number
      temp%maxline = line_number
    END IF
    IF (PRESENT(line_number_max)) temp%maxline = line_number_max
    disorder = this%strings%can_disorder
    this%strings%can_disorder = .TRUE.
    in_index = this%strings%insert(temp, dest_index)
    this%strings%can_disorder = disorder

  END FUNCTION ess_insert_string_ucs4
#endif


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Stores a string to a numbered item. String is ASCII
  !> @param[inout] this
  !> @param[in] text
  !> @return in_index
  FUNCTION ess_insert_string_ascii(this, dest_index, text, filename, &
      line_number, line_number_max) RESULT(in_index)
    CLASS(eis_string_store), INTENT(INOUT) :: this
    !> Index at which to insert text
    INTEGER, INTENT(IN) :: dest_index
    !> String to store
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN) :: text
    !> Filename associated with line when it was created
    CHARACTER(LEN=*, KIND=UCS4), INTENT(IN), OPTIONAL :: filename
    !> Minimum line number in original source associated with line
    INTEGER, INTENT(IN), OPTIONAL :: line_number
    !> Maximum line number in original source associated with line
    INTEGER, INTENT(IN), OPTIONAL :: line_number_max
    !> Index of stored string
    INTEGER(eis_i4) :: in_index
    TYPE(string_holder) :: temp
    LOGICAL :: disorder

    ALLOCATE(temp%text, SOURCE = text)
    IF (PRESENT(filename)) ALLOCATE(temp%filename, SOURCE = filename)
    IF (PRESENT(line_number)) THEN
      temp%minline = line_number
      temp%maxline = line_number
    END IF
    IF (PRESENT(line_number_max)) temp%maxline = line_number_max
    disorder = this%strings%can_disorder
    this%strings%can_disorder = .TRUE.
    in_index = this%strings%insert(temp, dest_index)
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
  !> @param[in] index_start
  !> @param[in] slc_start
  !> @param[in] mlc_start
  !> @param[in] mlc_end
  !> @result index_range
  FUNCTION ess_populate_from_ascii(this, str, errcode, index_start, filename) &
      RESULT(index_range)
    CLASS(eis_string_store), INTENT(INOUT) :: this
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN) :: str
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER, INTENT(IN), OPTIONAL :: index_start
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN), OPTIONAL :: filename
    INTEGER, DIMENSION(2) :: index_range
    INTEGER :: newline_pos, newline_offset, last_pos
    INTEGER :: min_index, max_index, indx, line
    CHARACTER(LEN=:), ALLOCATABLE :: str_compose

    errcode = eis_err_none

    IF (PRESENT(index_start)) indx = index_start

    newline_pos = INDEX(str, NEW_LINE(str))
    newline_offset = newline_pos
    last_pos = 1
    min_index = HUGE(min_index)
    max_index = 0
    line = 0
    DO WHILE(newline_offset > 0)
      line = line + 1
      IF (last_pos /= newline_pos) THEN
        IF (PRESENT(index_start)) THEN
          indx = this%insert(indx, str(last_pos:newline_pos - 1), &
              line_number = line, filename = filename)
          indx = indx + 1
        ELSE
          indx = this%store(str(last_pos:newline_pos - 1), line_number = line, &
              filename = filename)
        END IF
      ELSE
        IF (PRESENT(index_start)) THEN
          indx = this%insert(indx, "", filename = filename)
          indx = indx + 1
        ELSE
          indx = this%store("", filename = filename)
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
  !> Routine to convert the stored strings back to a single newline separated
  !> string
  !> @param[in] this
  !> @param[inout] str_out
  !> @param[inout] errcode
  SUBROUTINE ess_to_string_ascii(this, str_out, errcode)
    CLASS(eis_string_store), INTENT(INOUT) :: this
    !> String to hold the contents
    CHARACTER(LEN=:, KIND=ASCII), INTENT(INOUT), ALLOCATABLE :: str_out
    !> Error code
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER :: istr
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE :: str
    LOGICAL :: ok

    errcode = eis_err_none
    IF (ALLOCATED(str_out)) DEALLOCATE(str_out)
    IF (this%get_size() < 1) RETURN
    DO istr = 1, this%get_size()
      ok = this%get(istr, str)
      CALL eis_append_string(str_out, str, newline = .TRUE.)
    END DO

  END SUBROUTINE ess_to_string_ascii



#ifdef UNICODE
  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Routine to convert the stored strings back to a single newline separated
  !> string
  !> @param[in] this
  !> @param[inout] str_out
  !> @param[inout] errcode
  SUBROUTINE ess_to_string_ucs4(this, str_out, errcode)
    CLASS(eis_string_store), INTENT(INOUT) :: this
    !> String to hold the contents
    CHARACTER(LEN=:, KIND=UCS4), INTENT(INOUT), ALLOCATABLE :: str_out
    !> Error code
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER :: istr
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE :: str
    LOGICAL :: ok

    errcode = eis_err_none
    IF (ALLOCATED(str_out)) DEALLOCATE(str_out)
    IF (this%get_size() < 1) RETURN
    DO istr = 1, this%get_size()
      ok = this%get(istr, str)
      CALL eis_append_string(str_out, str, newline = .TRUE.)
    END DO

  END SUBROUTINE ess_to_string_ucs4
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
  !> @param[in] index_start
  !> @param[in] slc_start
  !> @param[in] mlc_start
  !> @param[in] mlc_end
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
    index_range = -1
    IF (.NOT. ALLOCATED(str)) THEN
      errcode = IOR(errcode, eis_err_no_file)
      RETURN
    END IF

    index_range = this%populate(str, errcode, index_start, filename = filename)
    DEALLOCATE(str)

  END FUNCTION ess_load_from_ascii_file



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Remove blank lines from the list of lines
  !> @param[in] this
  SUBROUTINE ess_remove_blank_lines(this)
    CLASS(eis_string_store), INTENT(INOUT) :: this
    INTEGER :: r_index
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE :: str
    LOGICAL :: ok, disorder

    r_index = this%get_size()
    disorder = this%strings%can_disorder
    this%strings%can_disorder = .TRUE.
    DO WHILE (r_index > 0)
      ok = this%get(r_index, str)
      IF (str == "") THEN
        ok = this%delete(r_index)
      END IF
      r_index = r_index - 1
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
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE :: str, str2, fname
    LOGICAL :: ok, loop, disorder
    INTEGER :: eol_length, str_length, c_index, sindex, con_start

    disorder = this%strings%can_disorder
    this%strings%can_disorder = .TRUE.
    loop = .TRUE.
    eol_length = LEN(eol_sequence)
    DO WHILE (loop)
      loop = .FALSE.
      con_start = -1
      DO c_index = 1, this%get_size()
        ok = this%get(c_index, str)
        str_length = LEN(str)
        IF (str_length > eol_length) THEN
          IF (str(str_length - eol_length+1:str_length) == eol_sequence) THEN
            IF (con_start < 0) con_start = c_index
            sindex = this%store(str(1:str_length - eol_length), c_index)
            CYCLE
          END IF
        END IF
        IF (con_start > 0) THEN
          CALL this%combine_lines(con_start, c_index)
          loop = .TRUE.
          EXIT
        END IF
      END DO
    END DO
    this%strings%can_disorder = disorder

  END SUBROUTINE ess_combine_split_lines



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Removes text that appears on a line after a comment character
  !> Lines that have only a comment on them become blank lines
  !> @param[inout] this
  !> @param[in] slc_start
  !> @param[in] mlc_start
  !> @param[in] mlc_end
  SUBROUTINE ess_remove_comments(this, slc_start, mlc_start, mlc_end)
    CLASS(eis_string_store), INTENT(INOUT) :: this
    !> Character sequence to start a single line comment
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN), OPTIONAL :: slc_start
    !> Character sequence to start a multi line comment
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN), OPTIONAL :: mlc_start
    !> Character sequence to end a multi line comment
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN), OPTIONAL :: mlc_end
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE :: str, str_temp
    INTEGER :: istr, ipos, sindex, ipos2
    LOGICAL :: ok, disorder, in_comment

    disorder = this%strings%can_disorder
    this%strings%can_disorder = .TRUE.
    !Do single line comments
    IF (PRESENT(slc_start)) THEN
      istr = 1
      DO
        IF (istr > this%get_size()) EXIT
        ok = this%get(istr, str)
        ipos = INDEX(str, slc_start)
        IF (ipos > 0) THEN
          IF (INDEX(TRIM(ADJUSTL(str)), slc_start) /= 1) THEN
            sindex = this%store(str(1:ipos-1), istr)
          ELSE
            ok = this%delete(istr)
            istr = istr - 1
          END IF
        END IF
      istr = istr + 1
      END DO
    END IF

    !Do multi line comments
    IF (PRESENT(mlc_start) .AND. PRESENT(mlc_end)) THEN
      istr = 1
      in_comment = .FALSE.
      DO
        IF (istr > this%get_size()) EXIT
        ok = this%get(istr, str)
        IF (.NOT. in_comment) THEN
          !If not already inside an mlc comment check for the start sequence
          ipos = INDEX(str, mlc_start)
          IF (ipos > 0) THEN
            IF (ipos + LEN(mlc_start) < LEN(str)) THEN
              ipos2 = INDEX(str(ipos + LEN(mlc_start):), mlc_end)
            ELSE
              ipos2 = 0
            END IF
            !If we didn't find the end comment section on the same line
            !then we're in a comment field
            in_comment = (ipos2 == 0)
            ipos2 = ipos2 + ipos + LEN(mlc_start) - 1
            IF (INDEX(TRIM(ADJUSTL(str)), mlc_start) /= 1) THEN
              !Is the mlc start not at the start of the line?
              IF (.NOT. in_comment .AND. ipos2 < LEN(str)) THEN
                !If the mlc section is hanging in the middle of a line then
                !chop it out.
                sindex = this%store(str(1:ipos-1) &
                    // str(ipos2 + LEN(mlc_end):), istr)
              ELSE
                !If either the mlc end sequence isn't on this line
                !or is right at the end of this line then just
                !delete everything after the start of the mlc start
                !sequence
                sindex = this%store(str(1:ipos-1), istr)
              END IF
            ELSE
              !The mlc start sequence is at the start of a line
              IF (.NOT. in_comment .AND. ipos2 < LEN(str)) THEN
                !If the mlc end sequence is on the same line and 
                !not at the end of the line then trim out all of the
                !text between the start and end markers inclusively
                sindex = this%store(str(ipos2 + LEN(mlc_end):), istr)
              ELSE
                !In this case the line is all mlc comment, so delete it
                ok = this%delete(istr)
                istr = istr - 1
              END IF
            END IF
          END IF
        ELSE
          !If we are in an mlc section then check for the mlc end sequence
          ipos = INDEX(str, mlc_end)
          IF (ipos > 0) THEN
            !Found the mlc end sequence
            in_comment = .FALSE.
            ok = this%get(istr - 1, str_temp)
            IF (ipos + LEN(mlc_end) < LEN(str)) THEN
              sindex = this%store(str_temp // str(ipos + LEN(mlc_end):), istr-1)
            END IF
            ok = this%delete(istr)
            istr = istr - 1
          ELSE
            ok = this%delete(istr)
            istr = istr - 1
          END IF
        END IF
        istr = istr + 1
      END DO
    END IF
    this%strings%can_disorder = disorder

    IF (ALLOCATED(str)) DEALLOCATE(str)
    IF (ALLOCATED(str_temp)) DEALLOCATE(str_temp)

  END SUBROUTINE ess_remove_comments



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Remove whitespace from start and end of lines
  !> @param[in] this
  SUBROUTINE ess_remove_whitespace(this)
    CLASS(eis_string_store), INTENT(INOUT) :: this
    INTEGER :: istr, sindex, line_number, line_number_max
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE :: str
    LOGICAL :: ok, disorder

    disorder = this%strings%can_disorder
    this%strings%can_disorder = .TRUE.
    DO istr = 1, this%get_size()
      ok = this%get(istr, str)
      sindex = this%store(TRIM(ADJUSTL(str)), istr, &
          trimmed_white_space_length = LEN(TRIM(str)) &
          - LEN(TRIM(ADJUSTL(str))))
    END DO
    this%strings%can_disorder = disorder

  END SUBROUTINE ess_remove_whitespace



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Convert stored strings to a portable representation
  !> that can recreate an entire string representation
  !> @param[in] this
  !> @param[out] str
  SUBROUTINE ess_serialise(this, str)
    CLASS(eis_string_store), INTENT(INOUT) :: this
    !> String holding the portable representation
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE, INTENT(OUT) :: str
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE :: str_rec
    CHARACTER(LEN=:, KIND=UCS4 ), ALLOCATABLE :: filename
    INTEGER :: ln, lnm, istr
    CHARACTER(LEN=20, KIND=ASCII) :: ln_str
    LOGICAL :: ok

    IF (ALLOCATED(str)) DEALLOCATE(str)
    CALL eis_append_string(str,serial_header,newline=.FALSE.)
    DO istr = 1, this%get_size()
      ok = this%get(istr, str_rec, line_number = ln, line_number_max = lnm, &
          filename = filename)
      CALL eis_append_string(str, str_rec // ACHAR(0), newline = .FALSE.)
      IF (ALLOCATED(filename)) THEN
        CALL eis_append_string(str, filename // ACHAR(0), newline = .FALSE.)
      ELSE
        CALL eis_append_string(str, ACHAR(0), newline = .FALSE.)
      END IF
      WRITE(ln_str,'(I20)') ln
      CALL eis_append_string(str, ln_str // ACHAR(0), newline = .FALSE.)
      WRITE(ln_str,'(I20)') lnm
      CALL eis_append_string(str, ln_str // ACHAR(0), newline = .FALSE.)
    END DO

  END SUBROUTINE ess_serialise



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Convert strings in the portable representation
  !> back into strings stored in this store
  !> @param[in] this
  !> @param[out] str
  !> @param[out] errcode
  !> @param[in] store_index
  !> @result index_range
  FUNCTION ess_deserialise(this, str, errcode, store_index) RESULT(index_range)
    CLASS(eis_string_store), INTENT(INOUT) :: this
    !> String holding the portable representation
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN) :: str
    !> Error code from the operation
    INTEGER(eis_error), INTENT(OUT) :: errcode
    !> Index to start unpacking the strings at
    !> Optional, default end of existing strings
    INTEGER, INTENT(IN), OPTIONAL :: store_index
    !> Range of indices to which the strings have been stored
    INTEGER, DIMENSION(2) :: index_range
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE :: txt, filename
    INTEGER :: ln, lnm, lastpos, tpos, fpos, lpos, lmpos
    INTEGER :: is, sindex, imn, imx, ind

    errcode = eis_err_none
    IF (str(1:LEN(serial_header)) /= serial_header) THEN
      errcode = eis_err_malformed_file
      RETURN
    END IF

    lastpos = LEN(serial_header)
    IF (PRESENT(store_index)) sindex = store_index
    imn = HUGE(imn)
    imx = -1
    DO
      IF (lastpos == LEN(str)) EXIT
      tpos  = INDEX(str(lastpos+1:), ACHAR(0)) + lastpos
      fpos  = INDEX(str(tpos+1:), ACHAR(0)) + tpos
      lpos  = INDEX(str(fpos+1:), ACHAR(0)) + fpos
      lmpos = INDEX(str(lpos+1:), ACHAR(0)) + lpos
      READ(str(fpos+1:lpos-1 ),'(I20)') ln
      READ(str(lpos+1:lmpos-1),'(I20)') lnm
      IF (PRESENT(store_index)) THEN
        ind = this%store(str(lastpos+1:tpos-1), sindex, line_number = ln, &
            line_number_max = lnm, filename = str(tpos+1:fpos-1))
        imn = MIN(ind, imn)
        imx = MAX(ind, imx)
        sindex = sindex + 1
      ELSE
        ind = this%store(str(lastpos+1:tpos-1), line_number = ln, &
            line_number_max = lnm, filename = str(tpos+1:fpos-1))
        imn = MIN(ind, imn)
        imx = MAX(ind, imx)
      END IF
      lastpos = lmpos
    END DO
    index_range = [imn, imx]

  END FUNCTION ess_deserialise

END MODULE eis_string_store_mod
