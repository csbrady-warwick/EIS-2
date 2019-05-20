MODULE eis_string_store_mod

  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE eis_constants
  USE eis_named_store_mod
  USE eis_utils

  IMPLICIT NONE

  TYPE :: string_holder
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE :: text
    CONTAINS
    FINAL :: sh_destructor
  END TYPE string_holder

  TYPE :: eis_string_store
    PRIVATE
    TYPE(named_store) :: strings

    CONTAINS
#ifdef UNICODE
    PROCEDURE :: store_string_ucs4 => ess_store_string_ucs4
    PROCEDURE :: get_string_ucs4 => ess_get_string_ucs4
    PROCEDURE :: store_string_ascii => ess_store_string_ascii
    PROCEDURE :: get_string_ascii => ess_get_string_ascii
    PROCEDURE :: append_string_ucs4 => ess_append_string_ucs4
    PROCEDURE :: append_string_ascii => ess_append_string_ascii
    PROCEDURE :: format_fill_ucs4 => ess_format_fill_ucs4
    PROCEDURE :: format_fill_ascii => ess_format_fill_ascii

    GENERIC, PUBLIC :: store => store_string_ucs4, store_string_ascii
    GENERIC, PUBLIC :: get => get_string_ucs4, get_string_ascii
    GENERIC, PUBLIC :: append => append_string_ucs4, append_string_ascii
    GENERIC, PUBLIC :: format_fill => format_fill_uu, format_fill_aa
#else
    PROCEDURE, PUBLIC :: store => ess_store_string_ascii
    PROCEDURE, PUBLIC :: get => ess_get_string_ascii
    PROCEDURE, PUBLIC :: append => ess_append_string_ascii
    PROCEDURE, PUBLIC :: format_fill => ess_format_fill_aa
#endif
    
  END TYPE eis_string_store

  PRIVATE
  PUBLIC :: eis_string_store

CONTAINS

  SUBROUTINE sh_destructor(this)
    TYPE(string_holder), INTENT(INOUT) :: this
    IF (ALLOCATED(this%text)) DEALLOCATE(this%text)
  END SUBROUTINE sh_destructor


#ifdef UNICODE
  SUBROUTINE ess_store_string_ucs4(this, key, text)
    CLASS(eis_string_store), INTENT(INOUT) :: this
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN) :: key
    CHARACTER(LEN=*, KIND=UCS4), INTENT(IN) :: text
    TYPE(string_holder) :: temp

    ALLOCATE(temp%text, SOURCE = text)
    CALL this%strings%store(key, temp)

  END SUBROUTINE ess_store_string_ucs4
#endif



  SUBROUTINE ess_store_string_ascii(this, key, text)
    CLASS(eis_string_store), INTENT(INOUT) :: this
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN) :: key
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN) :: text
    TYPE(string_holder) :: temp

    ALLOCATE(CHARACTER(LEN=LEN(text), KIND=UCS4)::temp%text)
    temp%text = text
    CALL this%strings%store(key, temp)

  END SUBROUTINE ess_store_string_ascii


#ifdef UNICODE
  FUNCTION ess_get_string_ucs4(this, key, text)
    CLASS(eis_string_store), INTENT(IN) :: this
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN) :: key
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE, INTENT(INOUT) :: text
    LOGICAL :: ess_get_string_ucs4
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



  FUNCTION ess_get_string_ascii(this,key, text)
    CLASS(eis_string_store), INTENT(IN) :: this
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN) :: key
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE, INTENT(INOUT) :: text
    LOGICAL :: ess_get_string_ascii
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




  FUNCTION ess_append_string_ucs4(this, key, text, newline)
    CLASS(eis_string_store), INTENT(IN) :: this
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN) :: key
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE, INTENT(INOUT) :: text
    LOGICAL, INTENT(IN), OPTIONAL :: newline
    LOGICAL :: ess_append_string_ucs4
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE :: retreived

    ess_append_string_ucs4 = this%get(key, retreived)
    IF (.NOT. ess_append_string_ucs4) RETURN

    CALL eis_append_string(text, retreived, newline = newline)

  END FUNCTION ess_append_string_ucs4



  FUNCTION ess_append_string_ascii(this, key, text, newline)
    CLASS(eis_string_store), INTENT(IN) :: this
    CHARACTER(LEN=*, KIND=ASCII), INTENT(IN) :: key
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE, INTENT(INOUT) :: text
    LOGICAL, INTENT(IN), OPTIONAL :: newline
    LOGICAL :: ess_append_string_ascii
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE :: retreived

    ess_append_string_ascii = this%get(key, retreived)
    IF (.NOT. ess_append_string_ascii) RETURN

    CALL eis_append_string(text, retreived, newline = newline)

  END FUNCTION ess_append_string_ascii



  SUBROUTINE ess_format_fill_aa(this, text)
    CLASS(eis_string_store), INTENT(IN) :: this
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE, INTENT(INOUT) :: text
    CHARACTER(LEN=:, KIND=ASCII), ALLOCATABLE :: temp, temp_o
    INTEGER :: istr, rstr
    LOGICAL :: recording_name, ok

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

  END SUBROUTINE ess_format_fill_aa

END MODULE eis_string_store_mod
