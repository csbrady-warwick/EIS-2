MODULE eis_string_store_mod

  USE eis_named_store_mod
  USE, INTRINSIC :: ISO_FORTRAN_ENV

  IMPLICIT NONE

#ifndef NO_UNICODE
  INTEGER, PARAMETER :: UCS4 = SELECTED_CHAR_KIND('ISO_10646')
#else
  INTEGER, PARAMETER :: UCS4 = SELECTED_CHAR_KIND('ASCII')
#endif
  INTEGER, PARAMETER :: ASCII = SELECTED_CHAR_KIND('ASCII')

  TYPE :: string_holder
    CHARACTER(LEN=:, KIND=UCS4), ALLOCATABLE :: text
    CONTAINS
    FINAL :: sh_destructor
  END TYPE string_holder

  TYPE :: eis_string_store
    PRIVATE
    TYPE(named_store) :: strings

    CONTAINS
#ifndef NO_UNICODE
    PROCEDURE :: store_string_ucs4 => ess_store_string_ucs4
    PROCEDURE :: get_string_ucs4 => ess_get_string_ucs4
    PROCEDURE :: store_string_ascii => ess_store_string_ascii
    PROCEDURE :: get_string_ascii => ess_get_string_ascii
    GENERIC, PUBLIC :: store_string => store_string_ucs4, store_string_ascii
    GENERIC, PUBLIC :: get_string => get_string_ucs4, get_string_ascii
#else
    PROCEDURE :: store_string => ess_store_string_ascii
    PROCEDURE :: get_string => ess_get_string_ascii
#endif
    
  END TYPE eis_string_store

  PRIVATE
  PUBLIC :: eis_string_store

CONTAINS

  SUBROUTINE sh_destructor(this)
    TYPE(string_holder), INTENT(INOUT) :: this
    IF (ALLOCATED(this%text)) DEALLOCATE(this%text)
  END SUBROUTINE sh_destructor


#ifndef NO_UNICODE
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


#ifndef NO_UNICODE
  FUNCTION ess_get_string_ucs4(this, key, text)
    CLASS(eis_string_store), INTENT(INOUT) :: this
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
    CLASS(eis_string_store), INTENT(INOUT) :: this
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

END MODULE eis_string_store_mod
