MODULE mymod

  USE eis_header
  USE eis_parser_mod
  USE eis_deck_header
  IMPLICIT NONE
  CONTAINS


  SUBROUTINE key_str_sub(key_text, key_value, &
      parents, parent_kind, status_code, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: key_text
    CHARACTER(LEN=*), INTENT(IN) :: key_value
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER :: lq, uq

    !If no quotes are present then this is not a bad value
    lq = INDEX(key_value, '"')
    IF (lq == 0) THEN
      status_code = eis_status_not_handled
      RETURN
    END IF

    uq = INDEX(key_value, '"', .TRUE.)
    !Otherwise check that there are two quotes and that there is at least
    !a single character between them. If not then this is a bad value
    IF (uq - lq < 2) THEN
      errcode = eis_err_bad_value
      RETURN
    END IF

    PRINT *,'Found text key ', TRIM(key_text), '. Value is ', &
        key_value(lq+1:uq-1)

  END SUBROUTINE key_str_sub


  SUBROUTINE key_val_sub(key_text, values, parser, &
      parents, parent_kind, status_code, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: key_text
    REAL(eis_num), DIMENSION(:), INTENT(IN) :: values
    TYPE(eis_parser), INTENT(INOUT) :: parser
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Found numerical key ', TRIM(key_text), '. Values are ', values

  END SUBROUTINE key_val_sub

END MODULE mymod


PROGRAM testprog

  USE eis_deck_definition_mod
  USE eis_deck_from_text_mod
  USE eis_deck_header
  USE mymod
  IMPLICIT NONE

  TYPE(eis_text_deck_parser) :: deck
  TYPE(eis_deck_definition) :: dfn
  INTEGER(eis_error) :: errcode
  TYPE(eis_deck_block_definition), POINTER :: root, block
  CHARACTER(LEN=:), ALLOCATABLE :: str
  INTEGER :: ierr

  errcode = eis_err_none
  root => dfn%init()
  block => root%add_block('block1', any_key_value = key_str_sub, &
      any_key_numeric_value = key_val_sub)

  block => root%add_block('block2')

  CALL block%add_key('new_key', key_value_fn = key_str_sub, &
      key_numeric_value_fn = key_val_sub)

  CALL deck%init()
  CALL deck%parse_deck_file('test.deck', dfn, errcode, &
      allow_empty_blocks = .TRUE.)
  IF (errcode /= eis_err_none) THEN
    DO ierr = 1, deck%get_error_count()
      CALL deck%get_error_report(ierr, str)
      PRINT *, str
    END DO
    DEALLOCATE(str)
  END IF

END PROGRAM testprog
