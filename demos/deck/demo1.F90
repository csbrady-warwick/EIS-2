MODULE mymod

  USE eis_header
  IMPLICIT NONE
  CONTAINS

  SUBROUTINE init_block(block_text, parent_kind, status, host_state, &
      errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Calling init for block : ', block_text
  END SUBROUTINE init_block

  SUBROUTINE final_block(block_text, parent_kind, status, host_state, &
      errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Calling final for block : ', block_text
  END SUBROUTINE final_block

  SUBROUTINE start_block(block_text, parents, parent_kind, status, host_state, &
      errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Calling start for block : ', block_text
  END SUBROUTINE start_block

  SUBROUTINE end_block(block_text, parents, parent_kind, status, host_state, &
      errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Calling end for block : ', block_text
  END SUBROUTINE end_block

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
  block => root%add_block('block1', init_block = init_block, &
      start_block = start_block, end_block = end_block, &
      final_block = final_block)

  block => root%add_block('block2', init_block = init_block, &
      start_block = start_block, end_block = end_block, &
      final_block = final_block)

  CALL deck%init()
  CALL deck%parse_deck_file('test.deck', dfn, errcode, &
      allow_empty_blocks = .TRUE.)
  IF (errcode /= eis_err_none) THEN
    DO ierr = 1, deck%get_error_count()
      CALL deck%get_error_report(ierr, str)
      PRINT *, str
    END DO
  END IF

END PROGRAM testprog
