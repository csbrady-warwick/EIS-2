MODULE mymod

  USE eis_deck_header
  IMPLICIT NONE
  CONTAINS

  SUBROUTINE init_block(block_text, pass_number, parent_kind, status, &
      host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Calling init for block : ', block_text
  END SUBROUTINE init_block

  SUBROUTINE final_block(block_text, pass_number, parent_kind, status, &
      host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Calling final for block : ', block_text
  END SUBROUTINE final_block

  SUBROUTINE start_block(block_text, pass_number, parents, parent_kind, &
      status, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Calling start for block : ', block_text
  END SUBROUTINE start_block

  SUBROUTINE end_block(block_text, pass_number, parents, parent_kind, status, &
      host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Calling end for block : ', block_text
  END SUBROUTINE end_block

  SUBROUTINE key_sub(key_text, value_text, pass_number, parents, parent_kind, &
      status_code, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: key_text, value_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,' Found in block : ', parents
    PRINT *,' Found in block of type : ', parent_kind
    PRINT *,' Found known key : ', key_text, ' with value ', value_text
  END SUBROUTINE key_sub

END MODULE mymod


PROGRAM testprog

  USE mymod
  IMPLICIT NONE

  TYPE(eis_text_deck_parser) :: deck
  TYPE(eis_deck_definition) :: dfn
  INTEGER(eis_error) :: errcode
  TYPE(eis_deck_block_definition), POINTER :: root, block
  CHARACTER(LEN=:), ALLOCATABLE :: str
  INTEGER :: ierr

  PRINT *, 'This extends demo1 by adding keys to the blocks. "block1" now has &
      &keys "key1" and "key2" and "block2" has the key "new_key". When any or &
      &all of these keys are encountered they are printed. If any other keys &
      &are encountered an error occurs'

  errcode = eis_err_none
  root => dfn%init()
  block => root%add_block('block1', init_block = init_block, &
      start_block = start_block, end_block = end_block, &
      final_block = final_block)

  CALL block%add_key('key1', key_value_fn = key_sub)
  CALL block%add_key('key2', key_value_fn = key_sub)

  block => root%add_block('block2', init_block = init_block, &
      start_block = start_block, end_block = end_block, &
      final_block = final_block)

  CALL block%add_key('new_key', key_value_fn = key_sub)

  CALL deck%init()
  CALL deck%parse_deck_file('demo2.deck', dfn, errcode, &
      allow_empty_blocks = .TRUE.)
  IF (errcode /= eis_err_none) THEN
    DO ierr = 1, deck%get_error_count()
      CALL deck%get_error_report(ierr, str)
      PRINT *, str
    END DO
  END IF

END PROGRAM testprog
