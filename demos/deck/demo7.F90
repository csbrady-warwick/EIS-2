MODULE mymod

  USE eis_deck_header
  IMPLICIT NONE
  SAVE
  TYPE(eis_deck_definition) :: dfn
  TYPE(eis_text_deck_parser) :: deck
  CONTAINS

  SUBROUTINE remapper(block_text, pass_number, remap_text, status_code, &
      host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    CHARACTER(LEN=*), INTENT(INOUT) :: remap_text
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    IF (INDEX(block_text, "block") /= 0) THEN
      remap_text = "generic_block"
    ELSE
      status_code = eis_status_not_handled
    END IF
  END SUBROUTINE remapper

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

  SUBROUTINE key_sub(key_text, value_text, pass_number, parents, &
      parent_kind, status_code, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: key_text
    CHARACTER(LEN=*), INTENT(IN) :: value_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    CHARACTER(LEN=:), ALLOCATABLE :: name

    PRINT *, 'Found key ', TRIM(key_text), ' with value ', TRIM(value_text)
    CALL dfn%get_block_name(parent_kind(SIZE(parent_kind)), name)
    PRINT *, 'Block generic name for key is ', name
    CALL deck%get_block_name(parents(SIZE(parents)), name)
    PRINT *, 'Block specific name for key is ', name

  END SUBROUTINE key_sub

END MODULE mymod


PROGRAM testprog

  USE mymod
  IMPLICIT NONE

  INTEGER(eis_error) :: errcode
  TYPE(eis_deck_block_definition), POINTER :: root, block
  CHARACTER(LEN=:), ALLOCATABLE :: str
  INTEGER :: ierr

  PRINT *,'This example demonstrates block remapping. Block remapping allows &
      &an EIS host code to tell EIS to look up a block under a name other than &
      &the name that actually appears in the deck. In this case it involves &
      &remapping any block containing the word "block" to the block &
      &"generic_block"'

  errcode = eis_err_none
  root => dfn%init(block_remapper = remapper)
  block => root%add_block('generic_block', init_block = init_block, &
      start_block = start_block, end_block = end_block, &
      final_block = final_block, any_key_value = key_sub)

  CALL deck%init()
  CALL deck%parse_deck_file('demo7.deck', dfn, errcode, &
      allow_empty_blocks = .TRUE.)
  IF (errcode /= eis_err_none) THEN
    DO ierr = 1, deck%get_error_count()
      CALL deck%get_error_report(ierr, str)
      PRINT *, str
    END DO
    DEALLOCATE(str)
  END IF

END PROGRAM testprog
