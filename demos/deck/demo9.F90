MODULE mymod

  USE eis_deck_header
  IMPLICIT NONE
  SAVE

  INTEGER :: block1_id, block2_id
  TYPE :: block1
    CHARACTER(LEN=:), ALLOCATABLE :: key1, key2
    INTEGER :: block_uid
    TYPE(block1), POINTER :: next => NULL()
  END TYPE block1

  TYPE :: block2
    CHARACTER(LEN=:), ALLOCATABLE :: new_key
    LOGICAL :: is_child = .FALSE.
    INTEGER :: block_uid
    TYPE(block2), POINTER :: next => NULL()
  END TYPE block2

  TYPE(block1), POINTER :: b1head => NULL(), b1tail => NULL()
  TYPE(block2), POINTER :: b2head => NULL(), b2tail => NULL()

  CONTAINS

  SUBROUTINE start_block1(block_text, pass_number, parents, parent_kind, &
      status, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    IF (ASSOCIATED(b1tail)) THEN
      ALLOCATE(b1tail%next)
      b1tail => b1tail%next
    ELSE
      ALLOCATE(b1head)
      b1tail => b1head
    END IF
    b1tail%block_uid = parents(SIZE(parents))

  END SUBROUTINE start_block1

  SUBROUTINE start_block2(block_text, pass_number, parents, parent_kind, &
      status, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    IF (ASSOCIATED(b2tail)) THEN
      ALLOCATE(b2tail%next)
      b2tail => b2tail%next
    ELSE
      ALLOCATE(b2head)
      b2tail => b2head
    END IF
    b2tail%block_uid = parents(SIZE(parents))
    IF (SIZE(parents) > 2) b2tail%is_child = .TRUE.

  END SUBROUTINE start_block2

  SUBROUTINE key1_sub(key_text, value_text, pass_number, parents, parent_kind, &
      status_code, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: key_text, value_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    ALLOCATE(b1tail%key1, SOURCE = value_text)

  END SUBROUTINE key1_sub

  SUBROUTINE key2_sub(key_text, value_text, pass_number, parents, parent_kind, &
      status_code, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: key_text, value_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    ALLOCATE(b1tail%key2, SOURCE = value_text)

  END SUBROUTINE key2_sub

  SUBROUTINE new_key_sub(key_text, value_text, pass_number, parents, &
      parent_kind, status_code, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: key_text, value_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    ALLOCATE(b2tail%new_key, SOURCE = value_text)

  END SUBROUTINE new_key_sub

  SUBROUTINE on_key_ok(key_text, pass_number, parents, parent_kind, &
      status_code, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: key_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'|'//REPEAT('-', SIZE(parents)+1)//'Key "' // key_text // &
        '" processed OK'

  END SUBROUTINE on_key_ok

  SUBROUTINE on_key_fail(key_text, pass_number, parents, parent_kind, &
      status_code, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: key_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'|'//REPEAT('-', SIZE(parents)+1)//'Key "' // key_text // &
        '" failed processing'
    errcode = eis_err_none

  END SUBROUTINE on_key_fail

  SUBROUTINE on_block_start(block_text, pass_number, parents, parent_kind, &
      status_code, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    IF (block_text == "{ROOT}") RETURN

    IF (parents(SIZE(parents)) /= 1) PRINT *,'|'
    PRINT *,'|'//REPEAT('-', SIZE(parents))//'Started block :', block_text

  END SUBROUTINE on_block_start

  SUBROUTINE on_block_end(block_text, pass_number, parents, parent_kind, &
      status_code, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    IF (block_text == "{ROOT}") RETURN

    PRINT *,'|'//REPEAT('-', SIZE(parents))//'Ended block :', block_text

  END SUBROUTINE on_block_end

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
  TYPE(block1), POINTER :: p1
  TYPE(block2), POINTER :: p2
  CHARACTER(LEN=:), ALLOCATABLE :: dot

  PRINT *,'This example shows how to use the visualisation routines in EIS. &
      &It outputs two files "fort.10" contains a dot file showing &
      &the structure of the actual deck that was being parsed. Blocks that &
      &appear multiple times are shown multiple times. "fort.20" contains a &
      &dot file describing the deck definition. It is not related to the &
      &deck file that was actually read and only shows the blocks and keys &
      &that could be in the file. You will need tools such as Graphviz to &
      &convert dot files to viewable picture files'

  errcode = eis_err_none
  root => dfn%init(on_key_success = on_key_ok, on_key_failure = on_key_fail, &
      on_block_start = on_block_start, on_block_end = on_block_end)

  block => root%add_block('block1', start_block = start_block1)
  CALL block%add_key('key1', key_value_fn = key1_sub)
  CALL block%add_key('key2', key_value_fn = key2_sub)

  block => block%add_block('block2', start_block = start_block2)
  CALL block%add_key('new_key', key_value_fn = new_key_sub)

  block => root%add_block('block2', start_block = start_block2)
  CALL block%add_key('new_key', key_value_fn = new_key_sub)

  CALL deck%init()
  CALL deck%parse_deck_file('demo9.deck', dfn, errcode, &
      allow_empty_blocks = .TRUE.)
  IF (errcode /= eis_err_none) THEN
    DO ierr = 1, deck%get_error_count()
      CALL deck%get_error_report(ierr, str)
      PRINT *, str
    END DO
    STOP
  END IF
  CALL deck%get_block_structure(dot, include_keys = .TRUE.)
  WRITE(10,*) dot
  DEALLOCATE(dot)

  CALL dfn%visualise(dot)
  WRITE(20,*) dot
  DEALLOCATE(dot)

  PRINT *, REPEAT('*',80)

  p1 => b1head
  DO WHILE(ASSOCIATED(p1))
    PRINT *,'Unique block ID is ', p1%block_uid
    IF (ALLOCATED(p1%key1)) PRINT *, 'Key1 is ', p1%key1
    IF (ALLOCATED(p1%key2)) PRINT *, 'Key2 is ', p1%key2
    PRINT *, ""
    p1 => p1%next
  END DO

  p2 => b2head
  DO WHILE(ASSOCIATED(p2))
    PRINT *,'Unique block ID is ', p2%block_uid
    IF (p2%is_child) PRINT *,"Block is a child block"
    IF (ALLOCATED(p2%new_key)) PRINT *, 'New key is ', p2%new_key
    PRINT *, ""
    p2 => p2%next
  END DO

END PROGRAM testprog
