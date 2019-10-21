MODULE mymod

  USE eis_deck_header
  IMPLICIT NONE
  SAVE

  !Must be a POINTER variable in F2003
  !Can be TARGET in F2008
  !MUST NOT be a plain variable
  INTEGER, POINTER :: scalar
  REAL, DIMENSION(:), POINTER :: array

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

  PRINT *, 'This shows using assignment variables in the deck. You can point &
      &a deck key to a specific pointer variable (or TARGET in F2008 mode) &
      &and that variable will be automatically filled when the key is &
      &encountered. Multiple keys will overwrite previous values without &
      &warning unless other features of EIS are used to prevent this. &
      &It is possible to store REAL32, REAL64, INT32 and INT64 values like &
      &this. It is also possible to store rank 0 or rank 1 values. Parser &
      &expressions for storing to rank 0 variables must return a single value. &
      &Expressions for storing to rank 1 arrays must return <= the size of the &
      &array.'

  ALLOCATE(scalar)
  ALLOCATE(array(4))

  errcode = eis_err_none
  root => dfn%init()
  block => root%add_block('block1')
  CALL block%add_key('scalar_value', i32value = scalar)
  CALL block%add_key('array_value', r32array = array)

  CALL deck%init()
  CALL deck%parse_deck_file('demo10.deck', dfn, errcode)
  IF (errcode /= eis_err_none) THEN
    DO ierr = 1, deck%get_error_count()
      CALL deck%get_error_report(ierr, str)
      PRINT *, str
    END DO
    STOP
  END IF

  PRINT *, 'Scalar value is ', scalar
  PRINT *, 'Array value is ', array

  DEALLOCATE(scalar)
  DEALLOCATE(array)

END PROGRAM testprog
