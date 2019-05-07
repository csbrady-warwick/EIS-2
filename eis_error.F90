MODULE eis_error_mod

  USE eis_header
  USE eis_constants
  IMPLICIT NONE

  TYPE :: eis_error_item
    CHARACTER(LEN=:), ALLOCATABLE :: errstring
    INTEGER(eis_error) :: errcode = eis_err_none
  END TYPE eis_error_item

  TYPE :: eis_error_handler
    TYPE(eis_error_item), DIMENSION(:), ALLOCATABLE :: errors
    CONTAINS
    PROCEDURE :: add_error => eeh_add_error
    PROCEDURE :: flush_errs => eeh_flush
    PROCEDURE :: get_error_count => eeh_get_count
    PROCEDURE, PRIVATE :: get_error_string => eeh_get_error_string
    PROCEDURE :: print_error_string => eeh_print_err
  END TYPE eis_error_handler

  CONTAINS

  SUBROUTINE append_string(str_old, str_new)
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: str_old
    CHARACTER(LEN=*), INTENT(IN) :: str_new
    CHARACTER(LEN=:), ALLOCATABLE :: temp

    IF (ALLOCATED(str_old)) THEN
      ALLOCATE(temp, SOURCE = str_old)
      DEALLOCATE(str_old)
      ALLOCATE(str_old, SOURCE = temp // NEW_LINE(str_new) // str_new)
      DEALLOCATE(temp)
    ELSE
      ALLOCATE(str_old, SOURCE = str_new)
    END IF

  END SUBROUTINE append_string



  SUBROUTINE eeh_add_error(this, err_source, errcode, errstring)

    CLASS(eis_error_handler), INTENT(INOUT) :: this
    INTEGER(eis_error), INTENT(IN) :: err_source, errcode
    CHARACTER(LEN=*), INTENT(IN) :: errstring
    TYPE(eis_error_item), DIMENSION(:), ALLOCATABLE :: temp
    INTEGER :: sz

    IF (errcode == eis_err_none) RETURN !Don't add on no error

    IF (ALLOCATED(this%errors)) THEN
      sz = SIZE(this%errors)
      ALLOCATE(temp(sz+1))
      temp(1:sz) = this%errors
      DEALLOCATE(this%errors)
      sz = sz + 1
    ELSE
      sz = 1
      ALLOCATE(temp(sz))
    END IF

    temp(sz)%errcode = IOR(err_source, errcode)
    ALLOCATE(temp(sz)%errstring, SOURCE = errstring)
    CALL MOVE_ALLOC(temp, this%errors)

  END SUBROUTINE eeh_add_error



  SUBROUTINE eeh_flush(this)
    CLASS(eis_error_handler), INTENT(INOUT) :: this
    IF (ALLOCATED(this%errors)) DEALLOCATE(this%errors)
  END SUBROUTINE eeh_flush




  FUNCTION eeh_get_count(this)
    CLASS(eis_error_handler), INTENT(IN) :: this
    INTEGER :: eeh_get_count

    IF (ALLOCATED(this%errors)) THEN
      eeh_get_count = SIZE(this%errors)
    ELSE
      eeh_get_count = 0
    END IF
  END FUNCTION eeh_get_count



  FUNCTION eeh_get_error_string(this, errcode, str_out)
    CLASS(eis_error_handler) :: this
    INTEGER(eis_error), INTENT(IN) :: errcode
    CHARACTER(LEN=*), INTENT(INOUT) :: str_out
    CHARACTER(LEN=:), ALLOCATABLE :: str1
    INTEGER :: eeh_get_error_string, pos

    IF (IAND(errcode, eis_err_bad_value) /= 0) THEN
      CALL append_string(str1, 'There was a bad value in the expression')
    END IF
    IF (IAND(errcode, eis_err_malformed) /= 0) THEN
      CALL append_string(str1, 'The expression was of invalid form')
    END IF
    IF (IAND(errcode, eis_err_wrong_parameters) /= 0) THEN
      CALL append_string(str1, 'The wrong number of parameters was used in a &
          &function')
    END IF
    IF (IAND(errcode, eis_err_maths_domain) /= 0) THEN
      CALL append_string(str1, 'A mathematically invalid operation was &
          &requested')
    END IF

    IF (.NOT. ALLOCATED(str1)) ALLOCATE(str1, SOURCE = 'No error found')

    eeh_get_error_string = LEN(str1)
    pos = MIN(LEN(str_out), LEN(str1))
    str_out = ""
    str_out(1:pos) = str1(1:pos)
    DEALLOCATE(str1)

  END FUNCTION eeh_get_error_string



  SUBROUTINE eeh_print_err(this, index)
    CLASS(eis_error_handler), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: index
    CHARACTER(LEN=:), ALLOCATABLE :: errstring
    INTEGER :: icount

    IF (.NOT. ALLOCATED(this%errors)) THEN
      PRINT *, 'No stored errors'
      RETURN
    END IF

    IF (index < 1 .OR. index > SIZE(this%errors)) THEN
      PRINT *,'Error out of range'
      RETURN
    END IF

    ALLOCATE(CHARACTER(LEN=1)::errstring)
    icount = this%get_error_string(this%errors(index)%errcode, errstring)
    DEALLOCATE(errstring)
    ALLOCATE(CHARACTER(LEN=icount)::errstring)
    icount = this%get_error_string(this%errors(index)%errcode, errstring)

    PRINT *,'Error in block with text ', TRIM(this%errors(index)%errstring)
    PRINT *,'-------------------------'
    PRINT *, errstring

  END SUBROUTINE eeh_print_err

END MODULE eis_error_mod
