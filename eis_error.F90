MODULE eis_error_mod

  USE eis_header
  USE eis_constants
  IMPLICIT NONE

  TYPE :: eis_error_item
    CHARACTER(LEN=:), ALLOCATABLE :: errstring
    INTEGER(eis_error) :: errcode = eis_err_none
    INTEGER :: charindex = -1
  END TYPE eis_error_item

  TYPE :: eis_error_handler
    TYPE(eis_error_item), DIMENSION(:), ALLOCATABLE :: errors
    CONTAINS
    PROCEDURE :: add_error => eeh_add_error
    PROCEDURE :: flush_errors => eeh_flush
    PROCEDURE :: get_error_count => eeh_get_count
    PROCEDURE, PRIVATE :: get_error_string => eeh_get_error_string
    PROCEDURE, PRIVATE :: get_error_string_from_code &
        => eeh_get_error_string_from_code
    PROCEDURE, PRIVATE :: get_error_source => eeh_get_error_source
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



  SUBROUTINE eeh_add_error(this, err_source, errcode, errstring, charindex)

    CLASS(eis_error_handler), INTENT(INOUT) :: this
    INTEGER(eis_error), INTENT(IN) :: err_source, errcode
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: errstring
    INTEGER, OPTIONAL, INTENT(IN) :: charindex
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
    IF (PRESENT(errstring)) THEN
      ALLOCATE(temp(sz)%errstring, SOURCE = errstring)
    ELSE
      ALLOCATE(temp(sz)%errstring, SOURCE = '{UNKNOWN}')
    END IF
    IF (PRESENT(charindex)) temp(sz)%charindex = charindex
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



  FUNCTION eeh_get_error_source(this, index, str_out, cloc_out)
    CLASS(eis_error_handler) :: this
    INTEGER, INTENT(IN) :: index
    CHARACTER(LEN=*), INTENT(INOUT) :: str_out
    INTEGER, INTENT(OUT) :: cloc_out
    INTEGER :: eeh_get_error_source, pos

    cloc_out = -1
    eeh_get_error_source = -1
    IF (.NOT. ALLOCATED(this%errors)) RETURN
    IF (index < 1 .OR. index > SIZE(this%errors)) RETURN
    IF (.NOT. ALLOCATED(this%errors(index)%errstring)) RETURN

    eeh_get_error_source = LEN(this%errors(index)%errstring)
    pos = MIN(LEN(str_out), eeh_get_error_source)
    str_out(1:pos) = this%errors(index)%errstring(1:pos)
    cloc_out = this%errors(index)%charindex

  END FUNCTION eeh_get_error_source



  FUNCTION eeh_get_error_string_from_code(this, errcode, str_out)
    CLASS(eis_error_handler) :: this
    INTEGER(eis_error), INTENT(IN) :: errcode
    CHARACTER(LEN=*), INTENT(INOUT) :: str_out
    CHARACTER(LEN=:), ALLOCATABLE :: str1
    INTEGER :: eeh_get_error_string_from_code, pos

    IF (IAND(errcode, eis_err_bad_value) /= 0) THEN
      CALL append_string(str1, 'There was a bad value in the expression')
    END IF
    IF (IAND(errcode, eis_err_malformed) /= 0) THEN
      CALL append_string(str1, 'The expression was of invalid form')
    END IF
    IF (IAND(errcode, eis_err_wrong_parameters) /= 0) THEN
      CALL append_string(str1, 'The wrong number of parameters was used in a &
          &function call')
    END IF
    IF (IAND(errcode, eis_err_maths_domain) /= 0) THEN
      CALL append_string(str1, 'A mathematically invalid operation was &
          &requested')
    END IF
    IF (IAND(errcode, eis_err_not_found) /= 0) THEN
      CALL append_string(str1, 'The specified block was not found in the list &
          &of known keys')
    END IF
    IF (IAND(errcode, eis_err_has_emplaced) /= 0) THEN
      CALL append_string(str1, 'The specified stack has emplaced elements')
    END IF
    IF (IAND(errcode, eis_err_where) /= 0) THEN
      CALL append_string(str1, 'The specified stack uses the "where" construct &
        &but is not tested for no-ops when evaluated')
    END IF

    IF (.NOT. ALLOCATED(str1)) ALLOCATE(str1, SOURCE = 'No error text found')

    eeh_get_error_string_from_code = LEN(str1)
    pos = MIN(LEN(str_out), LEN(str1))
    str_out = ""
    str_out(1:pos) = str1(1:pos)
    DEALLOCATE(str1)

  END FUNCTION eeh_get_error_string_from_code



  FUNCTION eeh_get_error_string(this, index, str_out)
    CLASS(eis_error_handler) :: this
    INTEGER, INTENT(IN) :: index
    CHARACTER(LEN=*), INTENT(INOUT) :: str_out
    INTEGER :: eeh_get_error_string

    eeh_get_error_string = -1

    IF (.NOT. ALLOCATED(this%errors)) RETURN
    IF (index < 1 .OR. index > SIZE(this%errors)) RETURN

    eeh_get_error_string = this%get_error_string_from_code(&
        this%errors(index)%errcode, str_out)

  END FUNCTION eeh_get_error_string



  SUBROUTINE eeh_print_err(this, index)
    CLASS(eis_error_handler), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: index
    CHARACTER(LEN=:), ALLOCATABLE :: errstring, errname
    CHARACTER(LEN=19) :: format_str
    INTEGER :: icount, charpos, nchar

    IF (.NOT. ALLOCATED(this%errors)) THEN
      PRINT *, 'No stored errors'
      RETURN
    END IF

    IF (index < 1 .OR. index > SIZE(this%errors)) THEN
      PRINT *,'Error out of range'
      RETURN
    END IF

    ALLOCATE(CHARACTER(LEN=1)::errstring)
    icount = this%get_error_string(index, errstring)
    DEALLOCATE(errstring)
    ALLOCATE(CHARACTER(LEN=icount)::errstring)
    icount = this%get_error_string(index, errstring)

    ALLOCATE(CHARACTER(LEN=1)::errname)
    icount = this%get_error_source(index, errname, charpos)
    IF (icount > 0) THEN
      DEALLOCATE(errname)
      ALLOCATE(CHARACTER(LEN=icount)::errname)
      icount = this%get_error_source(index, errname, charpos)
    END IF


    PRINT *,REPEAT("=", 80)

    IF (IAND(this%errors(index)%errcode, eis_err_parser) /= 0) THEN
      PRINT *,'Error when parsing text to stack'
    ELSE IF (IAND(this%errors(index)%errcode, eis_err_simplifier) /= 0) THEN
      PRINT *,'Error when simplifying stack'
    ELSE IF (IAND(this%errors(index)%errcode, eis_err_emplacer) /= 0) THEN
      PRINT *,'Error when emplacement of function'
    ELSE IF (IAND(this%errors(index)%errcode, eis_err_evaluator) /= 0) THEN
      PRINT *,'Error when evaluating stack'
    END IF

    IF (charpos > 0) THEN
      nchar = CEILING(LOG10(REAL(charpos, eis_num)))
      WRITE(format_str, '(A,I1,A)') '(A, A, A, I',nchar,', A, A)'
      PRINT format_str, 'In block with text "', TRIM(errname), &
          '" starting at character position ', charpos, ' : ' , errstring
    ELSE
      PRINT *,'Unable to report source of error. To see the location of the &
          &error do not minify the stack. Errors are :'
      PRINT *, errstring
    END IF
    PRINT *,REPEAT("=", 80)
    PRINT *, ""

  END SUBROUTINE eeh_print_err

END MODULE eis_error_mod
