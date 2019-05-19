MODULE eis_error_mod

  USE eis_header
  USE eis_constants
  USE eis_utils
  USE eis_string_store_mod
  IMPLICIT NONE

  TYPE :: eis_error_item
    CHARACTER(LEN=:), ALLOCATABLE :: errstring
    INTEGER(eis_error) :: errcode = eis_err_none
    INTEGER :: charindex = -1
  END TYPE eis_error_item

  TYPE :: eis_error_handler
    PRIVATE
    TYPE(eis_error_item), DIMENSION(:), ALLOCATABLE :: errors
    TYPE(eis_string_store) :: strings
    LOGICAL :: is_init = .FALSE.
    INTEGER :: language = -1
    CONTAINS
    PROCEDURE, PUBLIC:: add_error => eeh_add_error
    PROCEDURE, PUBLIC :: flush_errors => eeh_flush
    PROCEDURE, PUBLIC :: get_error_count => eeh_get_count
    PROCEDURE, PUBLIC :: get_error_string => eeh_get_error_string
    PROCEDURE, PUBLIC :: get_error_string_from_code &
        => eeh_get_error_string_from_code
    PROCEDURE, PUBLIC :: get_error_cause => eeh_get_error_cause
    PROCEDURE, PUBLIC :: get_error_report => eeh_get_error_report
    PROCEDURE, PUBLIC :: print_error_string => eeh_print_err
    PROCEDURE, PUBLIC :: init => eeh_init
  END TYPE eis_error_handler

  CONTAINS

  SUBROUTINE eeh_init(this, language)
    CLASS(eis_error_handler), INTENT(INOUT) :: this
    INTEGER, INTENT(IN), OPTIONAL :: language
    INTEGER :: language_i

    IF (PRESENT(language)) THEN
      language_i = language
    ELSE
      language_i = eis_lang_en
    END IF

    IF (this%is_init .AND. language_i== this%language) RETURN
    this%is_init = .TRUE.
    this%language = language_i

    !All non-English translations are from machine translation
    !IF you wish to supply an improved translation, please contact the authors
    IF (this%language == eis_lang_en) THEN
      CALL this%strings%store('err_src_parse','Error when parsing text to &
          &stack')
      CALL this%strings%store('err_src_simplify','Error when simplifying stack')
      CALL this%strings%store('err_src_emplace','Error during emplacement of &
          &function')
      CALL this%strings%store('err_src_evaluate','Error when evaluating stack')
      CALL this%strings%store('err_src_undefined', 'Error in unspecified part &
          &of the parser. (This is probably a bug in the parser code)')

      CALL this%strings%store('err_bad_value', 'There was a bad value in the &
          &expression')
      CALL this%strings%store('err_malformed', 'The expression was of invalid &
          &form')
      CALL this%strings%store('err_wrong_parameters', 'The wrong number of &
          &parameters was used in a function call')
      CALL this%strings%store('err_maths_domain', 'A mathematically invalid &
          &operation was requested')
      CALL this%strings%store('err_not_found', 'The specified block was not &
          &found in the list of known keys')
      CALL this%strings%store('err_has_emplaced', 'The specified stack has &
          &unresolved emplaced elements')
      CALL this%strings%store('err_has_deferred', 'The specified stack has &
          &unresolved deferred elements')
      CALL this%strings%store('err_where', 'The specified stack uses the &
        &"where" construct but is not tested for no-ops when evaluated')
      CALL this%strings%store('err_bracketed_constant', 'Attempting to &
          &subscript a constant')
      CALL this%strings%store('err_extra_bracket', 'Extraneous bracket')
      CALL this%strings%store('err_bad_stack', 'Invalid or unavailable stack')
      CALL this%strings%store('err_report_place', 'In block with text &
          &"{errtext}" starting at character position {charpos}: ')
      CALL this%strings%store('err_report_fail', 'Unable to report source of &
          &error. To see the location of the error do not minify the stack. &
          &Errors are :')
    ELSE IF (this%language == eis_lang_ru) THEN
      CALL this%strings%store('err_src_parse','Ошибка при разборе текста в &
          &стек')
      CALL this%strings%store('err_src_simplify','Ошибка при упрощении стека')
      CALL this%strings%store('err_src_emplace','Ошибка при «emplacing» &
          &функции')
      CALL this%strings%store('err_src_evaluate','Ошибка при оценке стека')
      CALL this%strings%store('err_src_undefined', 'Ошибка в неуказанной части &
          &парсера. (Это, вероятно, ошибка в коде парсера)')

      CALL this%strings%store('err_bad_value', 'Выражение неверно')
      CALL this%strings%store('err_malformed', 'Выражение было искажено')
      CALL this%strings%store('err_wrong_parameters', 'Неправильное количество &
          &параметров было использовано при вызове функции')
      CALL this%strings%store('err_maths_domain', 'Запрошена математически &
          &неверная операция')
      CALL this%strings%store('err_not_found', 'Указанный блок не найден в &
          &списке известных ключей')
      CALL this%strings%store('err_has_emplaced', 'В указанном стеке есть &
          &неразрешенные «emplaced» элементы')
      CALL this%strings%store('err_has_deferred', 'Указанный стек имеет &
          &неразрешенные «deferred» элементы')
      CALL this%strings%store('err_where', 'Указанный стек использует &
          &конструкцию «where», но при оценке не проверяется на нулевые &
          &операции')
      CALL this%strings%store('err_bracketed_constant', 'Попытка вызвать &
          &константу как функцию')
      CALL this%strings%store('err_extra_bracket', 'Посторонняя скобка')
      CALL this%strings%store('err_bad_stack', 'Неверный или недоступный стек')
      CALL this%strings%store('err_report_place', 'В блоке с текстом &
          &«{errtext}», начинающимся с позиции символа {charpos} :')
      CALL this%strings%store('err_report_fail', 'Невозможно &
          &сообщить источник ошибки. Чтобы увидеть расположение &
          &ошибки, не минимизируйте стек. Ошибки:')
    END IF

  END SUBROUTINE eeh_init



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



  SUBROUTINE eeh_get_error_cause(this, index, err_text, cloc_out)
    CLASS(eis_error_handler) :: this
    INTEGER, INTENT(IN) :: index
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: err_text
    INTEGER, INTENT(OUT) :: cloc_out
    LOGICAL :: ok

    cloc_out = -1
    IF (ALLOCATED(err_text)) DEALLOCATE(err_text)
    IF (.NOT. ALLOCATED(this%errors)) RETURN
    IF (index < 1 .OR. index > SIZE(this%errors)) RETURN
    cloc_out = this%errors(index)%charindex

    IF (.NOT. ALLOCATED(this%errors(index)%errstring)) RETURN
    ALLOCATE(err_text, SOURCE = this%errors(index)%errstring)

  END SUBROUTINE eeh_get_error_CAUSE



  SUBROUTINE eeh_get_error_string_from_code(this, errcode, err_string, &
      err_source)
    CLASS(eis_error_handler) :: this
    INTEGER(eis_error), INTENT(IN) :: errcode
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: err_string, err_source
    LOGICAL :: ok

    IF (ALLOCATED(err_string)) DEALLOCATE(err_string)
    IF (ALLOCATED(err_source)) DEALLOCATE(err_source)

    IF (IAND(errcode, eis_err_parser) /= 0) THEN
      ok = this%strings%append('err_src_parse', err_source)
    ELSE IF (IAND(errcode, eis_err_simplifier) /= 0) THEN
      ok = this%strings%append('err_src_simplify', err_source)
    ELSE IF (IAND(errcode, eis_err_emplacer) /= 0) THEN
      ok = this%strings%append('err_src_emplace', err_source)
    ELSE IF (IAND(errcode, eis_err_evaluator) /= 0) THEN
      ok = this%strings%append('err_src_evaluate', err_source)
    ELSE
      ok = this%strings%append('err_src_undefined', err_source)
    END IF


    IF (IAND(errcode, eis_err_bad_value) /= 0) THEN
      ok = this%strings%append('err_bad_value', err_string)
    END IF
    IF (IAND(errcode, eis_err_malformed) /= 0) THEN
      ok = this%strings%append('err_malformed', err_string)
    END IF
    IF (IAND(errcode, eis_err_wrong_parameters) /= 0) THEN
      ok = this%strings%append('err_wrong_parameters', err_string)
    END IF
    IF (IAND(errcode, eis_err_maths_domain) /= 0) THEN
      ok = this%strings%append('err_maths_domain', err_string)
    END IF
    IF (IAND(errcode, eis_err_not_found) /= 0) THEN
      ok = this%strings%append('err_not_found', err_string)
    END IF
    IF (IAND(errcode, eis_err_has_emplaced) /= 0) THEN
      ok = this%strings%append('err_has_emplaced', err_string)
    END IF
    IF (IAND(errcode, eis_err_has_deferred) /= 0) THEN
      ok = this%strings%append('err_has_deferred', err_string)
    END IF
    IF (IAND(errcode, eis_err_where) /= 0) THEN
      ok = this%strings%append('err_where', err_string)
    END IF
    IF (IAND(errcode, eis_err_bracketed_constant) /= 0) THEN
      ok = this%strings%append('err_bracketed_constant', err_string)
    END IF
    IF (IAND(errcode, eis_err_extra_bracket) /= 0) THEN
      ok = this%strings%append('err_extra_bracket', err_string)
    END IF
    IF (IAND(errcode, eis_err_bad_stack) /= 0) THEN
      ok = this%strings%append('err_bad_stack', err_string)
    END IF

    IF (.NOT. ALLOCATED(err_string)) ALLOCATE(err_string, SOURCE = &
        'No error text found')

  END SUBROUTINE eeh_get_error_string_from_code



  SUBROUTINE eeh_get_error_string(this, index, err_string, err_source)
    CLASS(eis_error_handler) :: this
    INTEGER, INTENT(IN) :: index
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: err_string, err_source

    IF (ALLOCATED(err_string)) DEALLOCATE(err_string)
    IF (ALLOCATED(err_source)) DEALLOCATE(err_source)
    IF (.NOT. ALLOCATED(this%errors)) RETURN
    IF (index < 1 .OR. index > SIZE(this%errors)) RETURN

    CALL this%get_error_string_from_code(this%errors(index)%errcode, &
        err_string, err_source)

  END SUBROUTINE eeh_get_error_string



  SUBROUTINE eeh_get_error_report(this, index, report)
    CLASS(eis_error_handler), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: index
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: report
    CHARACTER(LEN=:), ALLOCATABLE :: errstring, errname, err_source, temp
    CHARACTER(LEN=9) :: posstr
    CHARACTER(LEN=19) :: format_str
    INTEGER :: icount, charpos, nchar
    TYPE(eis_string_store) :: errstr_store
    LOGICAL :: ok

    IF (ALLOCATED(report)) DEALLOCATE(report)

    IF (.NOT. ALLOCATED(this%errors)) THEN
      CALL eis_append_string(report, 'No stored errors')
      RETURN
    END IF

    IF (index < 1 .OR. index > SIZE(this%errors)) THEN
      CALL eis_append_string(report, 'Error out of range')
      RETURN
    END IF

    CALL this%get_error_cause(index, errname, charpos)
    CALL this%get_error_string(index, errstring, err_source)

    CALL eis_append_string(report, REPEAT("=", 80))

    CALL eis_append_string(report, err_source)

    IF (charpos > 0) THEN
      nchar = CEILING(LOG10(REAL(charpos, eis_num))+1)
      WRITE(format_str, '(A,I1,A)') '(I',nchar,')'
      WRITE(posstr, format_str) charpos
      CALL errstr_store%store('charpos', TRIM(posstr))
      CALL errstr_store%store('errtext', TRIM(errname))
      ok = this%strings%get('err_report_place', temp)
      CALL errstr_store%format_fill(temp)
      CALL eis_append_string(report, temp)
    ELSE
      ok = this%strings%append('err_report_fail', report)
    END IF
    CALL eis_append_string(report, errstring, newline = .FALSE.)
    CALL eis_append_string(report, REPEAT("=", 80))
    CALL eis_append_string(report,"")

    IF (ALLOCATED(err_source)) DEALLOCATE(err_source)
    IF (ALLOCATED(errstring)) DEALLOCATE(errstring)
    IF (ALLOCATED(errname)) DEALLOCATE(errname)
    IF (ALLOCATED(temp)) DEALLOCATE(temp)

  END SUBROUTINE eeh_get_error_report


  SUBROUTINE eeh_print_err(this, index)
    CLASS(eis_error_handler), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: index
    CHARACTER(LEN=:), ALLOCATABLE :: report

    CALL this%get_error_report(index, report)
    WRITE(*,'(A)') report
    DEALLOCATE(report)

  END SUBROUTINE eeh_print_err

END MODULE eis_error_mod
