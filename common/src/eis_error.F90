MODULE eis_error_mod

  USE eis_constants
  USE eis_key_value_store_mod
  USE eis_utils
  IMPLICIT NONE

  !> Type representing an error
  TYPE :: eis_error_item
    !> String representation of the source of the error
    CHARACTER(LEN=:), ALLOCATABLE :: errstring
    !> Filename for the source of the error
    CHARACTER(LEN=:), ALLOCATABLE :: filename
    !> Full line that contains the source of the error
    CHARACTER(LEN=:), ALLOCATABLE :: full_line
    !> Numerical representation of the type of the error
    INTEGER(eis_error) :: errcode = eis_err_none
    !> Character offset from start of the string of the error
    INTEGER :: charindex = -1
    !> Line number for an error on a specific line of a file
    INTEGER :: line_number = -1
    !> Context line number. Used if the error is caused by a
    !> different line than defines the text
    INTEGER :: context_line_number
    !> Context filename
    CHARACTER(LEN=:), ALLOCATABLE :: context_filename
    INTEGER :: full_line_pos = -1
  END TYPE eis_error_item

  !>Error handler class
  TYPE :: eis_error_handler
    PRIVATE
    !> Held list of errors
    TYPE(eis_error_item), DIMENSION(:), ALLOCATABLE :: errors
    !> Object holding the error report strings in the specified language
    TYPE(eis_key_value_store) :: strings
    !>Is this error handler initialised
    LOGICAL :: is_init = .FALSE.
    CHARACTER(LEN=:), ALLOCATABLE :: language_pack
    PROCEDURE(on_error_callback), NOPASS, POINTER :: on_error_fn => NULL()
    CONTAINS
    PROCEDURE, PUBLIC:: add_error => eeh_add_error !< Add an error
    PROCEDURE, PUBLIC :: flush_errors => eeh_flush !< Delete all stored errors
    !> Return number of errors
    PROCEDURE, PUBLIC :: get_error_count => eeh_get_count
    !> Return string describing error for a stored error item
    PROCEDURE, PUBLIC :: get_error_string => eeh_get_error_string
    !> Return string describing error for a given error code
    PROCEDURE, PUBLIC :: get_error_string_from_code &
        => eeh_get_error_string_from_code
    !> Return string describing the cause of an error
    PROCEDURE, PUBLIC :: get_error_cause => eeh_get_error_cause
    !> Return entire error report
    PROCEDURE, PUBLIC :: get_error_report => eeh_get_error_report
    !> Print error report to screen
    PROCEDURE, PUBLIC :: print_error_string => eeh_print_err
    !> Initialise error handler
    PROCEDURE, PUBLIC :: init => eeh_init
  END TYPE eis_error_handler

  PRIVATE
  PUBLIC :: eis_error_handler

  CONTAINS

  !>@brief
  !>Is an optional string both present and not empty
  FUNCTION filled(str)
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: str
    LOGICAL :: filled
    filled = .FALSE.
    IF (PRESENT(str)) THEN
      IF (LEN(TRIM(str)) /= 0) filled = .TRUE.
    END IF
  END FUNCTION filled



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Initialise the error handler with a specific language for error messages
  !> @param[inout] this
  !> @param[out] errcode
  !> @param[in] language_pack
  !> @param[in] on_error
  SUBROUTINE eeh_init(this, errcode, language_pack, on_error)
    CLASS(eis_error_handler), INTENT(INOUT) :: this !< Self pointer
    INTEGER(eis_error), INTENT(OUT) :: errcode
    !> Optional filename for a language pack. This will described the required
    !> errors in a "key=value" form. See the manual for a description of the
    !> keys needed
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: language_pack
    !> Function to be called whenever an error is added to the error handler
    PROCEDURE(on_error_callback), OPTIONAL :: on_error
    LOGICAL :: same_language, lang_default

    IF (ALLOCATED(this%language_pack)) THEN
      IF (PRESENT(language_pack)) THEN
        same_language = TRIM(language_pack) == TRIM(this%language_pack)
      ELSE
        same_language = .FALSE.
        DEALLOCATE(this%language_pack)
        ALLOCATE(this%language_pack, SOURCE = language_pack)
      END IF
    ELSE
      IF (PRESENT(language_pack)) THEN
        same_language = .FALSE.
        ALLOCATE(this%language_pack, SOURCE = language_pack)
      ELSE
        same_language = .TRUE.
      END IF
    END IF

    IF (PRESENT(on_error)) this%on_error_fn => on_error

    IF (this%is_init .AND. same_language) RETURN
    this%is_init = .TRUE.

    lang_default = .FALSE.
    IF (PRESENT(language_pack)) THEN
      errcode = eis_err_none
      CALL this%strings%load_from_ascii_file(language_pack, errcode)
      IF (errcode /= eis_err_none) THEN
        PRINT *,'Error loading language pack for error handler. Default to &
            &English'
        lang_default = .TRUE.
        DEALLOCATE(this%language_pack)
      END IF
    END IF

    IF (.NOT. PRESENT(language_pack) .OR. lang_default) THEN
      CALL this%strings%store('err_src_parse','parsing text to &
          &stack')
      CALL this%strings%store('err_src_simplify','simplifying a stack')
      CALL this%strings%store('err_src_emplace','emplacing a function')
      CALL this%strings%store('err_src_evaluate','evaluating stack')
      CALL this%strings%store('err_src_file','handling text file')
      CALL this%strings%store('err_src_deck','loading input deck file')
      CALL this%strings%store('err_src_deck_definition','handling &
          &input deck definition')
      CALL this%strings%store('err_src_deck_parser', 'parsing &
          &input deck file')
      CALL this%strings%store('err_src_undefined', 'in unspecified part &
          &of EIS. (This is probably a bug in EIS)')

      CALL this%strings%store('err_bad_value', 'There was a bad value in the &
          &expression')
      CALL this%strings%store('err_malformed', 'The expression was of invalid &
          &form')
      CALL this%strings%store('err_wrong_parameters', 'The wrong number of &
          &parameters was used in a function call')
      CALL this%strings%store('err_maths_domain', 'A mathematically invalid &
          &operation was requested')
      CALL this%strings%store('err_not_found', 'Unknown value or function')
      CALL this%strings%store('err_has_emplaced', 'The specified stack has &
          &unresolved emplaced elements')
      CALL this%strings%store('err_has_deferred', 'The specified stack has &
          &unresolved deferred elements')
      CALL this%strings%store('err_bracketed_constant', 'Attempting to &
          &subscript a constant')
      CALL this%strings%store('err_extra_bracket', 'Extraneous bracket')
      CALL this%strings%store('err_bad_stack', 'Invalid or unavailable stack')
      CALL this%strings%store('err_extra_results', 'Unexpected additional &
          &results from a stack')

      CALL this%strings%store('err_no_luns', 'No logical unit numbers were &
          &available for file access')
      CALL this%strings%store('err_no_file', 'The requested file was not found')
      CALL this%strings%store('err_malformed_file', 'Malformed string when &
          &reading a serialisation string.')
      CALL this%strings%store('err_mismatched_begin_end', 'Deck block "end"ed &
          &without matching "start" directive')
      CALL this%strings%store('err_deck_too_deep', 'Deck block encountered &
          &with too high a depth (i.e is a child of a block that should not &
          &have children). This is a malformed deck.')
      CALL this%strings%store('err_deck_empty_block','Deck block has no keys &
         &when this is not permitted. This is a malformed deck')
      CALL this%strings%store('err_root_keys', 'Deck key encountered &
          &outside a block. This is a malformed deck')
      CALL this%strings%store('err_defn_invalid', 'Deck definition object is in&
          & an invalid state')
      CALL this%strings%store('err_unknown_block', 'An unknown block was found &
          &in the deck')
      CALL this%strings%store('err_unknown_key', 'An unknown key was found &
          &in the deck')
      CALL this%strings%store('err_bad_key', 'A key in the deck could not be &
          &parsed')
      CALL this%strings%store('err_host', 'A non-parser error was reported')
      CALL this%strings%store('err_text', 'This function should not take text &
          &parameters')
      CALL this%strings%store('err_interop', 'You are using a Fortran/C &
          &interoperability function but the item you are using is not &
          &interoperable')
      CALL this%strings%store('err_out_of_range', 'The specified value is &
          &outwith the range of acceptable values for this function')
      CALL this%strings%store('err_stack_params', 'A stack variable that &
          &returns more than one parameter was used in a mathematical &
          &expression')

      CALL this%strings%store('err_is_error', '{error} at {errfile}, &
          &line {errline}')
      CALL this%strings%store('err_is_error_file', '{error} at {errfile}')
      CALL this%strings%store('err_is_error_none', '{error}')
      CALL this%strings%store('err_report_place','{charpos}:"{errtext}" &
          &- {error}')
      CALL this%strings%store('err_proximate_source', 'Error reported from &
          &{context_errfile}:{context_errline}')
      CALL this%strings%store('err_report_fail', 'Unable to report source of &
          &error')
    END IF

  END SUBROUTINE eeh_init


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Initialise the error handler with a specific language for error messages
  !> @param[inout] this
  !> @param[in] err_source
  !> @param[in] errcode
  !> @param[in] errstring
  !> @param[in] charindex
  !> @param[in] line_number
  !> @param[in] full_line
  !> @param[in] full_line_pos
  !> @param[in] context_line_number
  !> @param[in] context_filename
  SUBROUTINE eeh_add_error(this, err_source, errcode, errstring, &
      charindex, filename, line_number, full_line, full_line_pos, &
      context_line_number, context_filename)

    CLASS(eis_error_handler), INTENT(INOUT) :: this !< Self pointer
    !> Type code for source of error (tokenize, evalaute etc.)
    INTEGER(eis_error), INTENT(IN) :: err_source
    !> Type code for type of error
    INTEGER(eis_error), INTENT(IN) ::  errcode
    !> String describing parser key that causes the error
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: errstring
    !> Character location of key in error
    INTEGER, OPTIONAL, INTENT(IN) :: charindex
    !> String describing the file that contains the error
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: filename
    !> Line number of the error in multiple lines
    INTEGER, OPTIONAL, INTENT(IN) :: line_number
    !> Full line containing the error
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: full_line
    !> Position of the error in the full line
    INTEGER, INTENT(IN), OPTIONAL :: full_line_pos
    !> Line number for context information
    INTEGER, INTENT(IN), OPTIONAL :: context_line_number
    !> Filename for context information
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: context_filename
    TYPE(eis_error_item), DIMENSION(:), ALLOCATABLE :: temp
    INTEGER :: sz
    INTEGER(eis_error) :: err

    IF (.NOT. this%is_init) CALL this%init(err)

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
    END IF
    IF (FILLED(filename)) ALLOCATE(temp(sz)%filename, SOURCE = filename)
    IF (PRESENT(charindex)) temp(sz)%charindex = charindex
    IF (PRESENT(line_number)) temp(sz)%line_number = line_number
    IF (FILLED(full_line)) ALLOCATE(temp(sz)%full_line, SOURCE = full_line)
    IF (PRESENT(full_line_pos)) temp(sz)%full_line_pos = full_line_pos
    IF (PRESENT(context_line_number)) temp(sz)%context_line_number &
        = context_line_number
    IF (FILLED(context_filename)) ALLOCATE(temp(sz)%context_filename, &
        SOURCE = context_filename)

    CALL MOVE_ALLOC(temp, this%errors)

    IF (ASSOCIATED(this%on_error_fn)) CALL this%on_error_fn(errcode)

  END SUBROUTINE eeh_add_error



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Delete all stored errors
  !> @param[inout] this
  SUBROUTINE eeh_flush(this)
    CLASS(eis_error_handler), INTENT(INOUT) :: this
    IF (ALLOCATED(this%errors)) DEALLOCATE(this%errors)
  END SUBROUTINE eeh_flush



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Return the number of stored errors
  !> @param[in] this
  !> @return eeh_get_count
  FUNCTION eeh_get_count(this)
    CLASS(eis_error_handler), INTENT(IN) :: this
    INTEGER :: eeh_get_count

    IF (ALLOCATED(this%errors)) THEN
      eeh_get_count = SIZE(this%errors)
    ELSE
      eeh_get_count = 0
    END IF
  END FUNCTION eeh_get_count


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Return information about the key in the parsed string that is the cause of
  !> the errors
  !> @param[inout] this
  !> @param[in] index
  !> @param[inout] err_text
  !> @param[out] cloc_out
  !> @param[out] filename
  !> @param[out] line_number
  !> @param[out] context_filename
  !> @param[out] context_line_number
  !> @param[out] full_line_pos
  SUBROUTINE eeh_get_error_cause(this, index, err_text, cloc_out, filename, &
      line_number, context_filename, context_line_number, full_line_pos)
    CLASS(eis_error_handler), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: index !< Error index code
    !>Error text for the returned error text
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: err_text 
    !>Character location for the returned error (in the original string)
    INTEGER, INTENT(OUT), OPTIONAL :: cloc_out
    !>Filename for the error source
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT), OPTIONAL :: filename
    !> Line number of the cause of the error
    INTEGER, INTENT(OUT), OPTIONAL :: line_number
    !> Filename for the error context source
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT), OPTIONAL :: context_filename
    !> Line number for the error context
    INTEGER, INTENT(OUT), OPTIONAL :: context_line_number
    !> Position of the error text in the full line that encompasses it
    INTEGER, INTENT(OUT), OPTIONAL :: full_line_pos
    INTEGER(eis_error) :: err

    IF (.NOT. this%is_init) CALL this%init(err)

    cloc_out = -1
    IF (PRESENT(line_number)) line_number = -1
    IF (PRESENT(context_line_number)) context_line_number = -1
    IF (.NOT. ALLOCATED(this%errors)) RETURN
    IF (index < 1 .OR. index > SIZE(this%errors)) RETURN
    cloc_out = this%errors(index)%charindex

    IF (PRESENT(line_number)) line_number = this%errors(index)%line_number
    IF (PRESENT(context_line_number)) context_line_number = &
        this%errors(index)%context_line_number

    IF (ALLOCATED(this%errors(index)%errstring)) THEN
      ALLOCATE(err_text, SOURCE = TRIM(this%errors(index)%errstring))
    END IF

    IF (ALLOCATED(this%errors(index)%filename) .AND. PRESENT(filename)) THEN
      ALLOCATE(filename, SOURCE = TRIM(this%errors(index)%filename))
    END IF

    IF (ALLOCATED(this%errors(index)%context_filename) &
        .AND. PRESENT(context_filename)) THEN
      ALLOCATE(context_filename, SOURCE = &
          TRIM(this%errors(index)%context_filename))
    END IF

    IF (PRESENT(full_line_pos)) full_line_pos = this%errors(index)%full_line_pos

  END SUBROUTINE eeh_get_error_cause


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Return information about the type of error specified by a given error code
  !> error is returned in the specified language
  !> @param[in] this
  !> @param[in] errcode
  !> @param[inout] err_string
  !> @param[inout] err_source
  SUBROUTINE eeh_get_error_string_from_code(this, errcode, err_string, &
      err_source)
    CLASS(eis_error_handler), INTENT(INOUT) :: this
    INTEGER(eis_error), INTENT(IN) :: errcode !< Error code to look up
    !> Error type string in specified language
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: err_string
    !> Error source string in specified language
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: err_source
    LOGICAL :: ok
    INTEGER(eis_error) :: err

    IF (.NOT. this%is_init) CALL this%init(err)

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
    ELSE IF (IAND(errcode, eis_err_file) /= 0) THEN
      ok = this%strings%append('err_src_file', err_source)
    ELSE IF (IAND(errcode, eis_err_deck_file) /= 0) THEN
      ok = this%strings%append('err_src_deck', err_source)
    ELSE IF (IAND(errcode, eis_err_deck_definition) /= 0) THEN
      ok = this%strings%append('err_src_deck_definition', err_source)
    ELSE IF (IAND(errcode, eis_err_deck_parser) /= 0) THEN
      ok = this%strings%append('err_src_deck_parser', err_source)
    ELSE
      ok = this%strings%append('err_src_undefined', err_source)
    END IF

    IF (.NOT. ok) THEN
      ALLOCATE(err_source, SOURCE = 'SERIOUS ERROR: UNABLE TO FIND ERROR&
          & SOURCE STRING. THIS MIGHT BE A MALFORMED LANGUAGE PACK OR &
          & MIGHT BE A SERIOUS ERROR IN THE EIS PARSER SYSTEM')
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
    IF (IAND(errcode, eis_err_bracketed_constant) /= 0) THEN
      ok = this%strings%append('err_bracketed_constant', err_string)
    END IF
    IF (IAND(errcode, eis_err_extra_bracket) /= 0) THEN
      ok = this%strings%append('err_extra_bracket', err_string)
    END IF
    IF (IAND(errcode, eis_err_bad_stack) /= 0) THEN
      ok = this%strings%append('err_bad_stack', err_string)
    END IF

    !Errors from deck parser
    IF (IAND(errcode, eis_err_no_luns) /= 0) THEN
      ok = this%strings%append('err_no_luns', err_string)
    END IF
    IF (IAND(errcode, eis_err_no_file) /= 0) THEN
      ok = this%strings%append('err_no_file', err_string)
    END IF
    IF (IAND(errcode, eis_err_malformed_file) /= 0) THEN
      ok = this%strings%append('err_malformed_file', err_string)
    END IF
    IF (IAND(errcode, eis_err_mismatched_begin_end) /= 0) THEN
      ok = this%strings%append('err_mismatched_begin_end', err_string)
    END IF
    IF (IAND(errcode, eis_err_deck_too_deep) /= 0) THEN
      ok = this%strings%append('err_deck_too_deep', err_string)
    END IF
    IF (IAND(errcode, eis_err_deck_empty_block) /= 0) THEN
      ok = this%strings%append('err_deck_empty_block', err_string)
    END IF
    IF (IAND(errcode, eis_err_root_keys) /= 0) THEN
      ok = this%strings%append('err_root_keys', err_string)
    END IF

    !Errors from deck evaluation
    IF (IAND(errcode, eis_err_unknown_block) /= 0) THEN
      ok = this%strings%append('err_unknown_block', err_string)
    END IF
    IF (IAND(errcode, eis_err_unknown_key) /= 0) THEN
      ok = this%strings%append('err_unknown_key', err_string)
    END IF
    IF (IAND(errcode, eis_err_bad_key) /= 0) THEN
      ok = this%strings%append('err_bad_key', err_string)
    END IF
    IF (IAND(errcode, eis_err_host) /= 0) THEN
      ok = this%strings%append('err_host', err_string)
    END IF
    IF (IAND(errcode, eis_err_text) /=0) THEN
      ok = this%strings%append('err_text', err_string)
    END IF
    IF (IAND(errcode, eis_err_interop) /=0) THEN
      ok = this%strings%append('err_interop', err_string)
    END IF
    IF (IAND(errcode, eis_err_out_of_range) /=0) THEN
      ok = this%strings%append('err_out_of_range', err_string)
    END IF
    IF (IAND(errcode, eis_err_stack_params) /= 0) THEN
      ok = this%strings%append('err_stack_params', err_string)
    END IF

    IF (.NOT. ALLOCATED(err_string)) ALLOCATE(err_string, SOURCE = &
        'No error text found')

  END SUBROUTINE eeh_get_error_string_from_code



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Return information about the type of error stored in a given stored error
  !> error is returned in the specified language
  !> @param[in] this
  !> @param[in] index
  !> @param[inout] err_string
  !> @param[inout] err_source
  SUBROUTINE eeh_get_error_string(this, index, err_string, err_source)
    CLASS(eis_error_handler), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: index !< Error index code
    !> Error type string in specified language
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: err_string
    !> Error source string in specified language
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: err_source
    INTEGER(eis_error) :: err

    IF (.NOT. this%is_init) CALL this%init(err)

    IF (ALLOCATED(err_string)) DEALLOCATE(err_string)
    IF (ALLOCATED(err_source)) DEALLOCATE(err_source)
    IF (.NOT. ALLOCATED(this%errors)) RETURN
    IF (index < 1 .OR. index > SIZE(this%errors)) RETURN

    CALL this%get_error_string_from_code(this%errors(index)%errcode, &
        err_string, err_source)

  END SUBROUTINE eeh_get_error_string



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get a full human readable error report for a given stored error
  !> @param[in] this
  !> @param[in] index
  !> @param[inout] report
  SUBROUTINE eeh_get_error_report(this, index, report)
    CLASS(eis_error_handler), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: index !< Error report index
    !> Error report string
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: report
    CHARACTER(LEN=:), ALLOCATABLE :: errstring, errname, err_source, temp, &
        filename, context_filename
    CHARACTER(LEN=3) :: predots, postdots
    CHARACTER(LEN=9) :: posstr, linestr
    CHARACTER(LEN=19) :: format_str
    INTEGER :: charpos, nchar, line_number, spos, epos, context_line_number
    INTEGER :: full_line_pos, nspace, nline
    TYPE(eis_key_value_store) :: errstr_store
    LOGICAL :: ok, has_context
    INTEGER(eis_error) :: err

    IF (.NOT. this%is_init) CALL this%init(err)

    IF (.NOT. ALLOCATED(this%errors)) THEN
      CALL eis_append_string(report, 'No stored errors')
      RETURN
    END IF

    IF (index < 1 .OR. index > SIZE(this%errors)) THEN
      CALL eis_append_string(report, 'Error out of range')
      RETURN
    END IF

    CALL this%get_error_cause(index, errname, charpos, filename, line_number, &
        context_filename, context_line_number, full_line_pos)
    CALL this%get_error_string(index, errstring, err_source)

    !Set up the replacement strings
    IF (ALLOCATED(errstring)) THEN
      CALL errstr_store%store('error', errstring)
    ELSE
      CALL errstr_store%store('error', '{unknown}')
    END IF

    IF (ALLOCATED(err_source)) THEN
      CALL errstr_store%store('errpart', err_source)
    ELSE
      CALL errstr_store%store('errpart', '{unknown}')
    END IF

    IF (line_number > 0) THEN
      nchar = FLOOR(LOG10(REAL(line_number, eis_num)))
      WRITE(format_str, '(A,I3,A)') '(I',nchar+1,')'
      WRITE(linestr, format_str) line_number
      CALL errstr_store%store('errline', TRIM(linestr))
    ELSE
      CALL errstr_store%store('errline', '{unknown}')
    END IF
    IF (context_line_number > 0) THEN
      nchar = FLOOR(LOG10(REAL(context_line_number, eis_num)))
      WRITE(format_str, '(A,I3,A)') '(I',nchar+1,')'
      WRITE(linestr, format_str) context_line_number
      CALL errstr_store%store('context_errline', TRIM(linestr))
    ELSE
      CALL errstr_store%store('context_errline', '{unknown}')
    END IF

    IF (ALLOCATED(filename)) THEN
      CALL errstr_store%store('errfile',filename)
    ELSE
      CALL errstr_store%store('errfile','{unknown}')
    END IF

    IF (ALLOCATED(context_filename)) THEN
      CALL errstr_store%store('context_errfile', context_filename)
    ELSE
      CALL errstr_store%store('context_errfile', '{unknown}')
    END IF

    IF (charpos > 0) THEN
      nchar = FLOOR(LOG10(REAL(charpos, eis_num)))
      WRITE(format_str, '(A,I3,A)') '(I',nchar+1,')'
      WRITE(posstr, format_str) charpos
      CALL errstr_store%store('charpos', TRIM(posstr))
    ELSE
      CALL errstr_store%store('charpos','{unknown}')
    END IF

    IF (ALLOCATED(errname)) THEN
      CALL errstr_store%store('errtext', TRIM(errname))
    ELSE
      CALL errstr_store%store('errtext', '{unknown}')
    END IF

    !Now start building the actual error report

    CALL eis_append_string(report, REPEAT("=", 80))
    !Report that there is an error
    IF (ALLOCATED(err_source) .AND. line_number > 0) THEN
      ok = this%strings%get('err_is_error', temp)
    ELSE IF (ALLOCATED(err_source)) THEN
      ok = this%strings%get('err_is_error_file', temp)
    ELSE
      ok = this%strings%get('err_is_error_only', temp)
    END IF
    CALL errstr_store%format_fill(temp)
    CALL eis_append_string(report, temp)

    !Print the context and location information
    IF (ALLOCATED(this%errors(index)%full_line)) THEN
      nchar = FLOOR(LOG10(REAL(charpos, eis_num)))+1
      nline = FLOOR(LOG10(REAL(line_number, eis_num)))+1
      nspace = MAX(nchar, nline)
      WRITE(format_str, '(A,I3,A)') '(I',nspace,')'
      WRITE(linestr, format_str) line_number
      WRITE(posstr, format_str) charpos
      CALL eis_append_string(report, "")
      spos = MAX(1, this%errors(index)%full_line_pos-10)
      epos = MIN(LEN(this%errors(index)%full_line), &
          this%errors(index)%full_line_pos+MAX(10, LEN(errname)))
      predots = "" ; postdots = ""
      IF (spos /= 1) predots = "..."
      IF (epos /= LEN(this%errors(index)%full_line)) postdots = "..."
      CALL eis_append_string(report, TRIM(linestr) // " : " // TRIM(predots) &
           // this%errors(index)%full_line(spos:epos) // TRIM(postdots))
      CALL eis_append_string(report, REPEAT(" ", &
          MAX(this%errors(index)%full_line_pos - spos + LEN(TRIM(predots)) &
          + nspace + 3,0)) // "^")
      CALL eis_append_string(report, REPEAT(" ", &
          MAX(this%errors(index)%full_line_pos - spos + LEN(TRIM(predots)) &
          + nspace + 3,0)) // TRIM(posstr))
    ELSE
      IF (charpos > 0 .OR. ALLOCATED(errname)) THEN
        !Report the error location
        ok = this%strings%get('err_report_place', temp)
        CALL errstr_store%format_fill(temp)
        CALL eis_append_string(report, temp)
      END IF
    END IF

    IF (ALLOCATED(context_filename) .OR. context_line_number > 0) THEN
      has_context = .FALSE.
      IF (ALLOCATED(filename)) THEN
        has_context = TRIM(filename) /= TRIM(context_filename)
      ELSE
        has_context = .TRUE.
      END IF
      has_context = has_context .OR. line_number /= context_line_number
    ELSE
      has_context = .FALSE.
    END IF

    IF (has_context) THEN
      ok = this%strings%get('err_proximate_source', temp)
      CALL errstr_store%format_fill(temp)
      CALL eis_append_string(report, temp)
    END IF

    CALL eis_append_string(report, REPEAT("=", 80))
    CALL eis_append_string(report,"")

    IF (ALLOCATED(err_source)) DEALLOCATE(err_source)
    IF (ALLOCATED(errstring)) DEALLOCATE(errstring)
    IF (ALLOCATED(errname)) DEALLOCATE(errname)
    IF (ALLOCATED(errname)) DEALLOCATE(errname)
    IF (ALLOCATED(temp)) DEALLOCATE(temp)

  END SUBROUTINE eeh_get_error_report



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Print the error report to stdout
  !> @param[in] this
  !> @param[in] index
  SUBROUTINE eeh_print_err(this, index)
    CLASS(eis_error_handler), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: index !< Error report index
    CHARACTER(LEN=:), ALLOCATABLE :: report
    INTEGER(eis_error) :: err

    IF (.NOT. this%is_init) CALL this%init(err)

    CALL this%get_error_report(index, report)
    WRITE(*,'(A)') report
    DEALLOCATE(report)

  END SUBROUTINE eeh_print_err

END MODULE eis_error_mod
