MODULE eis_header

  USE eis_constants
  USE eis_utils
  USE ISO_C_BINDING
  IMPLICIT NONE

  INTEGER, PRIVATE, PARAMETER :: eis_v1 = 1
  INTEGER, PRIVATE, PARAMETER :: eis_v2 = 0
  INTEGER, PRIVATE, PARAMETER :: eis_v3 = 0

  ABSTRACT INTERFACE
    SUBROUTINE on_error_callback(errcode) BIND(C)
      IMPORT eis_error
      INTEGER(eis_error), VALUE, INTENT(IN) :: errcode
    END SUBROUTINE on_error_callback
  END INTERFACE

  INTEGER(eis_error), PARAMETER :: eis_err_parser = 2**0 !< Error in parser
  !> Error in simplify
  INTEGER(eis_error), PARAMETER :: eis_err_simplifier = 2**1
  INTEGER(eis_error), PARAMETER :: eis_err_emplacer = 2**2 !< Error in emplace
  INTEGER(eis_error), PARAMETER :: eis_err_evaluator = 2**3 !< Error in evaluate
  INTEGER(eis_error), PARAMETER :: eis_err_file = 2**4 !< Error in file
  !> Error in deck file
  INTEGER(eis_error), PARAMETER :: eis_err_deck_file = 2**5
  !> Error in deck definition
  INTEGER(eis_error), PARAMETER :: eis_err_deck_definition = 2**6
  !> Error in deck parser
  INTEGER(eis_error), PARAMETER :: eis_err_deck_parser = 2**7
  INTEGER(eis_error), PARAMETER :: eis_err_not_found = 2**8 !< Name not found
  !> Malformed mathematical expression
  INTEGER(eis_error), PARAMETER :: eis_err_malformed = 2**9
  !> Incorrect number of parameters specified
  INTEGER(eis_error), PARAMETER :: eis_err_wrong_parameters = 2**10
  !> Specified expression is mathematically invalid (log of 0 etc.)
  INTEGER(eis_error), PARAMETER :: eis_err_maths_domain = 2**11
  !> Value is nonsensical in non-mathematical way
  INTEGER(eis_error), PARAMETER :: eis_err_bad_value = 2**12
  !> Stack has deferred elements that cannot be resolved
  INTEGER(eis_error), PARAMETER :: eis_err_has_deferred = 2**13
  !> Stack has emplaced elements that have not been resolved
  INTEGER(eis_error), PARAMETER :: eis_err_has_emplaced = 2**14
  !> Stack makes use of the "where" construct but the necessary "no-op"
  !> value is not tested when the stack is evaluated
  INTEGER(eis_error), PARAMETER :: eis_err_where = 2**15
  !> Attempted to subscript a constant as though it was a function
  INTEGER(eis_error), PARAMETER :: eis_err_bracketed_constant = 2**16
  !> Extra bracket in expression
  INTEGER(eis_error), PARAMETER :: eis_err_extra_bracket = 2**17
  !> Stack specified through interoperable interface is invalid
  INTEGER(eis_error), PARAMETER :: eis_err_bad_stack = 2**18
  !> Stack returned more results through interoperable interface than expected
  INTEGER(eis_error), PARAMETER :: eis_err_extra_results = 2**19
  !> String handler couldn't find a LUN when loading a file
  INTEGER(eis_error), PARAMETER :: eis_err_no_luns = 2**20
  !> String handler couldn't find a specified file
  INTEGER(eis_error), PARAMETER :: eis_err_no_file = 2**21
  !> String handler was given an invalid serialisation string
  INTEGER(eis_error), PARAMETER :: eis_err_malformed_file = 2**22
  !> Deck handler found an unmatched start and end block description
  INTEGER(eis_error), PARAMETER :: eis_err_mismatched_begin_end = 2**23
  !> Deck handler found a deck block that was deeper than permitted
  INTEGER(eis_error), PARAMETER :: eis_err_deck_too_deep = 2**24
  !> Deck handler found an unpermitted empty block
  INTEGER(eis_error), PARAMETER :: eis_err_deck_empty_block = 2**25
  !> Deck handler found an unpermitted key in the root block
  INTEGER(eis_error), PARAMETER :: eis_err_root_keys = 2**26
  !> Deck handler tried to use an empty definition to parse a deck
  INTEGER(eis_error), PARAMETER :: eis_bad_deck_definition = 2**27
  !> Deck handler found an unknown block in a deck
  INTEGER(eis_error), PARAMETER :: eis_err_unknown_block = 2**28
  !> Deck handler found an unknown key in a deck
  INTEGER(eis_error), PARAMETER :: eis_err_unknown_key = 2**29
  !> Deck handler found a key that it was unable to handle
  INTEGER(eis_error), PARAMETER :: eis_err_bad_key = 2**30
  !> Fatal error was reported by the host code
  INTEGER(eis_error), PARAMETER :: eis_err_host = 2_eis_error**31_eis_error

  CONTAINS

  !> @brief
  !> Returns the version as a set of integers
  !> Uses accessor function rather than variables
  !> against future version wanting to have a more complex version system
  !> (backward compatability modes etc.)
  !> @param[out] v1
  !> @param[out] v2
  !> @param[out] v3
  SUBROUTINE eis_get_version_numbers(v1, v2, v3)
    INTEGER, INTENT(OUT), OPTIONAL :: v1 !< First digit
    INTEGER, INTENT(OUT), OPTIONAL :: v2 !< Second digit
    INTEGER, INTENT(OUT), OPTIONAL :: v3 !< Third digit

    IF (PRESENT(v1)) v1 = eis_v1
    IF (PRESENT(v2)) v2 = eis_v2
    IF (PRESENT(v3)) v3 = eis_v3
  END SUBROUTINE eis_get_version_numbers



  !> @brief
  !> Returns the version as a string
  !> @param[out] str
  !> @param[in] level
  SUBROUTINE eis_get_version_string(str, level)
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: str
    INTEGER, INTENT(IN), OPTIONAL :: level
    INTEGER :: ilevel, i
    INTEGER :: vnums(3)
    CHARACTER(LEN=:), ALLOCATABLE :: v

    ilevel = 3
    IF (PRESENT(level)) ilevel = level
    IF (ilevel < 1) ilevel = 1
    IF (ilevel > 3) ilevel = 3
    CALL eis_get_version_numbers(vnums(1), vnums(2), vnums(3))
    DO i = 1, ilevel
      CALL eis_integer_as_string(vnums(i), v)
      CALL eis_append_string(str, v, newline = .FALSE.)
      IF (i /= ilevel) CALL eis_append_string(str, '.', newline = .FALSE.)
    END DO

  END SUBROUTINE eis_get_version_string



  !> @brief
  !> Returns a string detailing the version of EIS being used
  !> If needed other copyright information will be returned here
  !> @param[out] str
  !> @param[in] level
  SUBROUTINE eis_get_version_report(str, level)
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: str
    INTEGER, INTENT(IN), OPTIONAL :: level
    CHARACTER(LEN=:), ALLOCATABLE :: vstring

    CALL eis_get_version_string(vstring, level)
    CALL eis_append_string(str, 'Using EIS-2 parser version ' // vstring)

  END SUBROUTINE eis_get_version_report

END MODULE eis_header
