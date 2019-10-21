MODULE eis_constants

  USE, INTRINSIC :: ISO_C_BINDING
#ifdef F2018
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : ATOMIC_INT_KIND
#endif
  IMPLICIT NONE

  INTEGER, PRIVATE, PARAMETER :: eis_v1 = 1
  INTEGER, PRIVATE, PARAMETER :: eis_v2 = 0
  INTEGER, PRIVATE, PARAMETER :: eis_v3 = 0

  INTEGER, PARAMETER :: INT8 = SELECTED_INT_KIND(2) !< Fortran 8 bit integer
  INTEGER, PARAMETER :: INT16 = SELECTED_INT_KIND(4) !< Fortran 16 bit integer
  INTEGER, PARAMETER :: INT32 = SELECTED_INT_KIND(9) !< Fortran 32 bit integer
  INTEGER, PARAMETER :: INT64 = SELECTED_INT_KIND(15) !< Fortran 64 bit integer
  !> Fortran 32 bit real
  INTEGER, PARAMETER :: REAL32 = SELECTED_REAL_KIND(6, 37)
  !> Fortran 64 bit real
  INTEGER, PARAMETER :: REAL64 = SELECTED_REAL_KIND(15, 307)
  !> Fortran >64 bit real (nominal 128 bit, 80bit on x86)
  INTEGER, PARAMETER :: REAL128 = SELECTED_REAL_KIND(33, 4931)

  !> Kind for UIDs from the UID generator
  INTEGER, PARAMETER :: uid_kind = INT32

#ifdef UNICODE
  !> Unicode UCS4 string kind
  INTEGER, PARAMETER :: UCS4 = SELECTED_CHAR_KIND('ISO_10646')
#else
  !> Mimic UCS4 in ASCII
  INTEGER, PARAMETER :: UCS4 = SELECTED_CHAR_KIND('ASCII')
#endif
  !> ASCII character kind
  INTEGER, PARAMETER :: ASCII = SELECTED_CHAR_KIND('ASCII')

  !> Number of default interoperable parsers.
  !> Number will grow if more are needed
  INTEGER, PARAMETER :: n_parsers_default = 4
  !> Number of default interoperable stacks
  !> Number will grow if more are needed
  INTEGER, PARAMETER :: n_stacks_default = 32


  !> kind parameter for real values used in EIS
  INTEGER, PARAMETER :: eis_num = C_DOUBLE
  !> kind parameter for real values in C. Equvalent to eis_num
  INTEGER, PARAMETER :: eis_num_c = C_DOUBLE
  !> kind parameter for 4 byte integers used in EIS
  INTEGER, PARAMETER :: eis_i4 = C_INT
  !> kind parameter for 8 byte integers used in EIS
  INTEGER, PARAMETER :: eis_i8 = C_LONG_LONG
  !> kind parameter for the integers used as error codes in EIS
  INTEGER, PARAMETER :: eis_error = eis_i8
  !> kind parameter for C equivalent type for eis_error
  INTEGER, PARAMETER :: eis_error_c = C_LONG_LONG
  !> kind parameter for the integers used as status codes in EIS
  INTEGER, PARAMETER :: eis_status = eis_i8
  !> kind parameter for C equivalent for eis_status
  INTEGER, PARAMETER :: eis_status_c = C_LONG_LONG
  !> kind parameter for the integers used for user bitmasks in EIS
  INTEGER, PARAMETER :: eis_bitmask = eis_i8
  !> kind parameter for C equivalent to eis_bitmask
  INTEGER, PARAMETER :: eis_bitmask_c = C_LONG_LONG

  !> TINY value for variables of kind eis_num
  REAL(eis_num), PARAMETER :: eis_tiny = TINY(1.0_eis_num)
  !> HUGE value for variable of kind eis_num
  REAL(eis_num), PARAMETER :: eis_huge = HUGE(1.0_eis_num)

  !> No error condition
  INTEGER(eis_error), PARAMETER :: eis_err_none = 0
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
  !> Attempted to subscript a constant as though it was a function
  INTEGER(eis_error), PARAMETER :: eis_err_bracketed_constant = 2**15
  !> Extra bracket in expression
  INTEGER(eis_error), PARAMETER :: eis_err_extra_bracket = 2**16
  !> Stack specified through interoperable interface is invalid
  INTEGER(eis_error), PARAMETER :: eis_err_bad_stack = 2**17
  !> Stack returned more results through interoperable interface than expected
  INTEGER(eis_error), PARAMETER :: eis_err_extra_results = 2**18
  !> String handler couldn't find a LUN when loading a file
  INTEGER(eis_error), PARAMETER :: eis_err_no_luns = 2**19
  !> String handler couldn't find a specified file
  INTEGER(eis_error), PARAMETER :: eis_err_no_file = 2**20
  !> String handler was given an invalid serialisation string
  INTEGER(eis_error), PARAMETER :: eis_err_malformed_file = 2**21
  !> Deck handler found an unmatched start and end block description
  INTEGER(eis_error), PARAMETER :: eis_err_mismatched_begin_end = 2**22
  !> Deck handler found a deck block that was deeper than permitted
  INTEGER(eis_error), PARAMETER :: eis_err_deck_too_deep = 2**23
  !> Deck handler found an unpermitted empty block
  INTEGER(eis_error), PARAMETER :: eis_err_deck_empty_block = 2**24
  !> Deck handler found an unpermitted key in the root block
  INTEGER(eis_error), PARAMETER :: eis_err_root_keys = 2**25
  !> Deck handler tried to use an empty definition to parse a deck
  INTEGER(eis_error), PARAMETER :: eis_err_bad_deck_definition = 2**26
  !> Deck handler found an unknown block in a deck
  INTEGER(eis_error), PARAMETER :: eis_err_unknown_block = 2**27
  !> Deck handler found an unknown key in a deck
  INTEGER(eis_error), PARAMETER :: eis_err_unknown_key = 2**28
  !> Deck handler found a key that it was unable to handle
  INTEGER(eis_error), PARAMETER :: eis_err_bad_key = 2**29
  !> Fatal error was reported by the host code
  INTEGER(eis_error), PARAMETER :: eis_err_host = 2**30
  !> Function had a text parameter that should not have had a text parameter
  INTEGER(eis_error), PARAMETER :: eis_err_text = 2_eis_error**31_eis_error
  !> Trying to use a function that wants an interoperable parser but
  !> the actual parser is not interoperable
  INTEGER(eis_error), PARAMETER :: eis_err_interop = 2_eis_error**32_eis_error
  !> Parameter was valid mathematically but was out of range in some other sense
  INTEGER(eis_error), PARAMETER :: eis_err_out_of_range &
      = 2_eis_error**33_eis_error
  !> Stack variable that returns more than one parameter was used in a maths
  !> expression
  INTEGER(eis_error), PARAMETER :: eis_err_stack_params &
      = 2_eis_error**34_eis_error
  !> Trying to perform an operation that is invalid after minification
  INTEGER(eis_error), PARAMETER :: eis_err_invalid_minify &
      = 2_eis_error**35_eis_error

  !> No status specified
  INTEGER(eis_status), PARAMETER :: eis_status_none = 0
  !> Terminate operations status
  INTEGER(eis_status), PARAMETER :: eis_status_terminate = 2**0
  !Interoperable stack should be retained
  INTEGER(eis_status), PARAMETER :: eis_status_retain_stack = 2**1
  !> This element in a stack should not be simplified through
  INTEGER(eis_status), PARAMETER :: eis_status_no_simplify = 2**2
  !> This element in a stack is an emplaced value that should not be emplaced
  !> yet
  INTEGER(eis_status), PARAMETER :: eis_status_no_emplace = 2**3
  !> This deck key is a directive (of the form key:value rather than
  !> key = value)
  INTEGER(eis_status), PARAMETER :: eis_status_directive = 2**4
  !> This deck key or block handling function has decided that it cannot
  !> handle this key or block
  INTEGER(eis_status), PARAMETER :: eis_status_not_handled = 2**5

  ABSTRACT INTERFACE
    SUBROUTINE on_error_callback(errcode) BIND(C)
      IMPORT eis_error
      INTEGER(eis_error), VALUE, INTENT(IN) :: errcode
    END SUBROUTINE on_error_callback
  END INTERFACE

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

END MODULE eis_constants
