MODULE eis_constants

  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE

  INTEGER, PARAMETER :: INT8 = SELECTED_INT_KIND(2) !< Fortran 8 bit integer
  INTEGER, PARAMETER :: INT16 = SELECTED_INT_KIND(4) !< Fortran 16 bit integer
  INTEGER, PARAMETER :: INT32 = SELECTED_INT_KIND(9) !< Fortran 32 bit integer
  INTEGER, PARAMETER :: INT64 = SELECTED_INT_KIND(15) !< Fortran 64 bit integer
  !> Fortran 64 bit real
  INTEGER, PARAMETER :: REAL64 = SELECTED_REAL_KIND(15, 307)
  !> Fortran >64 bit real (nominal 128 bit, 80bit on x86)
  INTEGER, PARAMETER :: REAL128 = SELECTED_REAL_KIND(33, 4931)

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

  ! block type constants
  INTEGER, PARAMETER :: eis_pt_variable = 1 !< Type variable
  INTEGER, PARAMETER :: eis_pt_constant = 2 !< Type constant
  INTEGER, PARAMETER :: eis_pt_operator = 3 !< Type operator
  INTEGER, PARAMETER :: eis_pt_function = 4 !< Type function
  INTEGER, PARAMETER :: eis_pt_parenthesis = 5 !< Type parenthesis
  INTEGER, PARAMETER :: eis_pt_separator = 6 !< Type separator
  INTEGER, PARAMETER :: eis_pt_character = 7 !< Type character
  !> Type stack variable (stored variable)
  INTEGER, PARAMETER :: eis_pt_stored_variable = 8
  INTEGER, PARAMETER :: eis_pt_deferred_variable = 9 !< Type deferred variable
  INTEGER, PARAMETER :: eis_pt_deferred_function = 10 !< Type deferred function
  INTEGER, PARAMETER :: eis_pt_emplaced_variable = 11 !< Type emplaced variable
  INTEGER, PARAMETER :: eis_pt_emplaced_function = 12 !< Type emplaced function
  INTEGER, PARAMETER :: eis_pt_bad = 1024 !< Block is of bad type (invalid)
  INTEGER, PARAMETER :: eis_pt_null = 1025 !< Block is of null (empty) type

  ! Associativity constants
  INTEGER, PARAMETER :: eis_assoc_null = 0 !< No associativity specified
  INTEGER, PARAMETER :: eis_assoc_a = 1 !< Fully associative operator
  INTEGER, PARAMETER :: eis_assoc_la = 2 !< Left associative operator
  INTEGER, PARAMETER :: eis_assoc_ra = 3 !< Right associative operator

  !> Parenthesis is left bracket
  INTEGER, PARAMETER :: eis_paren_left_bracket = 1
  !> Parenthesis is right bracket
  INTEGER, PARAMETER :: eis_paren_right_bracket = 2

END MODULE eis_constants
