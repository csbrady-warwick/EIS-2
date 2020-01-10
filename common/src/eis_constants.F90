MODULE eis_constants

  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE

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

  !> No status specified
  INTEGER(eis_status), PARAMETER :: eis_status_none = 0
  !> Terminate operations status
  INTEGER(eis_status), PARAMETER :: eis_status_terminate = 2**0
  !Interoperable stack should be retained
  INTEGER(eis_status), PARAMETER :: eis_status_retain_stack = 2**1

  !> No error specified
  INTEGER(eis_error), PARAMETER :: eis_err_none = 0

END MODULE eis_constants
