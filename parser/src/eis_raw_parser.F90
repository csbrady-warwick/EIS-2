MODULE eis_raw_parser_mod

  USE eis_constants
  USE eis_header
  IMPLICIT NONE

  CONTAINS

  !>@brief
  !>Compare two strings for equality
  !>@param[in] str1 - First string to test for equality
  !>@param[in] str2 - Second string to test for equality
  !>@return strcmp - Logical result for string equality
  FUNCTION strcmp(str1, str2)
    CHARACTER(LEN=*), INTENT(IN) :: str1, str2
    LOGICAL :: strcmp

    strcmp = .FALSE.
    IF (LEN_TRIM(str1) /= LEN_TRIM(str2)) RETURN
    strcmp = TRIM(str1) == TRIM(str2)

  END FUNCTION strcmp



  !> @brief
  !> Turn a string into a real
  !> @param[in] str_in - String to test for being a real
  !> @param[inout] err - Error code for conversion test
  !> result will have the eis_err_bad_value bit set if
  !> str_in cannot be parsed as a real
  !> @return parse_string_as_real - Number that str_in can be parsed to
  FUNCTION parse_string_as_real(str_in, err)

    CHARACTER(*), INTENT(IN) :: str_in
    INTEGER(eis_i8), INTENT(INOUT) :: err
    INTEGER :: f
    REAL(eis_num) :: parse_string_as_real
    REAL(eis_num) :: value

    f = 1
    value = 0.0_eis_num
    READ(unit=str_in, fmt=*, iostat=f) value
    IF (f /= 0) err = IOR(err, eis_err_bad_value)
    parse_string_as_real = value

  END FUNCTION parse_string_as_real



  !> @brief
  !> Turn a string into an integer
  !> @param[in] str_in - String to test for being an integer
  !> @param[inout] err - Error code for conversion test
  !> result will have the eis_err_bad_value bit set if
  !> str_in cannot be parsed as a real
  !> @return parse_string_as_integer - Number that str_in can be parsed to
  FUNCTION parse_string_as_integer(str_in, err)

    CHARACTER(*), INTENT(IN) :: str_in
    INTEGER(eis_i8), INTENT(INOUT) :: err
    INTEGER :: f
    INTEGER(eis_i8) :: parse_string_as_integer
    INTEGER(eis_i8) :: value = 0

    f = 1
    value = 0_eis_i8
    READ(unit=str_in, fmt=*, iostat=f) value
    IF (f /= 0) err = IOR(err, eis_err_bad_value)
    parse_string_as_integer = value

  END FUNCTION parse_string_as_integer

END MODULE eis_raw_parser_mod
