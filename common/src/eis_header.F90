MODULE eis_header

  USE eis_constants
  USE ISO_C_BINDING

  INTEGER(eis_error), PARAMETER :: eis_err_none = 0 !< No error
  INTEGER(eis_error), PARAMETER :: eis_err_parser = 2**0 !< Error in parser
  !> Error in simplify
  INTEGER(eis_error), PARAMETER :: eis_err_simplifier = 2**1
  INTEGER(eis_error), PARAMETER :: eis_err_emplacer = 2**2 !< Error in emplace
  INTEGER(eis_error), PARAMETER :: eis_err_evaluator = 2**3 !< Error in evaluate
  INTEGER(eis_error), PARAMETER :: eis_err_not_found = 2**4 !< Name not found
  !> Malformed mathematical expression
  INTEGER(eis_error), PARAMETER :: eis_err_malformed = 2**5
  !> Incorrect number of parameters specified
  INTEGER(eis_error), PARAMETER :: eis_err_wrong_parameters = 2**6
  !> Specified expression is mathematically invalid (log of 0 etc.)
  INTEGER(eis_error), PARAMETER :: eis_err_maths_domain = 2**7
  !> Value is nonsensical in non-mathematical way
  INTEGER(eis_error), PARAMETER :: eis_err_bad_value = 2**8
  !> Stack has deferred elements that cannot be resolved
  INTEGER(eis_error), PARAMETER :: eis_err_has_deferred = 2**9
  !> Stack has emplaced elements that have not been resolved
  INTEGER(eis_error), PARAMETER :: eis_err_has_emplaced = 2**10
  !> Stack makes use of the "where" construct but the necessary "no-op"
  !> value is not tested when the stack is evaluated
  INTEGER(eis_error), PARAMETER :: eis_err_where = 2**11
  !> Attempted to subscript a constant as though it was a function
  INTEGER(eis_error), PARAMETER :: eis_err_bracketed_constant = 2**12
  !> Extra bracket in expression
  INTEGER(eis_error), PARAMETER :: eis_err_extra_bracket = 2**13
  !> Stack specified through interoperable interface is invalid
  INTEGER(eis_error), PARAMETER :: eis_err_bad_stack = 2**14
  !> Stack returned more results through interoperable interface than expected
  INTEGER(eis_error), PARAMETER :: eis_err_extra_results = 2**15
  !> String handler couldn't find a LUN when loading a file
  INTEGER(eis_error), PARAMETER :: eis_err_no_luns = 2**16 
  !> String handler couldn't find a specified file
  INTEGER(eis_error), PARAMETER :: eis_err_no_file = 2**17

END MODULE eis_header
