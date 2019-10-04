MODULE eis_header

  USE eis_constants
  USE ISO_C_BINDING

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

END MODULE eis_header
