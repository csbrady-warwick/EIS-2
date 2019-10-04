MODULE eis_deck_header

  USE eis_constants
  USE eis_header

  INTEGER(eis_status), PARAMETER :: eis_status_directive = 2**1
  INTEGER(eis_status), PARAMETER :: eis_status_not_handled = 2**2
  INTEGER(eis_status), PARAMETER :: eis_status_retain_stack = 2**3

END MODULE eis_deck_header
