MODULE eis_constants

  IMPLICIT NONE

  INTEGER, PARAMETER :: INT8 = SELECTED_INT_KIND(2)
  INTEGER, PARAMETER :: INT16 = SELECTED_INT_KIND(4)
  INTEGER, PARAMETER :: INT32 = SELECTED_INT_KIND(9)
  INTEGER, PARAMETER :: INT64 = SELECTED_INT_KIND(15)
  INTEGER, PARAMETER :: REAL32 = SELECTED_REAL_KIND(6, 37)
  INTEGER, PARAMETER :: REAL64 = SELECTED_REAL_KIND(15, 307)
  INTEGER, PARAMETER :: REAL128 = SELECTED_REAL_KIND(33, 4931)

  INTEGER, PARAMETER :: eis_num = REAL64
  INTEGER, PARAMETER :: eis_i4 = INT32
  INTEGER, PARAMETER :: eis_i8 = INT64

  REAL(eis_num), PARAMETER :: eis_tiny = TINY(1.0_eis_num)
  REAL(eis_num), PARAMETER :: eis_huge = HUGE(1.0_eis_num)


  ! block type constants
  INTEGER, PARAMETER :: c_pt_variable = 1
  INTEGER, PARAMETER :: c_pt_constant = 2
  INTEGER, PARAMETER :: c_pt_operator = 3
  INTEGER, PARAMETER :: c_pt_function = 4
  INTEGER, PARAMETER :: c_pt_parenthesis = 5
  INTEGER, PARAMETER :: c_pt_separator = 6
  INTEGER, PARAMETER :: c_pt_character = 7
  INTEGER, PARAMETER :: c_pt_stored_variable = 8
  INTEGER, PARAMETER :: c_pt_stored_function = 9
  INTEGER, PARAMETER :: c_pt_bad = 1024
  INTEGER, PARAMETER :: c_pt_null = 1025

  ! Associativity constants
  INTEGER, PARAMETER :: c_assoc_null = 0
  INTEGER, PARAMETER :: c_assoc_a = 1
  INTEGER, PARAMETER :: c_assoc_la = 2
  INTEGER, PARAMETER :: c_assoc_ra = 3
  INTEGER, PARAMETER :: c_num_ops = 13

  INTEGER, DIMENSION(c_num_ops), PARAMETER :: &
      opcode_precedence = (/2, 2, 3, 3, 4, 4, 1, 1, 1, 0, 0, 4, 4/)
  INTEGER, DIMENSION(c_num_ops), PARAMETER :: &
      opcode_assoc = (/c_assoc_a, c_assoc_la, c_assoc_a, c_assoc_la, &
          c_assoc_ra, c_assoc_ra, c_assoc_la, c_assoc_la, c_assoc_la, &
          c_assoc_la, c_assoc_la, c_assoc_ra, c_assoc_ra/)

  INTEGER, PARAMETER :: c_paren_left_bracket = 1
  INTEGER, PARAMETER :: c_paren_right_bracket = 2

END MODULE eis_constants
