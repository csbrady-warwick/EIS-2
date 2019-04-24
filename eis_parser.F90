!Copyright (c) 2019, C.S.Brady
!All rights reserved.

!Redistribution and use in source and binary forms, with or without
!modification, are permitted provided that the following conditions are met:
!    * Redistributions of source code must retain the above copyright
!      notice, this list of conditions and the following disclaimer.
!    * Redistributions in binary form must reproduce the above copyright
!      notice, this list of conditions and the following disclaimer in the
!      documentation and/or other materials provided with the distribution.
!    * Neither the name of the <organization> nor the
!      names of its contributors may be used to endorse or promote products
!      derived from this software without specific prior written permission.

!THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
!ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
!WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
!DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
!DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
!(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
!LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
!ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
!(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
!SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

MODULE eis_parser

  USE eis_header
  USE eis_raw_parser_mod
  USE eis_function_registry_mod
  IMPLICIT NONE

  INTEGER, PARAMETER :: c_char_numeric = 1
  INTEGER, PARAMETER :: c_char_alpha = 2
  INTEGER, PARAMETER :: c_char_delimiter = 3
  INTEGER, PARAMETER :: c_char_space = 4
  INTEGER, PARAMETER :: c_char_opcode = 5
  INTEGER, PARAMETER :: c_char_unknown = 1024

  INTEGER :: last_block_type

  PRIVATE
  PUBLIC :: tokenize

CONTAINS

  FUNCTION char_type(chr)

    CHARACTER, INTENT(IN) :: chr
    INTEGER :: char_type

    char_type = c_char_unknown

    IF (chr == ' ') THEN
      char_type = c_char_space
    ELSE IF (chr >= '0' .AND. chr <= '9' .OR. chr == '.') THEN
      char_type = c_char_numeric
    ELSE IF ((chr >= 'A' .AND. chr <= 'Z') &
        .OR. (chr >= 'a' .AND. chr <= 'z') .OR. chr == '_') THEN
      char_type = c_char_alpha
    ELSE IF (chr == '(' .OR. chr == ')' .OR. chr == ',') THEN
      char_type = c_char_delimiter
    ! 92 is the ASCII code for backslash
    ELSE IF (chr == '+' .OR. chr == '-' .OR. ICHAR(chr) == 92 &
        .OR. chr == '/' .OR. chr == '*' .OR. chr == '^') THEN
      char_type = c_char_opcode
    END IF

  END FUNCTION char_type



  SUBROUTINE load_block(name, iblock)

    CHARACTER(LEN=*), INTENT(IN) :: name
    TYPE(eis_stack_element), INTENT(OUT) :: iblock
    INTEGER :: work
    REAL(eis_num) :: value
    PROCEDURE(parser_eval_fn), POINTER :: fn_ptr

    iblock%ptype = c_pt_bad
    iblock%value = 0
    iblock%numerical_data = 0.0_eis_num
#ifdef PARSER_DEBUG
    iblock%text = TRIM(name)
#endif
    work = 0

    IF (LEN(TRIM(name)) == 0) THEN
      iblock%ptype = c_pt_null
      iblock%value = 0
      iblock%numerical_data = 0.0_eis_num
      RETURN
    END IF

    value = parse_string_as_real(name, work)
    IF (IAND(work, eis_err_bad_value) == 0) THEN
      ! block is a simple variable
      iblock%ptype = c_pt_variable
      iblock%value = 0
      iblock%numerical_data = value
    END IF

  END SUBROUTINE load_block



  FUNCTION as_parenthesis(name)

    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER :: as_parenthesis

    as_parenthesis = 0

    IF (strcmp(name, '(')) THEN
      as_parenthesis = c_paren_left_bracket

    ELSE IF (strcmp(name, ')')) THEN
      as_parenthesis = c_paren_right_bracket
    END IF

  END FUNCTION as_parenthesis



  SUBROUTINE tokenize(expression, output, err, ispecies)

    CHARACTER(LEN=*), INTENT(IN) :: expression
    TYPE(eis_stack), INTENT(INOUT) :: output
    INTEGER, INTENT(INOUT) :: err
    INTEGER, INTENT(IN), OPTIONAL :: ispecies
    LOGICAL :: maybe_e

    CHARACTER(LEN=500) :: current
    INTEGER :: current_type, current_pointer, i, ptype

    TYPE(eis_stack) :: stack
    TYPE(eis_stack_element) :: iblock

    CALL initialise_stack(stack)

    current(:) = ' '
    current(1:1) = expression(1:1)
    current_pointer = 2
    current_type = char_type(expression(1:1))
    maybe_e = .FALSE.

    err = eis_err_none

    last_block_type = c_pt_null

    DO i = 2, LEN(TRIM(expression))
      ptype = char_type(expression(i:i))
      ! This is a bit of a hack.
      ! Allow numbers to follow letters in an expression *except* in the
      ! special case of a single 'e' character, to allow 10.0e5, etc.
      IF (ptype == current_type .AND. ptype /= c_char_delimiter &
          .OR. (ptype == c_char_numeric .AND. current_type == c_char_alpha &
          .AND. .NOT. strcmp(current, 'e'))) THEN
        current(current_pointer:current_pointer) = expression(i:i)
        current_pointer = current_pointer+1
      ELSE IF (strcmp(current, 'e') .AND. .NOT.maybe_e) THEN
        ! Only interpret "e" as the Euler number if it is both preceded and
        ! followed by a number
        current(current_pointer:current_pointer) = expression(i:i)
        current_pointer = current_pointer+1
      ELSE
#ifndef RPN_DECK
        CALL tokenize_subexpression_infix(current, iblock, stack, output, err)
#else
        CALL tokenize_subexpression_rpn(current, iblock, stack, output, err)
#endif
        IF (err /= eis_err_none) RETURN
        current(:) = ' '
        current_pointer = 2
        current(1:1) = expression(i:i)
        current_type = ptype
        maybe_e = iblock%ptype == c_pt_variable
      END IF
    END DO

#ifndef RPN_DECK
    CALL tokenize_subexpression_infix(current, iblock, stack, output, err)
#else
    CALL tokenize_subexpression_rpn(current, iblock, stack, output, err)
#endif
    IF (err /= eis_err_none) RETURN

    CALL fixup_species_functions(current, stack, output, ispecies)

    DO i = 1, stack%stack_point
      CALL pop_to_stack(stack, output)
    END DO
    CALL deallocate_stack(stack)

  END SUBROUTINE tokenize



  SUBROUTINE tokenize_subexpression_infix(current, iblock, stack, output, err)

    ! This subroutine tokenizes input in normal infix maths notation
    ! It uses Dijkstra's shunting yard algorithm to convert to RPN

    CHARACTER(LEN=*), INTENT(IN) :: current
    TYPE(eis_stack_element), INTENT(INOUT) :: iblock
    TYPE(eis_stack), INTENT(INOUT) :: stack, output
    INTEGER, INTENT(INOUT) :: err
    TYPE(eis_stack_element) :: block2
    INTEGER :: ipoint, io, iu

    IF (ICHAR(current(1:1)) == 0) RETURN

    ! Populate the block
    CALL load_block(current, iblock)
#ifdef PARSER_DEBUG
    iblock%text = TRIM(current)
#endif
    IF (iblock%ptype == c_pt_bad) THEN
      err = eis_err_bad_value
      CALL deallocate_stack(stack)
      RETURN
    END IF

    IF (iblock%ptype /= c_pt_parenthesis &
        .AND. iblock%ptype /= c_pt_null) THEN
      last_block_type = iblock%ptype
    END IF

    IF (iblock%ptype == c_pt_deck_constant) THEN
      !Do nothing
    ELSE IF (iblock%ptype == c_pt_variable &
        .OR. iblock%ptype == c_pt_constant &
        .OR. iblock%ptype == c_pt_default_constant &
        .OR. iblock%ptype == c_pt_species &
        .OR. iblock%ptype == c_pt_subset) THEN
      CALL push_to_stack(output, iblock)

    ELSE IF (iblock%ptype == c_pt_parenthesis) THEN
      IF (iblock%value == c_paren_left_bracket) THEN
        CALL push_to_stack(stack, iblock)
      ELSE
        DO
          CALL stack_snoop(stack, block2, 0)
          IF (block2%ptype == c_pt_parenthesis &
              .AND. block2%value == c_paren_left_bracket) THEN
            CALL pop_to_null(stack)
            ! If stack isn't empty then check for function
            IF (stack%stack_point /= 0) THEN
              CALL stack_snoop(stack, block2, 0)
              IF (block2%ptype == c_pt_function) THEN
                CALL add_function_to_stack(block2%value, stack, output)
              END IF
            END IF
            EXIT
          ELSE
            CALL pop_to_stack(stack, output)
          END IF
        END DO
      END IF

    ELSE IF (iblock%ptype == c_pt_function) THEN
      ! Just push functions straight onto the stack
      CALL push_to_stack(stack, iblock)

    ELSE IF (iblock%ptype == c_pt_separator) THEN
      DO
        CALL stack_snoop(stack, block2, 0)
        IF (block2%ptype /= c_pt_parenthesis) THEN
          CALL pop_to_stack(stack, output)
        ELSE
          IF (block2%value /= c_paren_left_bracket) THEN
            PRINT *, 'Bad function expression'
            STOP
          END IF
          EXIT
        END IF
      END DO

    ELSE IF (iblock%ptype == c_pt_operator) THEN
      DO
        IF (stack%stack_point == 0) THEN
          ! stack is empty, so just push operator onto stack and
          ! leave loop
          CALL push_to_stack(stack, iblock)
          EXIT
        END IF
        ! stack is not empty so check precedence etc.
        CALL stack_snoop(stack, block2, 0)
        IF (block2%ptype /= c_pt_operator) THEN
          ! Previous block is not an operator so push current operator
          ! to stack and leave loop
          CALL push_to_stack(stack, iblock)
          EXIT
        ELSE
          IF (opcode_assoc(iblock%value) == c_assoc_la &
              .OR. opcode_assoc(iblock%value) == c_assoc_a) THEN
            ! Operator is full associative or left associative
            IF (opcode_precedence(iblock%value) &
                <= opcode_precedence(block2%value)) THEN
              CALL pop_to_stack(stack, output)
              CYCLE
            ELSE
              CALL push_to_stack(stack, iblock)
              EXIT
            END IF
          ELSE
            IF (opcode_precedence(iblock%value) &
                < opcode_precedence(block2%value)) THEN
              CALL pop_to_stack(stack, output)
              CYCLE
            ELSE
              CALL push_to_stack(stack, iblock)
              EXIT
            END IF
          END IF
        END IF
      END DO
    END IF

  END SUBROUTINE tokenize_subexpression_infix



  SUBROUTINE display_tokens(token_list)

    TYPE(eis_stack), INTENT(IN) :: token_list
    INTEGER :: i

      DO i = 1, token_list%stack_point
        PRINT *, 'Type', token_list%entries(i)%ptype
        PRINT *, 'Data', token_list%entries(i)%value
        PRINT *, 'NumData', token_list%entries(i)%numerical_data
#ifdef PARSER_DEBUG
        PRINT *, 'Text :', TRIM(token_list%entries(i)%text)
#endif
        PRINT *, '---------------'
      END DO

  END SUBROUTINE display_tokens

END MODULE eis_parser
