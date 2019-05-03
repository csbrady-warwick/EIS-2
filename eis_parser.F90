!Copyright (c) 2019, C.S.Brady
!All rights reserved.

!Redistribution and use in source and binary forms, with or without
!modification, are permitted provided that the following conditions are met:
!    * Redistributions of source code must retain the above copyright
!      notice, this list of conditions and the following disclaimer.
!    * Redistributions in binary form must reproduce the above copyright
!      notice, this list of conditions and the following disclaimer in the
!      documentation and/or other materials provided with the distribution.
!    * Neither the name of the University of Warwick nor the
!      names of its contributors may be used to endorse or promote products
!      derived from this software without specific prior written permission.

!THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
!ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
!WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
!DISCLAIMED. IN NO EVENT SHALL THE UNIVERSITY OF WARWICK BE LIABLE FOR ANY
!DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
!(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
!LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
!ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
!(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
!SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

MODULE eis_parser_mod

  USE, INTRINSIC :: ISO_C_BINDING
  USE eis_header
  USE eis_raw_parser_mod
  USE eis_function_registry_mod
  USE eis_core_functions_mod
  USE eis_stack_mod
  USE eis_tree_mod
  USE eis_eval_stack_mod
  IMPLICIT NONE

  INTEGER, PARAMETER :: c_char_numeric = 1
  INTEGER, PARAMETER :: c_char_alpha = 2
  INTEGER, PARAMETER :: c_char_delimiter = 3
  INTEGER, PARAMETER :: c_char_space = 4
  INTEGER, PARAMETER :: c_char_opcode = 5
  INTEGER, PARAMETER :: c_char_unknown = 1024

  TYPE(eis_registry), SAVE :: global_registry
  LOGICAL, SAVE :: global_setup = .FALSE.

  TYPE :: eis_parser

    PRIVATE
    TYPE(eis_registry) :: registry
    TYPE(eis_eval_stack) :: evaluator
    INTEGER :: last_block_type
    LOGICAL :: is_init = .FALSE.
    LOGICAL :: should_simplify = .TRUE.
    LOGICAL :: should_minify = .TRUE.
    TYPE(eis_stack) :: stack, brackets
    TYPE(eis_stack), POINTER :: output
    CONTAINS
    PROCEDURE :: init => eip_init
    PROCEDURE :: load_block => eip_load_block
    PROCEDURE :: tokenize_subexpression_infix &
        => eip_tokenize_subexpression_infix
    PROCEDURE :: add_stack_variable_stack &
        => eip_add_stack_variable_stack
    PROCEDURE :: add_stack_variable_string &
        => eip_add_stack_variable_string
    PROCEDURE, PUBLIC :: add_function => eip_add_function
    PROCEDURE, PUBLIC :: add_constant => eip_add_constant
    PROCEDURE, PUBLIC :: add_variable => eip_add_variable
    GENERIC, PUBLIC :: add_stack_variable => add_stack_variable_stack, &
        add_stack_variable_string
    PROCEDURE, PUBLIC :: tokenize => eip_tokenize
    PROCEDURE, PUBLIC :: evaluate => eip_evaluate
    PROCEDURE, PUBLIC :: simplify => eip_simplify

  END TYPE eis_parser

  PRIVATE
  PUBLIC :: eis_parser

CONTAINS

  SUBROUTINE eip_init(this, should_simplify, should_minify)

    CLASS(eis_parser) :: this
    LOGICAL, INTENT(IN), OPTIONAL :: should_simplify, should_minify
    INTEGER(eis_error) :: err

    IF (PRESENT(should_simplify)) this%should_simplify = should_simplify
    IF (PRESENT(should_minify)) this%should_minify = should_minify

    this%is_init = .TRUE.

    IF (.NOT. global_setup) THEN
      global_setup = .TRUE.
      CALL global_registry%add_operator('+', eis_uplus, c_assoc_ra, 4, err, &
        unary = .TRUE.)
      CALL global_registry%add_operator('-', eis_uminus, c_assoc_ra, 4, err, &
          unary = .TRUE.)
      CALL global_registry%add_operator('+', eis_bplus, c_assoc_a, 2, err)
      CALL global_registry%add_operator('-', eis_bminus, c_assoc_la, 2, err)
      CALL global_registry%add_operator('*', eis_times, c_assoc_a, 3, err)
      CALL global_registry%add_operator('/', eis_divide, c_assoc_la, 3, err)
      CALL global_registry%add_operator('^', eis_pow, c_assoc_ra, 4, err)
      CALL global_registry%add_operator('e', eis_expo, c_assoc_la, 4, err)
      CALL global_registry%add_operator('lt', eis_lt, c_assoc_la, 1, err)
      CALL global_registry%add_operator('le', eis_le, c_assoc_la, 1, err)
      CALL global_registry%add_operator('gt', eis_gt, c_assoc_la, 1, err)
      CALL global_registry%add_operator('ge', eis_ge, c_assoc_la, 1, err)
      CALL global_registry%add_operator('eq', eis_eq, c_assoc_la, 1, err)
      CALL global_registry%add_operator('and', eis_and, c_assoc_la, 0, err)
      CALL global_registry%add_operator('or', eis_or, c_assoc_la, 0, err)

      CALL global_registry%add_constant('pi', &
          3.141592653589793238462643383279503_eis_num, err)
      CALL global_registry%add_constant('x', 1.0_eis_num, err, &
          can_simplify = .FALSE.)
      CALL global_registry%add_constant('y', 1.0_eis_num, err, &
          can_simplify = .FALSE.)


      CALL global_registry%add_function('abs', eis_abs, 1, err)
      CALL global_registry%add_function('floor', eis_floor, 1, err)
      CALL global_registry%add_function('ceil', eis_ceil, 1, err)
      CALL global_registry%add_function('ceiling', eis_ceil, 1, err)
      CALL global_registry%add_function('nint', eis_nint, 1, err)
      CALL global_registry%add_function('trunc', eis_aint, 1, err)
      CALL global_registry%add_function('truncate', eis_aint, 1, err)
      CALL global_registry%add_function('aint', eis_aint, 1, err)
      CALL global_registry%add_function('sqrt', eis_sqrt, 1, err)
      CALL global_registry%add_function('sin', eis_sin, 1, err)
      CALL global_registry%add_function('cos', eis_cos, 1, err)
      CALL global_registry%add_function('tan', eis_tan, 1, err)
      CALL global_registry%add_function('asin', eis_asin, 1, err)
      CALL global_registry%add_function('acos', eis_acos, 1, err)
      CALL global_registry%add_function('atan', eis_atan, 1, err)
      CALL global_registry%add_function('atan2', eis_atan2, 2, err)
      CALL global_registry%add_function('sinh', eis_sinh, 1, err)
      CALL global_registry%add_function('cosh',eis_cosh, 1, err)
      CALL global_registry%add_function('tanh',eis_tanh, 1, err)
      CALL global_registry%add_function('exp', eis_exp, 1, err)
      CALL global_registry%add_function('loge', eis_loge, 1, err)
      CALL global_registry%add_function('log10', eis_log10, 1, err)
      CALL global_registry%add_function('log_base', eis_log_base, 2, err)
      CALL global_registry%add_function('gauss', eis_gauss, 3, err)
      CALL global_registry%add_function('semigauss', eis_semigauss, 4, err)
      CALL global_registry%add_function('supergauss', eis_supergauss, 4, err)
    END IF

  END SUBROUTINE eip_init



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



  SUBROUTINE eip_load_block(this, name, iblock, icoblock, cap_bits)

    CLASS(eis_parser) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    TYPE(eis_stack_element), INTENT(OUT) :: iblock
    TYPE(eis_stack_co_element), INTENT(OUT) :: icoblock
    INTEGER(eis_bitmask), INTENT(OUT) :: cap_bits
    INTEGER(eis_i8) :: work
    REAL(eis_num) :: value
    LOGICAL :: can_be_unary
    INTEGER :: slen

    iblock%ptype = c_pt_bad
    iblock%value = 0
    iblock%numerical_data = 0.0_eis_num
    IF (ALLOCATED(icoblock%text)) DEALLOCATE(icoblock%text)
    ALLOCATE(icoblock%text, SOURCE = TRIM(name))
    work = 0
    cap_bits = 0

    IF (LEN(TRIM(name)) == 0) THEN
      iblock%ptype = c_pt_null
      iblock%value = 0
      iblock%numerical_data = 0.0_eis_num
      RETURN
    END IF

    work = as_parenthesis(name)
    IF (work /= 0) THEN
      ! block is a parenthesis
      iblock%ptype = c_pt_parenthesis
      iblock%value = work
      RETURN
    END IF

    IF (strcmp(name, ',')) THEN
      iblock%ptype = c_pt_separator
      iblock%value = 0
      RETURN
    END IF

    slen = LEN_TRIM(name)
    IF (strcmp(name(1:1), '"') .AND. strcmp(name(slen:slen), '"')) THEN
      iblock%ptype = c_pt_character
      RETURN
    END IF

    can_be_unary = .NOT. (this%last_block_type == c_pt_variable &
          .OR. this%last_block_type == c_pt_constant &
          .OR. this%last_block_type == c_pt_stored_variable)

    CALL global_registry%fill_block(name, iblock, icoblock, can_be_unary, &
        cap_bits)
    IF (iblock%ptype /= c_pt_bad) RETURN

    CALL this%registry%fill_block(name, iblock, icoblock, can_be_unary, &
        cap_bits)
    IF (iblock%ptype /= c_pt_bad) RETURN

    value = parse_string_as_real(name, work)
    IF (IAND(work, eis_err_bad_value) == 0) THEN
      ! block is a simple variable
      iblock%ptype = c_pt_constant
      iblock%value = 0
      iblock%numerical_data = value
      RETURN
    END IF

  END SUBROUTINE eip_load_block



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



  SUBROUTINE eip_tokenize(this, expression, output, err)

    CLASS(eis_parser) :: this
    CHARACTER(LEN=*), INTENT(IN) :: expression
    TYPE(eis_stack), INTENT(INOUT), TARGET :: output
    INTEGER(eis_error), INTENT(INOUT) :: err
    LOGICAL :: maybe_e

    CHARACTER(LEN=500) :: current
    INTEGER :: current_type, current_pointer, i, ptype
    INTEGER(eis_bitmask) :: cap_bits

    TYPE(eis_stack_element) :: iblock
    TYPE(eis_stack_co_element) :: icoblock

    IF (.NOT. this%is_init) CALL this%init()
    IF (.NOT. output%init) CALL initialise_stack(output)

    CALL initialise_stack(this%stack)
    CALL initialise_stack(this%brackets)
    this%output => output

    current(:) = ' '
    current(1:1) = expression(1:1)
    current_pointer = 2
    current_type = char_type(expression(1:1))
    maybe_e = .FALSE.

    err = eis_err_none

    this%last_block_type = c_pt_null

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
      ELSE IF (strcmp(current, 'e') .AND. .NOT. maybe_e) THEN
        ! Only interpret "e" as the Euler number if it is both preceded and
        ! followed by a number
        current(current_pointer:current_pointer) = expression(i:i)
        current_pointer = current_pointer+1
      ELSE
        CALL this%tokenize_subexpression_infix(current, iblock, icoblock, &
            cap_bits, err)
        IF (err /= eis_err_none) THEN
          CALL deallocate_stack(this%stack)
          CALL deallocate_stack(this%brackets)
          RETURN
        END IF
        output%cap_bits = IOR(output%cap_bits, cap_bits)
        current(:) = ' '
        current_pointer = 2
        current(1:1) = expression(i:i)
        current_type = ptype
        maybe_e = (iblock%ptype == c_pt_variable) .OR. (iblock%ptype &
            == c_pt_constant)
      END IF
    END DO

    CALL this%tokenize_subexpression_infix(current, iblock, icoblock, &
        cap_bits, err)
    output%cap_bits = IOR(output%cap_bits, cap_bits)

    IF (err == eis_err_none) THEN
      DO i = 1, this%stack%stack_point
        CALL pop_to_stack(this%stack, this%output)
      END DO
    ELSE
      err = IOR(err, eis_err_bad_value)
    END IF
    CALL deallocate_stack(this%stack)
    CALL deallocate_stack(this%brackets)

    IF (this%should_simplify) CALL this%simplify(this%output, C_NULL_PTR, err)
    IF (this%should_minify) CALL minify_stack(this%output)

  END SUBROUTINE eip_tokenize



  FUNCTION eip_evaluate(this, stack, result, params, errcode)
    CLASS(eis_parser) :: this
    CLASS(eis_stack), INTENT(IN) :: stack
    REAL(eis_num), DIMENSION(:), ALLOCATABLE :: result
    TYPE(C_PTR), INTENT(IN) :: params
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER :: eip_evaluate

    eip_evaluate = this%evaluator%evaluate(stack, result, params, errcode)

  END FUNCTION eip_evaluate



  SUBROUTINE eip_simplify(this, stack, params, errcode)
    CLASS(eis_parser) :: this
    CLASS(eis_stack), INTENT(INOUT) :: stack
    TYPE(C_PTR), INTENT(IN) :: params
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    CALL eis_simplify_stack(stack, params, errcode)

  END SUBROUTINE eip_simplify
  



  SUBROUTINE eip_add_function(this, name, fn, errcode,  cap_bits, &
      expected_params, can_simplify, global)

    CLASS(eis_parser) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    PROCEDURE(parser_eval_fn) :: fn
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits
    INTEGER, INTENT(IN), OPTIONAL :: expected_params
    LOGICAL, INTENT(IN), OPTIONAL :: can_simplify, global
    INTEGER :: params
    LOGICAL :: is_global

    IF (PRESENT(expected_params)) THEN
      params = expected_params
    ELSE
      params = -1
    END IF

    is_global = .FALSE.
    IF (PRESENT(global)) is_global = global

    IF (is_global) THEN
      CALL global_registry%add_function(name, fn, params, errcode, &
          can_simplify, cap_bits)
    ELSE
      CALL this%registry%add_function(name, fn, params, errcode, can_simplify, &
          cap_bits)
    END IF

  END SUBROUTINE eip_add_function



  SUBROUTINE eip_add_variable(this, name, fn, errcode, cap_bits, &
      can_simplify, global)

    CLASS(eis_parser) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    PROCEDURE(parser_eval_fn) :: fn
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits
    LOGICAL, INTENT(IN), OPTIONAL :: can_simplify, global
    LOGICAL :: is_global

    is_global = .FALSE.
    IF (PRESENT(global)) is_global = global

    IF (is_global) THEN
      CALL global_registry%add_variable(name, fn, errcode, can_simplify, &
          cap_bits)
    ELSE
      CALL this%registry%add_variable(name, fn, errcode, can_simplify, cap_bits)
    END IF

  END SUBROUTINE eip_add_variable



  SUBROUTINE eip_add_constant(this, name, value, errcode, cap_bits, &
      can_simplify, global)

    CLASS(eis_parser) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    REAL(eis_num), INTENT(IN) :: value
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits
    LOGICAL, INTENT(IN), OPTIONAL :: can_simplify, global
    LOGICAL :: is_global

    is_global = .FALSE.
    IF (PRESENT(global)) is_global = global

    IF (is_global) THEN
      CALL global_registry%add_constant(name, value, errcode, can_simplify, &
          cap_bits)
    ELSE
      CALL this%registry%add_constant(name, value, errcode, can_simplify, &
          cap_bits)
    END IF

  END SUBROUTINE eip_add_constant



  SUBROUTINE eip_add_stack_variable_stack(this, name, stack, errcode, global)

    CLASS(eis_parser) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    TYPE(eis_stack), INTENT(IN) :: stack
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    LOGICAL, INTENT(IN), OPTIONAL :: global
    LOGICAL :: is_global
        
    is_global = .FALSE.
    IF (PRESENT(global)) is_global = global

    IF (is_global) THEN
      CALL global_registry%add_stack_variable(name, stack, errcode)
    ELSE
      CALL this%registry%add_stack_variable(name, stack, errcode)
    END IF

  END SUBROUTINE eip_add_stack_variable_stack



  SUBROUTINE eip_add_stack_variable_string(this, name, string, errcode, global)

    CLASS(eis_parser) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name, string
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    LOGICAL, INTENT(IN), OPTIONAL :: global
    LOGICAL :: is_global
    TYPE(eis_stack) :: stack

    is_global = .FALSE.
    IF (PRESENT(global)) is_global = global

    CALL initialise_stack(stack)
    CALL this%tokenize(string, stack, errcode)
    IF (is_global) THEN
      CALL global_registry%add_stack_variable(name, stack, errcode)
    ELSE
      CALL this%registry%add_stack_variable(name, stack, errcode)
    END IF
    CALL deallocate_stack(stack)

  END SUBROUTINE eip_add_stack_variable_string



  SUBROUTINE eip_tokenize_subexpression_infix(this, current, iblock, icoblock, &
      cap_bits, err)

    ! This subroutine tokenizes input in normal infix maths notation
    ! It uses Dijkstra's shunting yard algorithm to convert to RPN

    CLASS(eis_parser) :: this
    CHARACTER(LEN=*), INTENT(IN) :: current
    TYPE(eis_stack_element), INTENT(INOUT) :: iblock
    TYPE(eis_stack_co_element), INTENT(INOUT) :: icoblock
    INTEGER(eis_bitmask), INTENT(OUT) :: cap_bits
    INTEGER(eis_error), INTENT(INOUT) :: err
    TYPE(eis_stack_element) :: block2
    TYPE(eis_stack_co_element) :: coblock2
    INTEGER :: ipoint, io, iu

    cap_bits = 0_eis_bitmask
    IF (ICHAR(current(1:1)) == 0) RETURN

    ! Populate the block
    CALL this%load_block(current, iblock, icoblock, cap_bits)
    IF (iblock%ptype == c_pt_bad) THEN
      err = eis_err_bad_value
      CALL deallocate_stack(this%stack)
      RETURN
    END IF

    IF (iblock%ptype /= c_pt_parenthesis &
        .AND. iblock%ptype /= c_pt_null) THEN
      this%last_block_type = iblock%ptype
    END IF

    IF (iblock%ptype == c_pt_variable &
        .OR. iblock%ptype == c_pt_constant) THEN
      CALL push_to_stack(this%output, iblock, icoblock)

    ELSE IF (iblock%ptype == c_pt_stored_variable) THEN
      CALL this%registry%copy_in_stored(iblock%value, this%output)

    ELSE IF (iblock%ptype == c_pt_stored_function) THEN
      this%output%has_stored_functions = .FALSE.
      this%stack%has_stored_functions = .FALSE.

    ELSE IF (iblock%ptype == c_pt_parenthesis) THEN
      IF (iblock%value == c_paren_left_bracket) THEN
        iblock%actual_params = 0
        CALL push_to_stack(this%stack, iblock, icoblock)
        CALL push_to_stack(this%brackets, iblock, icoblock)
      ELSE
        DO
          CALL stack_snoop(this%stack, block2, 0)
          IF (block2%ptype == c_pt_parenthesis &
              .AND. block2%value == c_paren_left_bracket) THEN
            CALL pop_to_null(this%stack)
            IF (this%brackets%stack_point > 1) THEN
              this%brackets%entries(this%brackets%stack_point-1)%actual_params &
                  = this%brackets%entries(this%brackets%stack_point-1)% &
                  actual_params &
                  + this%brackets%entries(this%brackets%stack_point)%&
                  actual_params
              CALL pop_to_null(this%brackets)
            END IF
            ! If stack isn't empty then check for function
            IF (this%stack%stack_point /= 0) THEN
              CALL stack_snoop(this%stack, block2, 0)
              IF (block2%ptype == c_pt_function) THEN
                this%stack%entries(this%stack%stack_point)%actual_params = &
                    this%brackets%entries(this%brackets%stack_point)%&
                    actual_params
                IF ((this%stack%entries(this%stack%stack_point)%actual_params &
                    /= this%stack%co_entries(this%stack%stack_point)% &
                    expected_params) .AND. &
                    this%stack%co_entries(this%stack%stack_point)% &
                    expected_params > 0) err = IOR(err, &
                    eis_err_wrong_parameters)
                CALL pop_to_stack(this%stack, this%output)
                CALL pop_to_null(this%brackets)
                !Add stored function here
              END IF
            END IF
            EXIT
          ELSE
            CALL pop_to_stack(this%stack, this%output)
          END IF
        END DO
      END IF

    ELSE IF (iblock%ptype == c_pt_function) THEN
      ! Just push functions straight onto the stack
      CALL push_to_stack(this%stack, iblock, icoblock)
      iblock%actual_params = 1
      CALL push_to_stack(this%brackets, iblock, icoblock)

    ELSE IF (iblock%ptype == c_pt_separator) THEN
      DO
        CALL stack_snoop(this%stack, block2, 0)
        IF (block2%ptype /= c_pt_parenthesis) THEN
          CALL pop_to_stack(this%stack, this%output)
        ELSE
          IF (block2%value /= c_paren_left_bracket) THEN
            err = IOR(err, eis_err_bad_value)
             RETURN
          END IF
          IF (this%brackets%stack_point > 0) THEN
            this%brackets%entries(this%brackets%stack_point)%actual_params = &
            this%brackets%entries(this%brackets%stack_point)%actual_params + 1
          END IF
          EXIT
        END IF
      END DO

    ELSE IF (iblock%ptype == c_pt_operator) THEN
      DO
        IF (this%stack%stack_point == 0) THEN
          ! stack is empty, so just push operator onto stack and
          ! leave loop
          CALL push_to_stack(this%stack, iblock, icoblock)
          EXIT
        END IF
        ! stack is not empty so check precedence etc.
        CALL stack_snoop(this%stack, block2, 0, coblock2)
        IF (block2%ptype /= c_pt_operator) THEN
          ! Previous block is not an operator so push current operator
          ! to stack and leave loop
          CALL push_to_stack(this%stack, iblock, icoblock)
          EXIT
        ELSE
          IF (icoblock%associativity == c_assoc_la &
              .OR. icoblock%associativity == c_assoc_a) THEN
            ! Operator is full associative or left associative
            IF (icoblock%precedence &
                <= coblock2%precedence) THEN
              CALL pop_to_stack(this%stack, this%output)
              CYCLE
            ELSE
              CALL push_to_stack(this%stack, iblock, icoblock)
              EXIT
            END IF
          ELSE
            IF (icoblock%precedence &
                < coblock2%precedence) THEN
              CALL pop_to_stack(this%stack, this%output)
              CYCLE
            ELSE
              CALL push_to_stack(this%stack, iblock, icoblock)
              EXIT
            END IF
          END IF
        END IF
      END DO
    END IF

  END SUBROUTINE eip_tokenize_subexpression_infix

END MODULE eis_parser_mod
