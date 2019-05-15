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
  USE eis_registry_mod
  USE eis_core_functions_mod
  USE eis_stack_mod
  USE eis_tree_mod
  USE eis_eval_stack_mod
  USE eis_error_mod
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
    TYPE(eis_error_handler) :: err_handler
    INTEGER :: last_block_type, last_block_value, last_charindex
    CHARACTER(LEN=:), ALLOCATABLE :: last_block_text
    LOGICAL :: is_init = .FALSE.
    LOGICAL :: should_simplify = .TRUE.
    LOGICAL :: should_minify = .FALSE.
    TYPE(eis_stack) :: stack, brackets
    TYPE(eis_stack), POINTER :: output
    INTEGER :: physics_units = eis_physics_none
    CONTAINS
    PROCEDURE :: init => eip_init
    PROCEDURE :: load_block => eip_load_block
    PROCEDURE :: tokenize_subexpression_infix &
        => eip_tokenize_subexpression_infix
    PROCEDURE :: add_stack_variable_stack &
        => eip_add_stack_variable_stack
    PROCEDURE :: add_stack_variable_string &
        => eip_add_stack_variable_string
    PROCEDURE :: add_stack_variable_defer &
        => eip_add_stack_variable_defer
    PROCEDURE :: emplace_node => eip_emplace_node
    PROCEDURE :: add_function_now => eip_add_function_now
    PROCEDURE :: add_function_defer => eip_add_function_defer
    PROCEDURE :: add_variable_now => eip_add_variable_now
    PROCEDURE :: add_variable_defer => eip_add_variable_defer
    PROCEDURE :: add_constant_now => eip_add_constant_now
    PROCEDURE :: add_constant_defer => eip_add_constant_defer
    PROCEDURE :: evaluate_stack => eip_evaluate_stack
    PROCEDURE :: evaluate_string => eip_evaluate_string

    GENERIC, PUBLIC :: add_function => add_function_now, add_function_defer
    GENERIC, PUBLIC :: add_variable => add_variable_now, add_variable_defer
    GENERIC, PUBLIC :: add_constant => add_constant_now, add_constant_defer
    GENERIC, PUBLIC :: add_stack_variable => add_stack_variable_stack, &
        add_stack_variable_string, add_stack_variable_defer
    PROCEDURE, PUBLIC :: add_emplaced_function => eip_add_emplaced_function
    PROCEDURE, PUBLIC :: add_emplaced_variable => eip_add_emplaced_variable
    PROCEDURE, PUBLIC :: tokenize => eip_tokenize
    GENERIC, PUBLIC :: evaluate => evaluate_string, evaluate_stack
    PROCEDURE, PUBLIC :: simplify => eip_simplify
    PROCEDURE, PUBLIC :: minify => eip_minify
    PROCEDURE, PUBLIC :: undefer => eip_undefer
    PROCEDURE, PUBLIC :: emplace => eip_emplace
    PROCEDURE, PUBLIC :: print_errors => eip_print_errors

  END TYPE eis_parser

  PRIVATE
  PUBLIC :: eis_parser

CONTAINS

  SUBROUTINE eip_init(this, should_simplify, should_minify, no_import, physics)

    CLASS(eis_parser) :: this
    LOGICAL, INTENT(IN), OPTIONAL :: should_simplify, should_minify, no_import
    INTEGER, INTENT(IN), OPTIONAL :: physics
    INTEGER(eis_error) :: err
    REAL(eis_num), PARAMETER :: pi = 3.141592653589793238462643383279503_eis_num
    REAL(eis_num) :: c
    LOGICAL :: no_import_l

    no_import_l = .FALSE.
    IF (PRESENT(should_simplify)) this%should_simplify = should_simplify
    IF (PRESENT(should_minify)) this%should_minify = should_minify
    IF (PRESENT(physics)) this%physics_units = physics
    IF (PRESENT(no_import)) no_import_l = no_import

    this%is_init = .TRUE.

    IF (.NOT. global_setup) THEN
      err = eis_err_none
      global_setup = .TRUE.
      CALL global_registry%add_operator('+', eis_uplus, c_assoc_ra, 4, err, &
        unary = .TRUE., err_handler = this%err_handler)
      CALL global_registry%add_operator('-', eis_uminus, c_assoc_ra, 4, err, &
          unary = .TRUE., err_handler = this%err_handler)
      CALL global_registry%add_operator('+', eis_bplus, c_assoc_a, 2, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('-', eis_bminus, c_assoc_la, 2, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('*', eis_times, c_assoc_a, 3, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('/', eis_divide, c_assoc_la, 3, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('^', eis_pow, c_assoc_ra, 4, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('e', eis_expo, c_assoc_la, 4, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('lt', eis_lt, c_assoc_la, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('<', eis_lt, c_assoc_la, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('le', eis_le, c_assoc_la, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('<=', eis_le, c_assoc_la, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('gt', eis_gt, c_assoc_la, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('>', eis_gt, c_assoc_la, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('ge', eis_ge, c_assoc_la, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('>=', eis_ge, c_assoc_la, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('eq', eis_eq, c_assoc_la, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('==', eis_eq, c_assoc_la, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('ne', eis_neq, c_assoc_la, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('/=', eis_neq, c_assoc_la, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('!=', eis_neq, c_assoc_la, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('and', eis_and, c_assoc_la, 0, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('or', eis_or, c_assoc_la, 0, err, &
          err_handler = this%err_handler)

      CALL global_registry%add_constant('math.pi', pi, err, &
          err_handler = this%err_handler)

      CALL global_registry%add_constant('scale.yotta', 1.0e24_eis_num, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_constant('scale.zetta', 1.0e21_eis_num, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_constant('scale.exa', 1.0e18_eis_num, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_constant('scale.peta', 1.0e15_eis_num, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_constant('scale.tera', 1.0e12_eis_num, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_constant('scale.giga', 1.0e9_eis_num, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_constant('scale.mega', 1.0e6_eis_num, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_constant('scale.kilo', 1.0e3_eis_num, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_constant('scale.hecto', 1.0e2_eis_num, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_constant('scale.deca', 1.0e1_eis_num, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_constant('scale.deci', 1.0e-1_eis_num, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_constant('scale.centi', 1.0e-2_eis_num, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_constant('scale.milli', 1.0e-3_eis_num, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_constant('scale.micro', 1.0e-6_eis_num, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_constant('scale.nano', 1.0e-9_eis_num, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_constant('scale.pico', 1.0e-12_eis_num, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_constant('scale.femto', 1.0e-15_eis_num, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_constant('scale.atto', 1.0e-18_eis_num, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_constant('scale.zepto', 1.0e-21_eis_num, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_constant('scale.yocto', 1.0e-24_eis_num, err, &
          err_handler = this%err_handler)

      CALL global_registry%add_function('math.abs', eis_abs, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_function('math.floor', eis_floor, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_function('math.ceil', eis_ceil, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_function('math.ceiling', eis_ceil, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_function('math.nint', eis_nint, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_function('math.trunc', eis_aint, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_function('math.truncate', eis_aint, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_function('math.aint', eis_aint, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_function('math.sqrt', eis_sqrt, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_function('math.sin', eis_sin, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_function('math.cos', eis_cos, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_function('math.tan', eis_tan, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_function('math.asin', eis_asin, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_function('math.acos', eis_acos, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_function('math.atan', eis_atan, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_function('math.atan2', eis_atan2, 2, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_function('math.sinh', eis_sinh, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_function('math.cosh',eis_cosh, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_function('math.tanh',eis_tanh, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_function('math.asinh', eis_asinh, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_function('math.acosh',eis_acosh, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_function('math.atanh',eis_atanh, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_function('math.exp', eis_exp, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_function('math.loge', eis_loge, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_function('math.log10', eis_log10, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_function('math.log_base', eis_log_base, 2, err, &
          err_handler = this%err_handler)

      CALL global_registry%add_function('utility.gauss', eis_gauss, 3, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_function('utility.semigauss', eis_semigauss, 4, &
          err, err_handler = this%err_handler)
      CALL global_registry%add_function('utility.supergauss', eis_supergauss, &
          4, err, err_handler = this%err_handler)

      CALL global_registry%add_function('logic.if', eis_if, &
          3, err, err_handler = this%err_handler)

      !Unit indepdendent physical constants
      CALL global_registry%add_constant('physics.na', &
          6.02214076e23_eis_num, err, err_handler = this%err_handler)

      !SI Physical constants
      CALL global_registry%add_constant('physics.si.c', 2.99792458e8_eis_num, &
          err, err_handler = this%err_handler)
      CALL global_registry%add_constant('physics.si.G', 6.67408e-11_eis_num, &
          err, err_handler = this%err_handler)
      CALL global_registry%add_constant('physics.si.h', &
          6.62607015e-34_eis_num, err, err_handler = this%err_handler)
      CALL global_registry%add_constant('physics.si.q0', &
          1.602176565e-19_eis_num, err, err_handler = this%err_handler)
      CALL global_registry%add_constant('physics.si.epsilon0', &
          8.854187817620389850536563031710750e-12_eis_num, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_constant('physics.si.mu0', &
          4.e-7_eis_num * pi, err, err_handler = this%err_handler)
      CALL global_registry%add_constant('physics.si.kb', &
          1.3806488e-23_eis_num, err, err_handler = this%err_handler)
      CALL global_registry%add_constant('physics.si.me', &
          9.10938291e-31_eis_num, err, err_handler = this%err_handler)
      CALL global_registry%add_constant('physics.si.mp', &
          1.6726219e-27_eis_num, err, err_handler = this%err_handler)
      CALL global_registry%add_constant('physics.si.mn', &
          1.674929e-27_eis_num, err, err_handler = this%err_handler)

      !CGS physical constants
      c = 2.99792458e10_eis_num
      CALL global_registry%add_constant('physics.cgs.gauss.c', &
          c, err, err_handler = this%err_handler)
      CALL global_registry%add_constant('physics.cgs.gauss.G', &
          6.67428e-8_eis_num, err, err_handler = this%err_handler)
      CALL global_registry%add_constant('physics.cgs.gauss.h', &
          6.62606885e-27_eis_num, err, err_handler = this%err_handler)
      CALL global_registry%add_constant('physics.cgs.gauss.q0', &
          4.80320427e-10_eis_num, err, err_handler = this%err_handler)
      CALL global_registry%add_constant('physics.cgs.gauss.epsilon0', &
          1.0_eis_num, err, err_handler = this%err_handler)
      CALL global_registry%add_constant('physics.cgs.gauss.mu0', &
          1.0_eis_num/c**2, err, err_handler = this%err_handler)
      CALL global_registry%add_constant('physics.cgs.gauss.na', &
          6.02214076e23_eis_num, err, err_handler = this%err_handler)
      CALL global_registry%add_constant('physics.cgs.gauss.kb', &
          1.3806504e-16_eis_num, err, err_handler = this%err_handler)
      CALL global_registry%add_constant('physics.cgs.gauss.mu', &
          1.67377e-27_eis_num, err, err_handler = this%err_handler)
      CALL global_registry%add_constant('physics.cgs.gauss.me', &
          9.10938215e-28_eis_num, err, err_handler = this%err_handler)
      CALL global_registry%add_constant('physics.cgs.gauss.mp', &
          1.6726219e-24_eis_num, err, err_handler = this%err_handler)
      CALL global_registry%add_constant('physics.cgs.gauss.mn', &
          1.674929e-24_eis_num, err, err_handler = this%err_handler)

      IF (.NOT. no_import_l) THEN
        CALL global_registry%include_namespace('math')
        CALL global_registry%include_namespace('scale')
        CALL global_registry%include_namespace('utility')
        CALL global_registry%include_namespace('logic')
        CALL this%registry%include_namespace('math')
        CALL this%registry%include_namespace('scale')
        CALL this%registry%include_namespace('utility')
        CALL this%registry%include_namespace('logic')
        IF (this%physics_units /= eis_physics_none) THEN
          CALL global_registry%include_namespace('physics')
          CALL this%registry%include_namespace('physics')
          IF (this%physics_units == eis_physics_si) THEN
            CALL global_registry%include_namespace('physics.si')
            CALL this%registry%include_namespace('physics.si')
          ELSE IF (this%physics_units == eis_physics_cgs_gauss) THEN
            CALL global_registry%include_namespace('physics.cgs')
            CALL global_registry%include_namespace('physics.cgs.gauss')
            CALL this%registry%include_namespace('physics.cgs')
            CALL this%registry%include_namespace('physics.cgs.gauss')
          END IF
        END IF
      END IF
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
        .OR. (chr >= 'a' .AND. chr <= 'z') .OR. chr == '_' .OR. chr == '"') THEN
      char_type = c_char_alpha
    ELSE IF (chr == '(' .OR. chr == ')' .OR. chr == ',') THEN
      char_type = c_char_delimiter
    ! 92 is the ASCII code for backslash
    ELSE IF (chr == '+' .OR. chr == '-' .OR. ICHAR(chr) == 92 &
        .OR. chr == '/' .OR. chr == '*' .OR. chr == '^' .OR. chr == '>' &
        .OR. chr == '<' .OR. chr == '=' .OR. chr == '!') THEN
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
          .OR. this%last_block_type == c_pt_stored_variable &
          .OR. (this%last_block_type == c_pt_parenthesis &
          .AND. this%last_block_value == c_paren_right_bracket))

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



  SUBROUTINE eip_tokenize(this, expression_in, output, err, simplify, minify)

    CLASS(eis_parser) :: this
    CHARACTER(LEN=*), INTENT(IN) :: expression_in
    TYPE(eis_stack), INTENT(INOUT), TARGET :: output
    INTEGER(eis_error), INTENT(INOUT) :: err
    LOGICAL, INTENT(IN), OPTIONAL :: simplify, minify
    LOGICAL :: maybe_e, should_simplify, should_minify
    CHARACTER(LEN=:), ALLOCATABLE :: current, expression
    INTEGER :: current_type, current_pointer, i, ptype
    INTEGER(eis_bitmask) :: cap_bits
    INTEGER :: charindex

    TYPE(eis_stack_element) :: iblock
    TYPE(eis_stack_co_element) :: icoblock

    should_minify = this%should_minify
    IF (PRESENT(minify)) should_minify = minify
    should_simplify = this%should_simplify
    IF (PRESENT(simplify)) should_simplify = simplify

    IF (.NOT. this%is_init) CALL this%init()
    IF (.NOT. output%init) CALL initialise_stack(output)

    IF (expression_in(1:6) == 'where(') THEN
      output%where_stack = .TRUE.
      ALLOCATE(expression, SOURCE = expression_in(7:LEN(expression_in)-1))
    ELSE
      ALLOCATE(expression, SOURCE = expression_in)
    END IF

    CALL initialise_stack(this%stack)
    CALL initialise_stack(this%brackets)
    this%output => output

    ALLOCATE(CHARACTER(LEN=LEN(expression))::current)

    current(:) = ' '
    current(1:1) = expression(1:1)
    current_pointer = 2
    current_type = char_type(expression(1:1))
    maybe_e = .FALSE.

    err = eis_err_none
    charindex = 1

    this%last_block_type = c_pt_null
    this%last_block_value = 0
    this%last_charindex = 1
    IF (ALLOCATED(this%last_block_text)) DEALLOCATE(this%last_block_text)
    ALLOCATE(this%last_block_text, SOURCE = " ")

    DO i = 2, LEN(TRIM(expression))
      ptype = char_type(expression(i:i))
      IF (ptype == current_type .AND. ptype /= c_char_delimiter &
          .OR. (ptype == c_char_numeric .AND. current_type == c_char_alpha &
          .AND. .NOT. strcmp(current, 'e'))) THEN
        current(current_pointer:current_pointer) = expression(i:i)
        current_pointer = current_pointer+1
      ELSE IF (strcmp(current, 'e') .AND. .NOT. maybe_e) THEN
        current(current_pointer:current_pointer) = expression(i:i)
        current_pointer = current_pointer+1
      ELSE
        CALL this%tokenize_subexpression_infix(current, iblock, icoblock, &
            cap_bits, charindex, err)
        charindex = i
        IF (err /= eis_err_none) THEN
          err = IOR(err, eis_err_parser)
          CALL deallocate_stack(this%stack)
          CALL deallocate_stack(this%brackets)
          DEALLOCATE(current)
          DEALLOCATE(expression)
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
        cap_bits,charindex,  err)
    output%cap_bits = IOR(output%cap_bits, cap_bits)

    IF (err == eis_err_none) THEN
      DO i = this%stack%stack_point, 1, -1
        IF (this%stack%entries(i)%ptype == c_pt_function) THEN
          IF (this%stack%co_entries(i)%expected_params > 0) THEN
            err = IOR(err, eis_err_wrong_parameters)
            CALL this%err_handler%add_error(eis_err_parser, err, &
                this%stack%co_entries(i)%text, &
                this%stack%co_entries(i)%charindex)
          ELSE
            this%stack%entries(i)%actual_params = 0
          END IF
        END IF
        CALL pop_to_stack(this%stack, this%output)
      END DO
    ELSE
      err = IOR(err, eis_err_parser)
    END IF
    CALL deallocate_stack(this%stack)
    CALL deallocate_stack(this%brackets)
    DEALLOCATE(current)
    DEALLOCATE(expression)

    IF (should_simplify) CALL this%simplify(this%output, C_NULL_PTR, err)
    IF (should_minify) CALL this%minify(this%output, err)

  END SUBROUTINE eip_tokenize



  FUNCTION eip_evaluate_stack(this, stack, result, params, errcode, is_no_op)
    CLASS(eis_parser) :: this
    CLASS(eis_stack), INTENT(INOUT) :: stack
    REAL(eis_num), DIMENSION(:), ALLOCATABLE :: result
    TYPE(C_PTR), INTENT(IN) :: params
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    LOGICAL, INTENT(OUT), OPTIONAL :: is_no_op
    INTEGER :: eip_evaluate_stack

    IF (stack%has_emplaced) THEN
      errcode = IOR(errcode, eis_err_has_emplaced)
      CALL this%err_handler%add_error(eis_err_evaluator, errcode)
      RETURN
    END IF

    IF (stack%has_deferred) THEN
      CALL this%undefer(stack, params, errcode)
      IF (errcode /= eis_err_none) RETURN
      stack%has_deferred = .FALSE.
    END IF

    eip_evaluate_stack = this%evaluator%evaluate(stack, result, params, &
        errcode, this%err_handler, is_no_op = is_no_op)

  END FUNCTION eip_evaluate_stack



  FUNCTION eip_evaluate_string(this, str, result, params, errcode, is_no_op, &
      simplify, minify)
    CLASS(eis_parser) :: this
    CHARACTER(LEN=*), INTENT(IN) :: str
    REAL(eis_num), DIMENSION(:), ALLOCATABLE :: result
    TYPE(C_PTR), INTENT(IN) :: params
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    LOGICAL, INTENT(OUT), OPTIONAL :: is_no_op
    LOGICAL, INTENT(IN), OPTIONAL :: simplify, minify
    INTEGER :: eip_evaluate_string
    TYPE(eis_stack) :: stack

    CALL this%tokenize(str, stack, errcode, simplify, minify)
    IF (errcode == eis_err_none) THEN
      eip_evaluate_string = this%evaluate(stack, result, params, errcode, &
          is_no_op)
      CALL deallocate_stack(stack)
    END IF

  END FUNCTION eip_evaluate_string



  SUBROUTINE eip_undefer(this, stack, params, errcode)
    CLASS(eis_parser) :: this
    CLASS(eis_stack), INTENT(INOUT) :: stack
    TYPE(C_PTR), INTENT(IN) :: params
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER :: ipt, stored_params
    INTEGER(eis_bitmask) :: cap_bits
    CHARACTER(LEN=:), ALLOCATABLE :: str

    cap_bits = 0_eis_bitmask

    PRINT *,'Undeferring'

    DO ipt = 1, stack%stack_point
      IF (stack%co_entries(ipt)%defer) THEN
        !This copy is a workaround, I do not believe it is required per standard
        ALLOCATE(str, SOURCE = stack%co_entries(ipt)%text)
        stored_params = stack%entries(ipt)%actual_params
        CALL this%load_block(str, stack%entries(ipt), &
            stack%co_entries(ipt), cap_bits)
        stack%entries(ipt)%actual_params = stored_params
        IF (stack%co_entries(ipt)%defer) THEN
          errcode = IOR(errcode, eis_err_has_deferred)
          CALL this%err_handler%add_error(eis_err_parser, errcode, &
              str, stack%co_entries(ipt)%charindex)
        END IF
        IF (stack%entries(ipt)%ptype == c_pt_stored_variable) THEN
          CALL this%registry%copy_in_stored(stack%entries(ipt)%value, &
              this%output, this%err_handler, ipt)
        END IF
        DEALLOCATE(str)
      END IF
    END DO

    stack%cap_bits = IOR(stack%cap_bits, cap_bits)

    IF (this%should_minify) CALL this%minify(stack, errcode)
    IF (this%should_simplify) CALL this%simplify(stack, params, errcode)

  END SUBROUTINE eip_undefer



  SUBROUTINE eip_simplify(this, stack, params, errcode)
    CLASS(eis_parser) :: this
    CLASS(eis_stack), INTENT(INOUT) :: stack
    TYPE(C_PTR), INTENT(IN) :: params
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    CALL eis_simplify_stack(stack, params, errcode, this%err_handler)

  END SUBROUTINE eip_simplify



  SUBROUTINE eip_minify(this, stack, errcode)
    CLASS(eis_parser) :: this
    CLASS(eis_stack), INTENT(INOUT) :: stack
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    CALL minify_stack(stack)

  END SUBROUTINE eip_minify



  RECURSIVE SUBROUTINE eip_emplace_node(this, tree_node, user_params, &
      remaining_functions, capbits)
    CLASS(eis_parser) :: this
    TYPE(eis_tree_item), POINTER, INTENT(INOUT) :: tree_node
    TYPE(C_PTR), INTENT(IN) :: user_params
    LOGICAL, INTENT(INOUT) :: remaining_functions
    INTEGER(eis_bitmask), INTENT(INOUT) :: capbits
    TYPE(eis_tree_item), POINTER :: new_node, next_node
    INTEGER(eis_error) :: errcode
    INTEGER :: inode, nparams, sp
    TYPE(eis_stack) :: temp_stack
    REAL(eis_num), DIMENSION(:), ALLOCATABLE :: results, params
    PROCEDURE(parser_late_bind_fn), POINTER :: late_bind_fn
    INTEGER(eis_status) :: status_code
    INTEGER :: rcount

    IF (ASSOCIATED(tree_node%nodes)) THEN
      DO inode = 1, SIZE(tree_node%nodes)
        next_node => tree_node%nodes(inode)
        CALL this%emplace_node(next_node, user_params, remaining_functions, &
            capbits)
      END DO
    END IF

    !This is not an stored function, no emplacement necessary
    IF (tree_node%value%ptype /= c_pt_emplaced_function &
        .AND. tree_node%value%ptype /= c_pt_emplaced_variable) RETURN

    !If there are unresolved functions further down then do nothing,
    !we'll have to emplace again later
    IF (remaining_functions) RETURN

    nparams = 0
    IF (ASSOCIATED(tree_node%nodes)) THEN
      nparams = SIZE(tree_node%nodes)
      ALLOCATE(params(nparams))
      DO inode = 1, nparams
        CALL initialise_stack(temp_stack)
        CALL eis_tree_to_stack(tree_node%nodes(inode), temp_stack)
        rcount = this%evaluate(temp_stack, results, user_params, errcode)
        params(inode) = results(1)
        CALL deallocate_stack(temp_stack)
      END DO
    ELSE
      ALLOCATE(params(nparams))
    END IF

    late_bind_fn => this%registry%get_stored_emplacement(tree_node%value%value)

    CALL initialise_stack(temp_stack)
    CALL late_bind_fn(nparams, params, user_params, temp_stack, status_code, &
        errcode)

    DEALLOCATE(params)

    IF (temp_stack%has_emplaced) CALL this%emplace(temp_stack, user_params, &
        errcode)

    capbits = IOR(capbits, temp_stack%cap_bits)
    !If emplacement is not forbidden by status then build new node
    IF (IAND(status_code, eis_status_no_emplace) == 0) THEN
      sp = temp_stack%stack_point + 1
      ALLOCATE(new_node)
      CALL eis_build_node(temp_stack, sp, new_node)
      DEALLOCATE(tree_node)
      tree_node => new_node
      new_node => NULL()
    ELSE
      remaining_functions = .TRUE.
    END IF

    CALL deallocate_stack(temp_stack)

  END SUBROUTINE eip_emplace_node



  SUBROUTINE eip_emplace(this, stack, user_params, errcode, destination)
    CLASS(eis_parser) :: this
    CLASS(eis_stack), INTENT(INOUT), TARGET :: stack
    TYPE(C_PTR), INTENT(IN) :: user_params
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    CLASS(eis_stack), INTENT(INOUT), OPTIONAL, TARGET :: destination
    TYPE(eis_tree_item), POINTER :: root
    INTEGER :: sp
    INTEGER(eis_bitmask) :: capbits
    LOGICAL :: remaining_functions
    CLASS(eis_stack), POINTER :: sptr

    IF (.NOT. stack%has_emplaced) THEN
      IF (PRESENT(destination)) THEN
        CALL copy_stack(stack, destination)
      END IF
      RETURN
    END IF

    IF (PRESENT(destination)) THEN
      CALL copy_stack(stack, destination)
      sptr => destination
    ELSE
      sptr => stack
    END IF

    remaining_functions = .FALSE.
    ALLOCATE(root)
    capbits = sptr%cap_bits
    sp = sptr%stack_point + 1
    CALL eis_build_node(stack, sp, root)
    CALL this%emplace_node(root, user_params, remaining_functions, capbits)
    CALL deallocate_stack(sptr)
    CALL initialise_stack(sptr)
    CALL eis_tree_to_stack(root, sptr)
    sptr%cap_bits = capbits
    DEALLOCATE(root)
    sptr%has_emplaced = remaining_functions

  END SUBROUTINE eip_emplace



  SUBROUTINE eip_add_function_defer(this, name, errcode,  cap_bits, &
      expected_params, can_simplify, global)

    CLASS(eis_parser) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits
    INTEGER, INTENT(IN), OPTIONAL :: expected_params
    LOGICAL, INTENT(IN), OPTIONAL :: can_simplify, global

    CALL this%add_function(name, eis_dummy, errcode, cap_bits, &
        expected_params, can_simplify, .TRUE., global)

  END SUBROUTINE eip_add_function_defer



  SUBROUTINE eip_add_function_now(this, name, fn, errcode,  cap_bits, &
      expected_params, can_simplify, defer, global)

    CLASS(eis_parser) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    PROCEDURE(parser_eval_fn) :: fn
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits
    INTEGER, INTENT(IN), OPTIONAL :: expected_params
    LOGICAL, INTENT(IN), OPTIONAL :: can_simplify, defer, global
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
          can_simplify, cap_bits, err_handler = this%err_handler, defer = defer)
    ELSE
      CALL this%registry%add_function(name, fn, params, errcode, can_simplify, &
          cap_bits, err_handler = this%err_handler, defer = defer)
    END IF

  END SUBROUTINE eip_add_function_now



  SUBROUTINE eip_add_variable_defer(this, name, errcode, cap_bits, &
      can_simplify, global)

    CLASS(eis_parser) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits
    LOGICAL, INTENT(IN), OPTIONAL :: can_simplify, global

    CALL this%add_variable(name, eis_dummy, errcode, cap_bits, can_simplify, &
        .TRUE., global)

  END SUBROUTINE eip_add_variable_defer



  SUBROUTINE eip_add_variable_now(this, name, fn, errcode, cap_bits, &
      can_simplify, defer, global)

    CLASS(eis_parser) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    PROCEDURE(parser_eval_fn) :: fn
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits
    LOGICAL, INTENT(IN), OPTIONAL :: can_simplify, defer, global
    LOGICAL :: is_global

    is_global = .FALSE.
    IF (PRESENT(global)) is_global = global

    IF (is_global) THEN
      CALL global_registry%add_variable(name, fn, errcode, can_simplify, &
          cap_bits, err_handler = this%err_handler, defer = defer)
    ELSE
      CALL this%registry%add_variable(name, fn, errcode, can_simplify, &
          cap_bits, err_handler = this%err_handler, defer = defer)
    END IF

  END SUBROUTINE eip_add_variable_now



  SUBROUTINE eip_add_constant_defer(this, name, errcode, cap_bits, &
      can_simplify, global)

    CLASS(eis_parser) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits
    LOGICAL, INTENT(IN), OPTIONAL :: can_simplify, global

    CALL this%add_constant(name, 0.0_eis_num, errcode, cap_bits, can_simplify, &
        .TRUE., global)

  END SUBROUTINE eip_add_constant_defer



  SUBROUTINE eip_add_constant_now(this, name, value, errcode, cap_bits, &
      can_simplify, defer, global)

    CLASS(eis_parser) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    REAL(eis_num), INTENT(IN) :: value
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits
    LOGICAL, INTENT(IN), OPTIONAL :: can_simplify, defer, global
    LOGICAL :: is_global

    is_global = .FALSE.
    IF (PRESENT(global)) is_global = global

    IF (is_global) THEN
      CALL global_registry%add_constant(name, value, errcode, can_simplify, &
          cap_bits, err_handler = this%err_handler, defer = defer)
    ELSE
      CALL this%registry%add_constant(name, value, errcode, can_simplify, &
          cap_bits, err_handler = this%err_handler, defer = defer)
    END IF

  END SUBROUTINE eip_add_constant_now



  SUBROUTINE eip_add_stack_variable_defer(this, name, errcode, global)

    CLASS(eis_parser) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    LOGICAL, INTENT(IN), OPTIONAL :: global
    TYPE(eis_stack) :: stack

    CALL this%add_stack_variable(name, stack, errcode, .TRUE., global)

  END SUBROUTINE eip_add_stack_variable_defer



  SUBROUTINE eip_add_stack_variable_stack(this, name, stack, errcode, defer, &
      global)

    CLASS(eis_parser) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    TYPE(eis_stack), INTENT(IN) :: stack
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    LOGICAL, INTENT(IN), OPTIONAL :: defer, global
    LOGICAL :: is_global
        
    is_global = .FALSE.
    IF (PRESENT(global)) is_global = global

    IF (is_global) THEN
      CALL global_registry%add_stack_variable(name, stack, errcode, &
          err_handler = this%err_handler, defer = defer)
    ELSE
      CALL this%registry%add_stack_variable(name, stack, errcode, &
          err_handler = this%err_handler, defer = defer)
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
      CALL global_registry%add_stack_variable(name, stack, errcode, &
          err_handler = this%err_handler)
    ELSE
      CALL this%registry%add_stack_variable(name, stack, errcode, &
          err_handler = this%err_handler)
    END IF
    CALL deallocate_stack(stack)

  END SUBROUTINE eip_add_stack_variable_string



  SUBROUTINE eip_add_emplaced_function(this, name, def_fn, errcode, &
      expected_params)

    CLASS(eis_parser) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    PROCEDURE(parser_late_bind_fn) :: def_fn
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER, INTENT(IN), OPTIONAL :: expected_params

    CALL this%registry%add_emplaced_function(name, def_fn, errcode, &
        err_handler = this%err_handler, &
        expected_parameters = expected_params)

  END SUBROUTINE eip_add_emplaced_function



  SUBROUTINE eip_add_emplaced_variable(this, name, def_fn, errcode)

    CLASS(eis_parser) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    PROCEDURE(parser_late_bind_fn) :: def_fn
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    CALL this%registry%add_emplaced_function(name, def_fn, errcode, &
        err_handler = this%err_handler, expected_parameters = 0)

  END SUBROUTINE eip_add_emplaced_variable



  SUBROUTINE eip_tokenize_subexpression_infix(this, current, iblock, icoblock, &
      cap_bits, charindex, err)

    ! This subroutine tokenizes input in normal infix maths notation
    ! It uses Dijkstra's shunting yard algorithm to convert to RPN

    CLASS(eis_parser) :: this
    CHARACTER(LEN=*), INTENT(IN) :: current
    TYPE(eis_stack_element), INTENT(INOUT) :: iblock
    TYPE(eis_stack_co_element), INTENT(INOUT) :: icoblock
    INTEGER(eis_bitmask), INTENT(OUT) :: cap_bits
    INTEGER, INTENT(IN) :: charindex
    INTEGER(eis_error), INTENT(INOUT) :: err
    TYPE(eis_stack_element) :: block2
    TYPE(eis_stack_co_element) :: coblock2
    INTEGER :: ipoint, io, iu
    LOGICAL :: stack_empty

    cap_bits = 0_eis_bitmask
    IF (ICHAR(current(1:1)) == 0) RETURN

    ! Populate the block
    CALL this%load_block(current, iblock, icoblock, cap_bits)
    icoblock%charindex = charindex
    IF (iblock%ptype == c_pt_bad) THEN
      err = eis_err_not_found
      CALL this%err_handler%add_error(eis_err_parser, err, current, charindex)
      CALL deallocate_stack(this%stack)
      RETURN
    END IF

    !Functions must be followed by a left bracket
    IF ((this%last_block_type == c_pt_function &
        .OR. this%last_block_type == c_pt_emplaced_function) &
        .AND. .NOT. (iblock%ptype == c_pt_parenthesis &
        .AND. iblock%value == c_paren_left_bracket)) THEN
      IF (this%stack%co_entries(this%stack%stack_point)%expected_params &
          == 0) THEN
        !Functions that take no parameters
        this%stack%entries(this%stack%stack_point)%actual_params = 0
        CALL pop_to_stack(this%stack, this%output)
      ELSE
        err = IOR(err, eis_err_malformed)
        CALL this%err_handler%add_error(eis_err_parser, err, &
            this%last_block_text, this%last_charindex)
        RETURN
      END IF
    END IF

    !Open brackets must not be preceeded by a constant or variable
    IF (iblock%ptype == c_pt_parenthesis &
        .AND. iblock%value == c_paren_left_bracket) THEN
      IF (this%last_block_type == c_pt_variable &
          .OR. this%last_block_type == c_pt_constant) THEN
        err = IOR(err, eis_err_bracketed_constant)
        CALL this%err_handler%add_error(eis_err_parser, err, &
            this%last_block_text, this%last_charindex)
        RETURN
      END IF
    END IF

    !If previous block was an operator than almost anything else is valid
    !except a separator or a right bracket
    IF (this%last_block_type == c_pt_operator .AND. &
        (iblock%ptype == c_pt_separator .OR. (iblock%ptype == c_pt_parenthesis &
        .AND. iblock%value == c_paren_right_bracket))) THEN
      err = IOR(err, eis_err_malformed)
      CALL this%err_handler%add_error(eis_err_parser, err, &
          this%last_block_text, this%last_charindex)
    END IF

    !If current block is a binary operator then previous block must not be
    !a separator or a left bracket. Unary operator must be preceded by either
    !a separator, an operator, an open bracket or null
    IF (iblock%ptype == c_pt_operator) THEN
      IF (icoblock%expected_params == 2) THEN
        IF (this%last_block_type == c_pt_separator &
            .OR. (this%last_block_type == c_pt_parenthesis &
            .AND. this%last_block_value == c_paren_left_bracket)) THEN
          err = IOR(err, eis_err_malformed)
          CALL this%err_handler%add_error(eis_err_parser, err, current, &
              charindex)
        END IF
      ELSE !No ternary operators so must be unary
        IF (.NOT. (this%last_block_type == c_pt_null &
            .OR. this%last_block_type == c_pt_separator &
            .OR. this%last_block_type == c_pt_operator &
            .OR. (this%last_block_type == c_pt_parenthesis &
            .AND. this%last_block_value == c_paren_left_bracket))) THEN
          err = IOR(err, eis_err_malformed)
          CALL this%err_handler%add_error(eis_err_parser, err, current, &
              charindex)
        END IF
      END IF
    END IF

    this%output%has_deferred = this%output%has_deferred .OR. icoblock%defer

    IF (iblock%ptype == c_pt_variable &
        .OR. iblock%ptype == c_pt_constant) THEN
      CALL push_to_stack(this%output, iblock, icoblock)

    ELSE IF (iblock%ptype == c_pt_stored_variable) THEN
      IF (.NOT. icoblock%defer) THEN
        CALL this%registry%copy_in_stored(iblock%value, this%output, &
        this%err_handler)
      ELSE
        CALL push_to_stack(this%stack, iblock, icoblock)
      END IF
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
              IF (this%last_block_type == c_pt_parenthesis &
                  .AND. this%last_block_value == c_paren_left_bracket) THEN
                !No actual parameters between open and close brackets
                  this%brackets%entries(this%brackets%stack_point)%&
                      actual_params &
                      = this%brackets%entries(this%brackets%stack_point)%&
                      actual_params - 1
              END IF
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
              IF (block2%ptype == c_pt_function .OR. &
                  block2%ptype == c_pt_emplaced_function) THEN
                this%stack%entries(this%stack%stack_point)%actual_params = &
                    this%brackets%entries(this%brackets%stack_point)%&
                    actual_params
                IF ((this%stack%entries(this%stack%stack_point)%actual_params &
                    /= this%stack%co_entries(this%stack%stack_point)% &
                    expected_params) .AND. &
                    this%stack%co_entries(this%stack%stack_point)% &
                    expected_params >= 0) THEN
                  err = IOR(err, eis_err_wrong_parameters)
                  IF (ALLOCATED(this%stack%co_entries)) THEN
                    CALL this%err_handler%add_error(eis_err_parser, err, &
                        this%stack%co_entries(this%stack%stack_point)%text, &
                        this%stack%co_entries(this%stack%stack_point)%charindex)
                  ELSE
                    CALL this%err_handler%add_error(eis_err_parser, err, &
                        "{unknown}", charindex)
                  END IF
                END IF
                CALL pop_to_stack(this%stack, this%output)
                CALL pop_to_null(this%brackets)
              END IF
            END IF
            EXIT
          ELSE
            CALL pop_to_stack(this%stack, this%output, stack_empty)
            IF (stack_empty) THEN
              err = IOR(err, eis_err_extra_bracket)
              CALL this%err_handler%add_error(eis_err_parser, err, &
                  ")", charindex)
              RETURN
            END IF
          END IF
        END DO
      END IF

    ELSE IF (iblock%ptype == c_pt_function .OR. iblock%ptype &
        == c_pt_emplaced_function) THEN
      IF (iblock%ptype == c_pt_emplaced_function) &
          this%output%has_emplaced = .TRUE.
      CALL push_to_stack(this%stack, iblock, icoblock)
      iblock%actual_params = 1
      CALL push_to_stack(this%brackets, iblock, icoblock)

    ELSE IF (iblock%ptype == c_pt_separator) THEN
      DO
        IF (this%stack%stack_point == 0) THEN
          !This is a separator in creating a vector, so nothing to do
          EXIT
        ELSE
          CALL stack_snoop(this%stack, block2, 0)
          IF (block2%ptype /= c_pt_parenthesis) THEN
            CALL pop_to_stack(this%stack, this%output)
          ELSE
            IF (block2%value /= c_paren_left_bracket) THEN
              err = IOR(err, eis_err_malformed)
              CALL this%err_handler%add_error(eis_err_parser, err, current, &
                  charindex)
              RETURN
            END IF
            IF (this%brackets%stack_point > 0) THEN
              this%brackets%entries(this%brackets%stack_point)%actual_params = &
              this%brackets%entries(this%brackets%stack_point)%actual_params + 1
            END IF
            EXIT
          END IF
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

    IF (iblock%ptype /= c_pt_null) THEN
      this%last_block_type = iblock%ptype
      this%last_block_value = iblock%value
      this%last_charindex = charindex
      IF (ALLOCATED(this%last_block_text)) THEN
        DEALLOCATE(this%last_block_text)
        ALLOCATE(this%last_block_text, SOURCE = current)
      END IF
    END IF

  END SUBROUTINE eip_tokenize_subexpression_infix



  SUBROUTINE eip_print_errors(this)
    CLASS(eis_parser) :: this
    INTEGER :: ierr, ec

    ec = this%err_handler%get_error_count()
    DO ierr = 1, ec
      CALL this%err_handler%print_error_string(ierr)
    END DO
    CALL this%err_handler%flush_errors()

  END SUBROUTINE eip_print_errors

END MODULE eis_parser_mod
