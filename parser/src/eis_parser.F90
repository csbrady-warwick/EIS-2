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
  USE eis_core_functions_mod
  USE eis_error_mod
  USE eis_eval_stack_mod
  USE eis_header
  USE eis_parser_header
  USE eis_raw_parser_mod
  USE eis_registry_mod
  USE eis_stack_mod
  USE eis_tree_mod
  IMPLICIT NONE

  !> Character is part of a numerical value
  INTEGER, PARAMETER :: eis_char_numeric = 1
  !> Character is part of an alphanumeric value
  INTEGER, PARAMETER :: eis_char_alpha = 2
  !> character is a delimiter
  INTEGER, PARAMETER :: eis_char_delimiter = 3
  !> character is a space
  INTEGER, PARAMETER :: eis_char_space = 4
  !> character is an opcode/special character
  INTEGER, PARAMETER :: eis_char_opcode = 5
  !> character is of unknown type
  INTEGER, PARAMETER :: eis_char_unknown = 1024

  TYPE(eis_registry), SAVE :: global_registry
  LOGICAL, SAVE :: global_setup = .FALSE.

  !>Type holding a pointer to a parser for interoperability interface
  TYPE :: parser_holder
    CLASS(eis_parser), POINTER :: contents => NULL()
    LOGICAL :: holds = .FALSE.
    CONTAINS
    FINAL :: ph_destructor
  END TYPE parser_holder

  !>Type holding a pointer to a stack for interoperability interface
  TYPE :: stack_holder
    LOGICAL :: holds_stack = .FALSE.
    INTEGER :: refcount = 1
    CLASS(eis_parser), POINTER :: parser => NULL()
    CLASS(eis_stack), POINTER :: contents => NULL()
    CONTAINS
    FINAL :: sh_destructor
  END TYPE stack_holder

  INTEGER :: interop_parser_count=0, interop_stack_count=0
  TYPE(parser_holder), DIMENSION(:), ALLOCATABLE :: interop_parsers
  TYPE(stack_holder), DIMENSION(:), ALLOCATABLE :: interop_stacks

  !> Type representing a maths parser
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
    PROCEDURE, PUBLIC :: add_constant_i4 => eip_add_constant_i4
    PROCEDURE, PUBLIC :: add_constant_i8 => eip_add_constant_i8
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
    PROCEDURE, PUBLIC :: get_error_count => eip_get_error_count
    PROCEDURE, PUBLIC :: get_error_report => eip_get_error_report
    PROCEDURE, PUBLIC :: get_error_info => eip_get_error_info
    PROCEDURE, PUBLIC :: get_tokens => eip_get_tokens
    PROCEDURE, PUBLIC :: visualize_stack => eip_visualize_stack

  END TYPE eis_parser

  PRIVATE
  PUBLIC :: eis_parser, stack_holder, parser_holder, interop_parser_count
  PUBLIC :: interop_stack_count, interop_parsers, interop_stacks
  PUBLIC :: eis_get_interop_parser, eis_get_interop_stack
  PUBLIC :: eis_add_interop_parser, eis_add_interop_stack

CONTAINS

  PURE ELEMENTAL SUBROUTINE ph_destructor(this)
    TYPE(parser_holder), INTENT(INOUT) :: this
    IF (ASSOCIATED(this%contents) .AND. this%holds) DEALLOCATE(this%contents)
  END SUBROUTINE ph_destructor
  PURE ELEMENTAL SUBROUTINE sh_destructor(this)
    TYPE(stack_holder), INTENT(INOUT) :: this
    IF (ASSOCIATED(this%contents) .AND. this%holds_stack) &
        DEALLOCATE(this%contents)
  END SUBROUTINE sh_destructor


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get a pointer to an interoperable parser
  !> @param[in] index
  !> @return eis_get_interop_parser
  FUNCTION eis_get_interop_parser(index)
    !> Parser index to retrieve
    INTEGER, INTENT(IN) :: index
    !> Pointer to parser
    TYPE(eis_parser), POINTER :: eis_get_interop_parser
    eis_get_interop_parser => NULL()
    IF (index < 1 .OR. index > interop_parser_count) RETURN
    eis_get_interop_parser => interop_parsers(index)%contents
  END FUNCTION eis_get_interop_parser


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get a pointer to an interoperable stack
  !> optionally also pointer to the parser that generated that stack
  !> @param[in] index
  !> @param[out] parser
  !> @return eis_get_interop_stack
  FUNCTION eis_get_interop_stack(index, parser)
    !> Stack index to retreive
    INTEGER, INTENT(IN) :: index
    !> Pointer to parser that generated the stack (optional)
    TYPE(eis_parser), POINTER, OPTIONAL, INTENT(OUT) :: parser
    !> Pointer to requested stack
    TYPE(eis_stack), POINTER :: eis_get_interop_stack
    eis_get_interop_stack => NULL()
    IF (PRESENT(parser)) parser => NULL()
    IF (index < 1 .OR. index > interop_stack_count) RETURN
    eis_get_interop_stack => interop_stacks(index)%contents
    IF (PRESENT(parser)) parser => interop_stacks(index)%parser
  END FUNCTION eis_get_interop_stack



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Create an interoperable parser reference from a parser
  !> @param[in] parser
  !> @param[in] holds
  !> @return eis_add_interop_parser
  FUNCTION eis_add_interop_parser(parser, holds)
    !> Parser to make interoperable
    TYPE(eis_parser), POINTER, INTENT(IN) :: parser
    LOGICAL, INTENT(IN), OPTIONAL :: holds
    !> Index of parser after storage
    INTEGER :: eis_add_interop_parser
    TYPE(parser_holder), DIMENSION(:), ALLOCATABLE :: temp

    IF (.NOT. ALLOCATED(interop_parsers)) THEN
      ALLOCATE(interop_parsers(n_parsers_default))
      interop_parser_count = 1
    ELSE
      interop_parser_count = interop_parser_count + 1
      IF (interop_parser_count > SIZE(interop_parsers)) THEN
        ALLOCATE(temp(SIZE(interop_parsers)*2))
        temp(1:SIZE(interop_parsers)) = interop_parsers
        DEALLOCATE(interop_parsers)
        CALL MOVE_ALLOC(temp, interop_parsers)
      END IF
    END IF
    interop_parsers(interop_parser_count)%contents => parser
    IF (PRESENT(holds)) interop_parsers(interop_parser_count)%holds = holds
    eis_add_interop_parser = interop_parser_count
  END FUNCTION eis_add_interop_parser



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get a pointer to an interoperable parser
  !> @param[in] index
  !> @param[in] parser_index
  !> @param[in] holds
  !> @return eis_get_interop_stack
  FUNCTION eis_add_interop_stack(stack, parser_index, holds)
    !> Stack to make interoperable
    TYPE(eis_stack), POINTER, INTENT(IN) :: stack
    !> Index of the interoperable parser that generated the stack
    INTEGER, INTENT(IN) :: parser_index
    !> Whether or not the interoperability layer holds the canonical
    !> reference to the stack
    LOGICAL, INTENT(IN), OPTIONAL :: holds
    INTEGER :: eis_add_interop_stack
    TYPE(stack_holder), DIMENSION(:), ALLOCATABLE :: temp

    eis_add_interop_stack = -1

    IF (.NOT. ALLOCATED(interop_stacks)) THEN
      ALLOCATE(interop_stacks(n_stacks_default))
      interop_stack_count = 1
    ELSE
      interop_stack_count = interop_stack_count + 1
      IF (interop_stack_count > SIZE(interop_stacks)) THEN
        ALLOCATE(temp(SIZE(interop_stacks)*2))
        temp(1:SIZE(interop_stacks)) = interop_stacks
        DEALLOCATE(interop_stacks)
        CALL MOVE_ALLOC(temp, interop_stacks)
      END IF
    END IF
    interop_stacks(interop_stack_count)%contents => stack
    interop_stacks(interop_stack_count)%parser => &
        interop_parsers(parser_index)%contents
    IF (PRESENT(holds)) &
        interop_stacks(interop_stack_count)%holds_stack = holds
    eis_add_interop_stack = interop_stack_count

  END FUNCTION eis_add_interop_stack


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Initialise a parser
  !> @param[inout] this - Self pointer
  !> @param[inout] errcode - Error code from initialising parser
  !> @param[in] should_simplify - Should the parser autosimplify. Optional,
  !> default .TRUE.
  !> @param[in] should_minify - Should the parser autominify. Optional,
  !> default .FALSE
  !> @param[in] no_import - Should the parser not import default namepaces
  !> Optional, default .FALSE.
  !> @param[in] physics - What (if any) physics module should be loaded
  !> Optional, default no physics
  !> @param[in] language_pack - Use a language pack to report errors in
  !> a different language. Optional, default use English errors
  SUBROUTINE eip_init(this, errcode, should_simplify, should_minify, &
      no_import, physics, language_pack)

    CLASS(eis_parser) :: this
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    LOGICAL, INTENT(IN), OPTIONAL :: should_simplify, should_minify, no_import
    INTEGER, INTENT(IN), OPTIONAL :: physics
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: language_pack
    INTEGER(eis_error) :: err
    REAL(eis_num), PARAMETER :: pi = 3.141592653589793238462643383279503_eis_num
    REAL(eis_num) :: c
    LOGICAL :: no_import_l

    no_import_l = .FALSE.
    IF (PRESENT(should_simplify)) this%should_simplify = should_simplify
    IF (PRESENT(should_minify)) this%should_minify = should_minify
    IF (PRESENT(physics)) this%physics_units = physics
    IF (PRESENT(no_import)) no_import_l = no_import

    CALL this%err_handler%init(errcode, language_pack = language_pack)

    this%is_init = .TRUE.

    IF (.NOT. global_setup) THEN
      err = eis_err_none
      global_setup = .TRUE.
      CALL global_registry%add_operator('+', eis_uplus, eis_assoc_ra, 4, err, &
        unary = .TRUE., err_handler = this%err_handler)
      CALL global_registry%add_operator('-', eis_uminus, eis_assoc_ra, 4, err, &
          unary = .TRUE., err_handler = this%err_handler)
      CALL global_registry%add_operator('+', eis_bplus, eis_assoc_a, 2, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('-', eis_bminus, eis_assoc_la, 2, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('*', eis_times, eis_assoc_a, 3, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('/', eis_divide, eis_assoc_la, 3, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('^', eis_pow, eis_assoc_ra, 4, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('e', eis_expo, eis_assoc_la, 4, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('lt', eis_lt, eis_assoc_la, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('<', eis_lt, eis_assoc_la, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('le', eis_le, eis_assoc_la, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('<=', eis_le, eis_assoc_la, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('gt', eis_gt, eis_assoc_la, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('>', eis_gt, eis_assoc_la, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('ge', eis_ge, eis_assoc_la, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('>=', eis_ge, eis_assoc_la, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('eq', eis_eq, eis_assoc_la, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('==', eis_eq, eis_assoc_la, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('ne', eis_neq, eis_assoc_la, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('/=', eis_neq, eis_assoc_la, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('!=', eis_neq, eis_assoc_la, 1, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('and', eis_and, eis_assoc_la, 0, err, &
          err_handler = this%err_handler)
      CALL global_registry%add_operator('or', eis_or, eis_assoc_la, 0, err, &
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


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to determine the type of a character
  !> @details
  !> Determines whether a character is a part of a number, a string, an operator
  !> etc.
  !> @param[in] chr
  !> @return char_tytpe
  FUNCTION char_type(chr)

    CHARACTER, INTENT(IN) :: chr !< Character to test
    INTEGER :: char_type !< Type code for character

    char_type = eis_char_unknown

    IF (chr == ' ') THEN
      char_type = eis_char_space
    ELSE IF (chr >= '0' .AND. chr <= '9' .OR. chr == '.') THEN
      char_type = eis_char_numeric
    ELSE IF ((chr >= 'A' .AND. chr <= 'Z') &
        .OR. (chr >= 'a' .AND. chr <= 'z') .OR. chr == '_' .OR. chr == '"') THEN
      char_type = eis_char_alpha
    ELSE IF (chr == '(' .OR. chr == ')' .OR. chr == ',') THEN
      char_type = eis_char_delimiter
    ! 92 is the ASCII code for backslash
    ELSE IF (chr == '+' .OR. chr == '-' .OR. ICHAR(chr) == 92 &
        .OR. chr == '/' .OR. chr == '*' .OR. chr == '^' .OR. chr == '>' &
        .OR. chr == '<' .OR. chr == '=' .OR. chr == '!') THEN
      char_type = eis_char_opcode
    END IF

  END FUNCTION char_type



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to identify the type of a parenthesis. Currently only identifies
  !> left and right round brackets. May expand in future
  !> @param[in] name
  !> @return as_parenthesis
  FUNCTION as_parenthesis(name)

    CHARACTER(LEN=*), INTENT(IN) :: name !< String to test
    INTEGER :: as_parenthesis !< Code for bracket type. 0 if not bracket

    as_parenthesis = 0

    IF (strcmp(name, '(')) THEN
      as_parenthesis = eis_paren_left_bracket

    ELSE IF (strcmp(name, ')')) THEN
      as_parenthesis = eis_paren_right_bracket
    END IF

  END FUNCTION as_parenthesis



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to load a given block with information from the registry
  !> @param[inout] this
  !> @param[in] name
  !> @param[out] iblock
  !> @param[out] icoblock
  !> @param[out] cap_bits
  SUBROUTINE eip_load_block(this, name, iblock, icoblock, cap_bits)

    CLASS(eis_parser), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name !< Name to look up in registry
    !> Block to fill with registry information
    TYPE(eis_stack_element), INTENT(OUT) :: iblock
    !> Coblock to fill with registry information
    TYPE(eis_stack_co_element), INTENT(OUT) :: icoblock
    !> Capability bits for the block retreived from the registry
    INTEGER(eis_bitmask), INTENT(OUT) :: cap_bits
    INTEGER(eis_i8) :: work
    REAL(eis_num) :: value
    LOGICAL :: can_be_unary
    INTEGER :: slen

    iblock%ptype = eis_pt_bad
    iblock%value = 0
    iblock%numerical_data = 0.0_eis_num
    IF (ALLOCATED(icoblock%text)) DEALLOCATE(icoblock%text)
    ALLOCATE(icoblock%text, SOURCE = TRIM(name))
    work = 0
    cap_bits = 0

    IF (LEN(TRIM(name)) == 0) THEN
      iblock%ptype = eis_pt_null
      iblock%value = 0
      iblock%numerical_data = 0.0_eis_num
      RETURN
    END IF

    work = as_parenthesis(name)
    IF (work /= 0) THEN
      ! block is a parenthesis
      iblock%ptype = eis_pt_parenthesis
      iblock%value = INT(work, eis_i4)
      RETURN
    END IF

    IF (strcmp(name, ',')) THEN
      iblock%ptype = eis_pt_separator
      iblock%value = 0
      RETURN
    END IF

    slen = LEN_TRIM(name)
    IF (strcmp(name(1:1), '"') .AND. strcmp(name(slen:slen), '"')) THEN
      iblock%ptype = eis_pt_character
      RETURN
    END IF

    can_be_unary = .NOT. (this%last_block_type == eis_pt_variable &
          .OR. this%last_block_type == eis_pt_constant &
          .OR. this%last_block_type == eis_pt_stored_variable &
          .OR. (this%last_block_type == eis_pt_parenthesis &
          .AND. this%last_block_value == eis_paren_right_bracket))

    CALL global_registry%fill_block(name, iblock, icoblock, can_be_unary, &
        cap_bits)
    IF (iblock%ptype /= eis_pt_bad) RETURN

    CALL this%registry%fill_block(name, iblock, icoblock, can_be_unary, &
        cap_bits)
    IF (iblock%ptype /= eis_pt_bad) RETURN

    value = parse_string_as_real(name, work)
    IF (IAND(work, eis_err_bad_value) == 0) THEN
      ! block is a simple variable
      iblock%ptype = eis_pt_constant
      iblock%value = 0
      iblock%numerical_data = value
      RETURN
    END IF

  END SUBROUTINE eip_load_block



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to tokenize an expression to a stack
  !> @param[inout] this
  !> @param[in] expression_in
  !> @param[inout] output
  !> @param[inout] err
  !> @param[in] simplify
  !> @param[in] minify
  SUBROUTINE eip_tokenize(this, expression_in, output, err, simplify, minify)

    CLASS(eis_parser) :: this
    !> Expression to convert to stack
    CHARACTER(LEN=*), INTENT(IN) :: expression_in
    !> Stack to contain the output. If stack is not empty then new values
    !> are pushed to the end of the stack. This will usually cause multi valued
    !> results
    TYPE(eis_stack), INTENT(INOUT), TARGET :: output
    !> Error code for errors during tokenize
    INTEGER(eis_error), INTENT(INOUT) :: err
    !> Should the stack be simplified after generation. Optional, default value
    !> set in call to "init"
    LOGICAL, INTENT(IN), OPTIONAL :: simplify
    !> Should the stack be minified after generation. Optional, default value
    !> set in call to "init"
    LOGICAL, INTENT(IN), OPTIONAL :: minify
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

    IF (.NOT. this%is_init) CALL this%init(err)
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

    this%last_block_type = eis_pt_null
    this%last_block_value = 0
    this%last_charindex = 1
    IF (ALLOCATED(this%last_block_text)) DEALLOCATE(this%last_block_text)
    ALLOCATE(this%last_block_text, SOURCE = " ")

    DO i = 2, LEN(TRIM(expression))
      ptype = char_type(expression(i:i))
      IF (ptype == current_type .AND. ptype /= eis_char_delimiter &
          .OR. (ptype == eis_char_numeric .AND. current_type == eis_char_alpha &
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
        maybe_e = (iblock%ptype == eis_pt_variable) .OR. (iblock%ptype &
            == eis_pt_constant)
      END IF
    END DO

    CALL this%tokenize_subexpression_infix(current, iblock, icoblock, &
        cap_bits,charindex,  err)
    output%cap_bits = IOR(output%cap_bits, cap_bits)

    IF (err == eis_err_none) THEN
      DO i = this%stack%stack_point, 1, -1
        IF (this%stack%entries(i)%ptype == eis_pt_function) THEN
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

    IF (err /= eis_err_none) RETURN

    IF (should_simplify) CALL this%simplify(this%output, err, &
        user_params = C_NULL_PTR)
    IF (should_minify) CALL this%minify(this%output, err)

  END SUBROUTINE eip_tokenize


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to evaluate a stack
  !> @param[inout] this
  !> @param[inout] stack
  !> @param[inout] errcode
  !> @param[in] user_params
  !> @param[out] is_no_op
  !> @return eip_evaluate_stack
  FUNCTION eip_evaluate_stack(this, stack, result, errcode, user_params, &
      is_no_op)
    CLASS(eis_parser) :: this
    CLASS(eis_stack), INTENT(INOUT) :: stack !< Stack to evaluate
    !> Allocatable array holding all the results from the evaluation.
    !> Will only be reallocated if it is too small to hold all the results
    REAL(eis_num), DIMENSION(:), ALLOCATABLE :: result
    !> Error code returned by the evaluation
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Host code parameters provided. Optional, default no values (C_PTR_NULL)
    TYPE(C_PTR), INTENT(IN), OPTIONAL :: user_params
    !> Logical determining if the stack should be a null operation. Currently
    !> related to the "where" construct
    LOGICAL, INTENT(OUT), OPTIONAL :: is_no_op
    !> Number of results returned by the evaluation
    INTEGER :: eip_evaluate_stack
    TYPE(C_PTR) :: params

    IF (PRESENT(user_params)) THEN
      params = user_params
    ELSE
      params = C_NULL_PTR
    END IF

    IF (stack%has_emplaced) THEN
      errcode = IOR(errcode, eis_err_has_emplaced)
      CALL this%err_handler%add_error(eis_err_evaluator, errcode)
      eip_evaluate_stack = 0.0_eis_num
      RETURN
    END IF

    IF (stack%has_deferred) THEN
      CALL this%undefer(stack, errcode, user_params = params)
      IF (errcode /= eis_err_none) RETURN
      stack%has_deferred = .FALSE.
    END IF

    eip_evaluate_stack = this%evaluator%evaluate(stack, result, params, &
        errcode, this%err_handler, is_no_op = is_no_op)

  END FUNCTION eip_evaluate_stack


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to evaluate a string straight to a result
  !> @param[inout] this
  !> @param[in] str
  !> @param[inout] errcode
  !> @param[in] user_params
  !> @param[out] is_no_op
  !> @return eip_evaluate_string
  FUNCTION eip_evaluate_string(this, str, result, errcode, user_params, &
      is_no_op, simplify, minify)
    CLASS(eis_parser) :: this
    !> String to evaluate as maths
    CHARACTER(LEN=*), INTENT(IN) :: str
    !> Allocatable array holding all the results from the evaluation.
    !> Will only be reallocated if it is too small to hold all the results
    REAL(eis_num), DIMENSION(:), ALLOCATABLE :: result
    !> Error code from the combined tokenization, optinally simplification and
    !> evaluation
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Host code parameters provided. Optional, default no values (C_PTR_NULL)
    TYPE(C_PTR), INTENT(IN), OPTIONAL :: user_params
    !> Logical determining if the stack should be a null operation. Currently
    !> related to the "where" construct
    LOGICAL, INTENT(OUT), OPTIONAL :: is_no_op
    !> Logical determining if the expression should be simplified before
    !> evaluation. Not usually beneficial for single direct string to value
    !> evaluations. Optional, default same as set during init
    LOGICAL, INTENT(IN), OPTIONAL :: simplify
    !> Logical determining if the expression should be minified before
    !> evaluation. Not usually beneficial for single direct string to value
    !> evaluations. Optional, default same as set during init
    LOGICAL, INTENT(IN), OPTIONAL :: minify
    !> Number of results returned by the evaluation
    INTEGER :: eip_evaluate_string
    TYPE(eis_stack) :: stack
    TYPE(C_PTR) :: params

    IF (PRESENT(user_params)) THEN
      params = user_params
    ELSE
      params = C_NULL_PTR
    END IF

    CALL this%tokenize(str, stack, errcode, simplify, minify)
    IF (errcode == eis_err_none) THEN
      eip_evaluate_string = this%evaluate(stack, result, errcode, &
          user_params = user_params, is_no_op = is_no_op)
      CALL deallocate_stack(stack)
    END IF

  END FUNCTION eip_evaluate_string



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to resolve deferred elements in a stack that have now been given
  !> final values. 
  !> @details
  !>This will happen automatically if a stack is marked as having
  !> deferred elements when the stack is evaluated. Undeferring is a permanent
  !> action that changes the stack so that it is now evaluatable. The resultant
  !> stack will be simplified and minified after undeferring using the default
  !> values for this stack
  !> @param[inout] this
  !> @param[inout] stack
  !> @param[inout] errcode
  !> @param[in] user_params
  SUBROUTINE eip_undefer(this, stack, errcode, user_params)
    CLASS(eis_parser) :: this
    !> Stack to undefer
    CLASS(eis_stack), INTENT(INOUT) :: stack
    !> Error code from undefering stage
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Optional host code parameters that might be needed during
    TYPE(C_PTR), INTENT(IN), OPTIONAL :: user_params
    INTEGER :: ipt, stored_params
    INTEGER(eis_bitmask) :: cap_bits
    CHARACTER(LEN=:), ALLOCATABLE :: str
    TYPE(C_PTR) :: params

    IF (PRESENT(user_params)) THEN
      params = user_params
    ELSE
      params = C_NULL_PTR
    END IF

    cap_bits = 0_eis_bitmask

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
        IF (stack%entries(ipt)%ptype == eis_pt_stored_variable) THEN
          CALL this%registry%copy_in_stored(stack%entries(ipt)%value, &
              this%output, this%err_handler, ipt)
        END IF
        DEALLOCATE(str)
      END IF
    END DO

    stack%cap_bits = IOR(stack%cap_bits, cap_bits)

    IF (this%should_minify) CALL this%minify(stack, errcode)
    IF (this%should_simplify) CALL this%simplify(stack, errcode, &
        user_params = params)

  END SUBROUTINE eip_undefer



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to simplify a stack. This ignores the default parser settings
  !> and always simplifies the stack
  !> @param[inout] this
  !> @param[inout] stack
  !> @param[inout] errcode
  !> @param[in] user_params
  SUBROUTINE eip_simplify(this, stack, errcode, user_params)
    CLASS(eis_parser) :: this
    !> Stack to simplify
    CLASS(eis_stack), INTENT(INOUT) :: stack
    !> Error code from the simplification step
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Optional host code parameters. Can be used by stack elements
    !> to determine if they can be simplified or not
    TYPE(C_PTR), INTENT(IN), OPTIONAL :: user_params
    TYPE(C_PTR) :: params

    IF (PRESENT(user_params)) THEN
      params = user_params
    ELSE
      params = C_NULL_PTR
    END IF

    CALL eis_simplify_stack(stack, params, errcode, this%err_handler)

  END SUBROUTINE eip_simplify



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to minify a stack. This ignores the default parser settings
  !> and always minify the stack
  !> @param[inout] this
  !> @param[inout] stack
  !> @param[inout] errcode
  !> @param[in] user_params
  SUBROUTINE eip_minify(this, stack, errcode)
    CLASS(eis_parser) :: this
    !> Stack to minify
    CLASS(eis_stack), INTENT(INOUT) :: stack
    !> Error code from the simplification step
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    CALL minify_stack(stack)

  END SUBROUTINE eip_minify



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to emplace a single node of an AST.
  !> @details
  !> This function goes down to the bottom of an AST and then goes upwards
  !> When it encounters a node that needs emplacement on the way up all of
  !> the parameters to that function (if any) are evaluated and their values
  !> are passed to the emplacement function. The entire node and it's children
  !> are replaced with the stack returned from the emplacement function. The
  !> parameter "remaining_functions" is set to true if any functions need
  !> emplacement but cannot be emplaced
  !> @param[inout] this 
  !> @param[inout] tree_name
  !> @param[in] user_params
  !> @param[out] remaining_functions
  !> @param[inout] capbits
  RECURSIVE SUBROUTINE eip_emplace_node(this, tree_node, user_params, &
      remaining_functions, capbits)
    CLASS(eis_parser) :: this
    TYPE(eis_tree_item), INTENT(INOUT) :: tree_node !< Node to operate on
    TYPE(C_PTR), INTENT(IN) :: user_params !< Host code specified parameters
    !> Logical determining if there are remaining unemplaced functions below
    !> this node
    LOGICAL, INTENT(INOUT) :: remaining_functions
    !> Capability bits for this node.
    INTEGER(eis_bitmask), INTENT(INOUT) :: capbits
    TYPE(eis_tree_item), POINTER :: new_node
    INTEGER(eis_error) :: errcode
    INTEGER :: inode, nparams, sp
    TYPE(eis_stack), TARGET :: temp_stack
    TYPE(eis_stack), POINTER :: emplace_stack
    REAL(eis_num), DIMENSION(:), ALLOCATABLE :: results, params
    REAL(eis_num_c), DIMENSION(:), ALLOCATABLE :: params_interop
    PROCEDURE(parser_late_bind_fn), POINTER :: late_bind_fn
    PROCEDURE(parser_late_bind_interop_fn), POINTER :: late_bind_fn_c
    INTEGER(eis_status) :: status_code
    INTEGER :: rcount
    INTEGER(C_INT) :: interop_stack_id

    status_code = 0_eis_bitmask
    errcode = 0_eis_error

    IF (ASSOCIATED(tree_node%nodes)) THEN
      DO inode = 1, SIZE(tree_node%nodes)
        CALL this%emplace_node(tree_node%nodes(inode), user_params, &
            remaining_functions, capbits)
      END DO
    END IF

    !This is not an stored function, no emplacement necessary
    IF (tree_node%value%ptype /= eis_pt_emplaced_function &
        .AND. tree_node%value%ptype /= eis_pt_emplaced_variable) RETURN

    nparams = 0
    IF (ASSOCIATED(tree_node%nodes)) THEN
      nparams = SIZE(tree_node%nodes)
      ALLOCATE(params(nparams))
      DO inode = 1, nparams
        CALL initialise_stack(temp_stack)
        CALL eis_tree_to_stack(tree_node%nodes(inode), temp_stack)
        rcount = this%evaluate(temp_stack, results, errcode, &
            user_params = user_params)
        params(inode) = results(1)
        CALL deallocate_stack(temp_stack)
      END DO
    ELSE
      ALLOCATE(params(nparams))
    END IF

    CALL this%registry%get_stored_emplacement(tree_node%value%value, &
        late_bind_fn, late_bind_fn_c)

    IF (ASSOCIATED(late_bind_fn)) THEN
      CALL initialise_stack(temp_stack)
      emplace_stack => temp_stack
      CALL late_bind_fn(nparams, params, user_params, temp_stack, status_code, &
          errcode)
    ELSE
      ALLOCATE(params_interop(nparams))
      params_interop = REAL(params, eis_num_c)
      CALL late_bind_fn_c(nparams, params_interop, user_params, &
          interop_stack_id, status_code, errcode)
      emplace_stack => eis_get_interop_stack(interop_stack_id) 
      DEALLOCATE(params_interop)
    END IF
    DEALLOCATE(params)

    remaining_functions = .TRUE.
    IF (.NOT. ASSOCIATED(emplace_stack)) RETURN
    IF (.NOT. emplace_stack%init) RETURN
    IF (emplace_stack%stack_point == 0) RETURN
      
    IF (emplace_stack%has_emplaced) CALL this%emplace(emplace_stack, errcode, &
        user_params = user_params)

    capbits = IOR(capbits, emplace_stack%cap_bits)
    !If emplacement is not forbidden by status then build new node
    IF (IAND(status_code, eis_status_no_emplace) == 0) THEN
      sp = emplace_stack%stack_point + 1
      ALLOCATE(new_node)
      CALL eis_build_node(emplace_stack, sp, new_node)
      DEALLOCATE(tree_node%nodes)
      tree_node = new_node
      DEALLOCATE(new_node)
      remaining_functions = .FALSE.
    ELSE
      remaining_functions = .TRUE.
    END IF

    CALL deallocate_stack(emplace_stack)

  END SUBROUTINE eip_emplace_node


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to emplace all functions in a stack
  !> @details
  !> Function emplaces all emplaceable functions in a stack. If the
  !> "destination" parameter is specified then the original stack is left
  !> unaltered and the emplaced stack is built in the destination parameter
  !> This allows multiple eplacements with different "user_params" to produce
  !> different stacks
  !> @param[inout] this
  !> @param[inout] stack
  !> @param[inout] errcode
  !> @param[in] user_params
  !> @param[out] destination
  SUBROUTINE eip_emplace(this, stack, errcode, user_params, destination)
    CLASS(eis_parser) :: this
    CLASS(eis_stack), INTENT(INOUT), TARGET :: stack !< Stack to emplace
    !> Error code from emplacement
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Host code specified parameters. Optional, default C_PTR_NULL
    TYPE(C_PTR), INTENT(IN), OPTIONAL :: user_params
    !> Destination for final emplaced stack. Optional, default is build in 
    !> "stack"
    CLASS(eis_stack), INTENT(INOUT), OPTIONAL, TARGET :: destination
    TYPE(eis_tree_item), POINTER :: root
    INTEGER :: sp
    INTEGER(eis_bitmask) :: capbits
    LOGICAL :: remaining_functions
    CLASS(eis_stack), POINTER :: sptr
    TYPE(C_PTR) :: params

    IF (PRESENT(user_params)) THEN
      params = user_params
    ELSE
      params = C_NULL_PTR
    END IF

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
    CALL this%emplace_node(root, params, remaining_functions, capbits)
    CALL deallocate_stack(sptr)
    CALL initialise_stack(sptr)
    CALL eis_tree_to_stack(root, sptr)
    sptr%cap_bits = capbits
    DEALLOCATE(root)
    sptr%has_emplaced = remaining_functions

    IF (this%should_minify) CALL this%minify(sptr, errcode)
    IF (this%should_simplify) CALL this%simplify(sptr, errcode, &
        user_params = params)

  END SUBROUTINE eip_emplace


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add a deferred function to the parser. A non-deferred version
  !> must be added before any stacks using the function are evaluated
  !> @param[inout] this
  !> @param[in] name
  !> @param[inout] errcode
  !> @param[in] cap_bits
  !> @param[in] expected_params
  !> @param[in] can_simplify
  !> @param[in] global
  SUBROUTINE eip_add_function_defer(this, name, errcode,  cap_bits, &
      expected_params, can_simplify, global)

    CLASS(eis_parser) :: this
    !> Name to register function with. Will be used in expressions to
    !> call the function
    CHARACTER(LEN=*), INTENT(IN) :: name
    !> Error code from storing the function
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Capability bits that will be induced in a stack by using this function
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits
    !> Number of expected parameters for this function. Optional, default -1
    !> (variadic function)
    INTEGER, INTENT(IN), OPTIONAL :: expected_params
    !> Whether this function can be simplified. Optional, default .TRUE. 
    LOGICAL, INTENT(IN), OPTIONAL :: can_simplify
    !> Whether to add this function to the global list of functions for all 
    !> parsers or just for this parser. Optional, default this parser only
    LOGICAL, INTENT(IN), OPTIONAL ::  global

    CALL this%add_function(name, eis_dummy, errcode, cap_bits, &
        expected_params, can_simplify, .TRUE., global)

  END SUBROUTINE eip_add_function_defer


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add a function to the parser
  !> @param[inout] this
  !> @param[in] name
  !> @param[in] fn
  !> @param[inout] errcode
  !> @param[in] cap_bits
  !> @param[in] expected_params
  !> @param[in] can_simplify
  !> @param[in] defer
  !> @param[in] global
  SUBROUTINE eip_add_function_now(this, name, fn, errcode,  cap_bits, &
      expected_params, can_simplify, defer, global)

    CLASS(eis_parser) :: this
    !> Name to register function with. Will be used in expressions to
    !> call the function
    CHARACTER(LEN=*), INTENT(IN) :: name
    !> Function to call when the expression is evaluated
    PROCEDURE(parser_eval_fn) :: fn
    !> Error code from storing the function
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Capability bits that will be induced in a stack by using this function
    !> Optional, default 0
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits
    !> Number of expected parameters for this function. Optional, default -1
    !> (variadic function)
    INTEGER, INTENT(IN), OPTIONAL :: expected_params
    !> Whether this function can be simplified. Optional, default .TRUE. 
    LOGICAL, INTENT(IN), OPTIONAL :: can_simplify
    !> Whether this function should be deferred. If .TRUE. effect is the
    !> same as calling eip_add_function_defer
    LOGICAL, INTENT(IN), OPTIONAL :: defer
    !> Whether to add this function to the global list of functions for all 
    !> parsers or just for this parser. Optional, default this parser only
    LOGICAL, INTENT(IN), OPTIONAL :: global
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



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add a deferred variable to the parser. A non-deferred version
  !> must be added before any stacks using the variable are evaluated
  !> @param[inout] this
  !> @param[in] name
  !> @param[inout] errcode
  !> @param[in] cap_bits
  !> @param[in] can_simplify
  !> @param[in] global
  SUBROUTINE eip_add_variable_defer(this, name, errcode, cap_bits, &
      can_simplify, global)

    CLASS(eis_parser) :: this
    !> Name to register variable with. Will be used in expressions to
    !> call the function
    CHARACTER(LEN=*), INTENT(IN) :: name
    !> Error code from storing the variable
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Capability bits that will be induced in a stack by using this variable
    !> Optional, default 0
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits
    !> Whether this function can be simplified. Optional, default .TRUE. 
    LOGICAL, INTENT(IN), OPTIONAL :: can_simplify
    !> Whether to add this function to the global list of functions for all 
    !> parsers or just for this parser. Optional, default this parser only
    LOGICAL, INTENT(IN), OPTIONAL ::  global

    CALL this%add_variable(name, eis_dummy, errcode, cap_bits, can_simplify, &
        .TRUE., global)

  END SUBROUTINE eip_add_variable_defer



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add a variable to the parser.
  !> @param[inout] this
  !> @param[in] name
  !> @param[in] fn
  !> @param[inout] errcode
  !> @param[in] cap_bits
  !> @param[in] can_simplify
  !> @param[in] defer
  !> @param[in] global
  SUBROUTINE eip_add_variable_now(this, name, fn, errcode, cap_bits, &
      can_simplify, defer, global)

    CLASS(eis_parser) :: this
    !> Name to register variable with. Will be used in expressions to
    !> call the function
    CHARACTER(LEN=*), INTENT(IN) :: name
    !> Function to call when the expression is evaluated
    PROCEDURE(parser_eval_fn) :: fn
    !> Error code from storing the variable
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Capability bits that will be induced in a stack by using this variable
    !> Optional, default 0
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits
    !> Whether this function can be simplified. Optional, default .TRUE. 
    LOGICAL, INTENT(IN), OPTIONAL :: can_simplify
    !> Whether this function should be deferred. If .TRUE. effect is the
    !> same as calling eip_add_function_defer
    LOGICAL, INTENT(IN), OPTIONAL :: defer
    !> Whether to add this function to the global list of functions for all 
    !> parsers or just for this parser. Optional, default this parser only
    LOGICAL, INTENT(IN), OPTIONAL ::  global
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



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add a deferred constant to the parser. A non-deferred version
  !> must be added before any stacks using the variable are evaluated
  !> @param[inout] this
  !> @param[in] name
  !> @param[inout] errcode
  !> @param[in] cap_bits
  !> @param[in] can_simplify
  !> @param[in] global
  SUBROUTINE eip_add_constant_defer(this, name, errcode, &
      can_simplify, global, cap_bits)

    CLASS(eis_parser) :: this
    !> Name to register variable with. Will be used in expressions to
    !> call the function
    CHARACTER(LEN=*), INTENT(IN) :: name
    !> Error code from storing the variable
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Capability bits that will be induced in a stack by using this variable
    !> Optional, default 0
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits
    !> Whether this function can be simplified. Optional, default .TRUE. 
    LOGICAL, INTENT(IN), OPTIONAL :: can_simplify
    !> Whether to add this function to the global list of functions for all 
    !> parsers or just for this parser. Optional, default this parser only
    LOGICAL, INTENT(IN), OPTIONAL ::  global

    CALL this%add_constant(name, 0.0_eis_num, errcode, cap_bits, can_simplify, &
        .TRUE., global)

  END SUBROUTINE eip_add_constant_defer



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add a constant to the parser.
  !> @param[inout] this
  !> @param[in] name
  !> @param[in] value
  !> @param[inout] errcode
  !> @param[in] cap_bits
  !> @param[in] can_simplify
  !> @param[in] defer
  !> @param[in] global
  SUBROUTINE eip_add_constant_now(this, name, value, errcode, cap_bits, &
      can_simplify, defer, global)

    CLASS(eis_parser) :: this
    !> Name to register variable with. Will be used in expressions to
    !> call the function
    CHARACTER(LEN=*), INTENT(IN) :: name
    !> Value to associate with the name
    REAL(eis_num), INTENT(IN) :: value
    !> Error code from storing the variable
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Capability bits that will be induced in a stack by using this constant
    !> Optional, default 0
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits
    !> Whether this constant can be simplified. Optional, default .TRUE. 
    LOGICAL, INTENT(IN), OPTIONAL :: can_simplify
    !> Whether this constant should be deferred. If .TRUE. effect is the
    !> same as calling eip_add_constant_defer
    LOGICAL, INTENT(IN), OPTIONAL :: defer
    !> Whether to add this constant to the global list of constant for all 
    !> parsers or just for this parser. Optional, default this parser only
    LOGICAL, INTENT(IN), OPTIONAL ::  global
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



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add a constant to the parser.
  !> @param[inout] this
  !> @param[in] name
  !> @param[in] value
  !> @param[inout] errcode
  !> @param[in] cap_bits
  !> @param[in] can_simplify
  !> @param[in] defer
  !> @param[in] global
  SUBROUTINE eip_add_constant_i4(this, name, value, errcode, cap_bits, &
      can_simplify, defer, global)

    CLASS(eis_parser) :: this
    !> Name to register variable with. Will be used in expressions to
    !> call the function
    CHARACTER(LEN=*), INTENT(IN) :: name
    !> Value to associate with the name
    INTEGER(eis_i4), INTENT(IN) :: value
    !> Error code from storing the variable
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Capability bits that will be induced in a stack by using this constant
    !> Optional, default 0
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits
    !> Whether this constant can be simplified. Optional, default .TRUE. 
    LOGICAL, INTENT(IN), OPTIONAL :: can_simplify
    !> Whether this constant should be deferred. If .TRUE. effect is the
    !> same as calling eip_add_constant_defer
    LOGICAL, INTENT(IN), OPTIONAL :: defer
    !> Whether to add this constant to the global list of constant for all 
    !> parsers or just for this parser. Optional, default this parser only
    LOGICAL, INTENT(IN), OPTIONAL ::  global
    LOGICAL :: is_global

    CALL this%add_constant(name, REAL(value, eis_num), errcode, cap_bits, &
        can_simplify, defer, global)

  END SUBROUTINE eip_add_constant_i4



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add a constant to the parser.
  !> @param[inout] this
  !> @param[in] name
  !> @param[in] value
  !> @param[inout] errcode
  !> @param[in] cap_bits
  !> @param[in] can_simplify
  !> @param[in] defer
  !> @param[in] global
  SUBROUTINE eip_add_constant_i8(this, name, value, errcode, cap_bits, &
      can_simplify, defer, global)

    CLASS(eis_parser) :: this
    !> Name to register variable with. Will be used in expressions to
    !> call the function
    CHARACTER(LEN=*), INTENT(IN) :: name
    !> Value to associate with the name
    INTEGER(eis_i8), INTENT(IN) :: value
    !> Error code from storing the variable
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Capability bits that will be induced in a stack by using this constant
    !> Optional, default 0
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits
    !> Whether this constant can be simplified. Optional, default .TRUE. 
    LOGICAL, INTENT(IN), OPTIONAL :: can_simplify
    !> Whether this constant should be deferred. If .TRUE. effect is the
    !> same as calling eip_add_constant_defer
    LOGICAL, INTENT(IN), OPTIONAL :: defer
    !> Whether to add this constant to the global list of constant for all 
    !> parsers or just for this parser. Optional, default this parser only
    LOGICAL, INTENT(IN), OPTIONAL ::  global
    LOGICAL :: is_global

    CALL this%add_constant(name, REAL(value, eis_num), errcode, cap_bits, &
        can_simplify, defer, global)

  END SUBROUTINE eip_add_constant_i8



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add a deferred stack variable to the parser. A non-deferred version
  !> must be added before any stacks using the variable are evaluated
  !> @param[inout] this
  !> @param[in] name
  !> @param[inout] errcode
  !> @param[in] can_simplify
  !> @param[in] global
  SUBROUTINE eip_add_stack_variable_defer(this, name, errcode, global)

    CLASS(eis_parser) :: this
    !> Name to register variable with. Will be used in expressions to
    !> use the stack variable
    CHARACTER(LEN=*), INTENT(IN) :: name
    !> Error code from storing the variable
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Whether to add this constant to the global list of constant for all 
    !> parsers or just for this parser. Optional, default this parser only
    LOGICAL, INTENT(IN), OPTIONAL :: global
    TYPE(eis_stack) :: stack

    CALL this%add_stack_variable(name, stack, errcode, .TRUE., global)

  END SUBROUTINE eip_add_stack_variable_defer


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add a constant to the parser.
  !> @param[inout] this
  !> @param[in] name
  !> @param[in] stack
  !> @param[inout] errcode
  !> @param[in] defer
  !> @param[in] global
  SUBROUTINE eip_add_stack_variable_stack(this, name, stack, errcode, defer, &
      global)

    CLASS(eis_parser) :: this
    !> Name to register variable with. Will be used in expressions to
    !> use the stack variable
    CHARACTER(LEN=*), INTENT(IN) :: name
    !> Stack to associate with the name
    TYPE(eis_stack), INTENT(IN) :: stack
    !> Error code from storing the stack variable
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Whether this stack variable should be deferred. If .TRUE. effect is the
    !> same as calling eip_add_stack_variable_defer
    LOGICAL, INTENT(IN), OPTIONAL :: defer
    !> Whether to add this stack_variable to the global list for all
    !> parsers or just for this parser. Optional, default this parser only
    LOGICAL, INTENT(IN), OPTIONAL ::  global
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


  !> @brief
  !> Add a stack variable from a string expression to the parser.
  !> @param[inout] this
  !> @param[in] name
  !> @param[in] string
  !> @param[inout] errcode
  !> @param[in] global
  SUBROUTINE eip_add_stack_variable_string(this, name, string, errcode, global)

    CLASS(eis_parser) :: this
    !> Name to associated with the stack variable
    CHARACTER(LEN=*), INTENT(IN) :: name
    !> Parseable string to make into stack variable
    CHARACTER(LEN=*), INTENT(IN) :: string
    !> Error code from the parse and the store of the variable
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Whether to add this stack_variable to the global list for all
    !> parsers or just for this parser. Optional, default this parser only
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


  !> @brief
  !> Add an emplaced function to the parser. Emplaced functions
  !> cannot be added globally or deferred
  !> @param[inout] this
  !> @param[in] name
  !> @param[in] def_fn
  !> @param[inout] errcode
  !> @param[in] expected_params
  SUBROUTINE eip_add_emplaced_function(this, name, def_fn, errcode, &
      expected_params)

    CLASS(eis_parser) :: this
    !> Name to associated with the emplaced function
    CHARACTER(LEN=*), INTENT(IN) :: name
    !> Function to be called when emplacing the function
    PROCEDURE(parser_late_bind_fn) :: def_fn
    !> Error code associated with the storing of the emplaced function
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Number of parameters expected for the emplaced function. Optional, 
    !> default = -1 (variadic function)
    INTEGER, INTENT(IN), OPTIONAL :: expected_params

    CALL this%registry%add_emplaced_function(name, def_fn, errcode, &
        err_handler = this%err_handler, &
        expected_parameters = expected_params)

  END SUBROUTINE eip_add_emplaced_function



  !> @brief
  !> Add an emplaced variable to the parser. Emplaced variables
  !> cannot be added globally or deferred
  !> @param[inout] this
  !> @param[in] name
  !> @param[in] def_fn
  !> @param[inout] errcode
  !> @param[in] expected_params
  SUBROUTINE eip_add_emplaced_variable(this, name, def_fn, errcode)

    CLASS(eis_parser) :: this
    !> Name to associated with the emplaced variable
    CHARACTER(LEN=*), INTENT(IN) :: name
    !> Function to be called when emplacing the variable
    PROCEDURE(parser_late_bind_fn) :: def_fn
    !> Error code associated with the storing of the emplaced variable
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    CALL this%registry%add_emplaced_function(name, def_fn, errcode, &
        err_handler = this%err_handler, expected_parameters = 0)

  END SUBROUTINE eip_add_emplaced_variable



  !> @brief
  !> Function to tokenize a single text chunk into a element and a co-element
  !> and populate appropriate objects with that information
  !> This subroutine tokenizes input in normal infix maths notation
  !> It uses Dijkstra's shunting yard algorithm to convert to RPN
  !> @param[inout] this
  !> @param[in] current
  !> @param[inout] iblock
  !> @param[inout] icoblock
  !> @param[inout] cap_bits
  !> @param[in] charindex
  !> @param[inout] err
  SUBROUTINE eip_tokenize_subexpression_infix(this, current, iblock, icoblock, &
      cap_bits, charindex, err)

    CLASS(eis_parser) :: this
    !> Text to parse
    CHARACTER(LEN=*), INTENT(IN) :: current
    !> Element to fill with information
    TYPE(eis_stack_element), INTENT(INOUT) :: iblock
    !> Co-element to fill with information
    TYPE(eis_stack_co_element), INTENT(INOUT) :: icoblock
    !> Cap bits from evaluating this element
    INTEGER(eis_bitmask), INTENT(OUT) :: cap_bits
    !> Character index for "current" in the original string
    !> used for error reporting
    INTEGER, INTENT(IN) :: charindex
    !> Error code from parsing this element
    INTEGER(eis_error), INTENT(INOUT) :: err
    TYPE(eis_stack_element) :: block2
    TYPE(eis_stack_co_element) :: coblock2
    LOGICAL :: stack_empty

    cap_bits = 0_eis_bitmask
    IF (ICHAR(current(1:1)) == 0) RETURN

    ! Populate the block
    CALL this%load_block(current, iblock, icoblock, cap_bits)
    icoblock%charindex = charindex
    IF (iblock%ptype == eis_pt_bad) THEN
      err = eis_err_not_found
      CALL this%err_handler%add_error(eis_err_parser, err, current, charindex)
      CALL deallocate_stack(this%stack)
      RETURN
    END IF

    !Functions must be followed by a left bracket
    IF ((this%last_block_type == eis_pt_function &
        .OR. this%last_block_type == eis_pt_emplaced_function) &
        .AND. .NOT. (iblock%ptype == eis_pt_parenthesis &
        .AND. iblock%value == eis_paren_left_bracket)) THEN
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
    IF (iblock%ptype == eis_pt_parenthesis &
        .AND. iblock%value == eis_paren_left_bracket) THEN
      IF (this%last_block_type == eis_pt_variable &
          .OR. this%last_block_type == eis_pt_constant) THEN
        err = IOR(err, eis_err_bracketed_constant)
        CALL this%err_handler%add_error(eis_err_parser, err, &
            this%last_block_text, this%last_charindex)
        RETURN
      END IF
    END IF

    !If previous block was an operator than almost anything else is valid
    !except a separator or a right bracket
    IF (this%last_block_type == eis_pt_operator .AND. &
        (iblock%ptype == eis_pt_separator &
        .OR. (iblock%ptype == eis_pt_parenthesis &
        .AND. iblock%value == eis_paren_right_bracket))) THEN
      err = IOR(err, eis_err_malformed)
      CALL this%err_handler%add_error(eis_err_parser, err, &
          this%last_block_text, this%last_charindex)
    END IF

    !If current block is a binary operator then previous block must not be
    !a separator or a left bracket. Unary operator must be preceded by either
    !a separator, an operator, an open bracket or null
    IF (iblock%ptype == eis_pt_operator) THEN
      IF (icoblock%expected_params == 2) THEN
        IF (this%last_block_type == eis_pt_separator &
            .OR. (this%last_block_type == eis_pt_parenthesis &
            .AND. this%last_block_value == eis_paren_left_bracket)) THEN
          err = IOR(err, eis_err_malformed)
          CALL this%err_handler%add_error(eis_err_parser, err, current, &
              charindex)
        END IF
      ELSE !No ternary operators so must be unary
        IF (.NOT. (this%last_block_type == eis_pt_null &
            .OR. this%last_block_type == eis_pt_separator &
            .OR. this%last_block_type == eis_pt_operator &
            .OR. (this%last_block_type == eis_pt_parenthesis &
            .AND. this%last_block_value == eis_paren_left_bracket))) THEN
          err = IOR(err, eis_err_malformed)
          CALL this%err_handler%add_error(eis_err_parser, err, current, &
              charindex)
        END IF
      END IF
    END IF

    this%output%has_deferred = this%output%has_deferred .OR. icoblock%defer

    IF (iblock%ptype == eis_pt_variable &
        .OR. iblock%ptype == eis_pt_constant) THEN
      CALL push_to_stack(this%output, iblock, icoblock)

    ELSE IF (iblock%ptype == eis_pt_stored_variable) THEN
      IF (.NOT. icoblock%defer) THEN
        CALL this%registry%copy_in_stored(iblock%value, this%output, &
        this%err_handler)
      ELSE
        CALL push_to_stack(this%stack, iblock, icoblock)
      END IF
    ELSE IF (iblock%ptype == eis_pt_parenthesis) THEN
      IF (iblock%value == eis_paren_left_bracket) THEN
        iblock%actual_params = 0
        CALL push_to_stack(this%stack, iblock, icoblock)
        CALL push_to_stack(this%brackets, iblock, icoblock)
      ELSE
        DO
          CALL stack_snoop(this%stack, block2, 0)
          IF (block2%ptype == eis_pt_parenthesis &
              .AND. block2%value == eis_paren_left_bracket) THEN
            CALL pop_to_null(this%stack)
            IF (this%brackets%stack_point > 1) THEN
              IF (this%last_block_type == eis_pt_parenthesis &
                  .AND. this%last_block_value == eis_paren_left_bracket) THEN
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
              IF (block2%ptype == eis_pt_function .OR. &
                  block2%ptype == eis_pt_emplaced_function) THEN
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

    ELSE IF (iblock%ptype == eis_pt_function .OR. iblock%ptype &
        == eis_pt_emplaced_function) THEN
      IF (iblock%ptype == eis_pt_emplaced_function) &
          this%output%has_emplaced = .TRUE.
      CALL push_to_stack(this%stack, iblock, icoblock)
      iblock%actual_params = 1
      CALL push_to_stack(this%brackets, iblock, icoblock)

    ELSE IF (iblock%ptype == eis_pt_separator) THEN
      DO
        IF (this%stack%stack_point == 0) THEN
          !This is a separator in creating a vector, so nothing to do
          EXIT
        ELSE
          CALL stack_snoop(this%stack, block2, 0)
          IF (block2%ptype /= eis_pt_parenthesis) THEN
            CALL pop_to_stack(this%stack, this%output)
          ELSE
            IF (block2%value /= eis_paren_left_bracket) THEN
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

    ELSE IF (iblock%ptype == eis_pt_operator) THEN
      DO
        IF (this%stack%stack_point == 0) THEN
          ! stack is empty, so just push operator onto stack and
          ! leave loop
          CALL push_to_stack(this%stack, iblock, icoblock)
          EXIT
        END IF
        ! stack is not empty so check precedence etc.
        CALL stack_snoop(this%stack, block2, 0, coblock2)
        IF (block2%ptype /= eis_pt_operator) THEN
          ! Previous block is not an operator so push current operator
          ! to stack and leave loop
          CALL push_to_stack(this%stack, iblock, icoblock)
          EXIT
        ELSE
          IF (icoblock%associativity == eis_assoc_la &
              .OR. icoblock%associativity == eis_assoc_a) THEN
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

    IF (iblock%ptype /= eis_pt_null) THEN
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


  !> @brief
  !> Get the number of errors reported on this parser
  !> @param[inout] this
  !> @return count
  FUNCTION eip_get_error_count(this) RESULT(count)
    CLASS(eis_parser), INTENT(IN) :: this
    INTEGER :: count !< Number of errors reported

    count = this%err_handler%get_error_count()

  END FUNCTION eip_get_error_count


  !> @brief
  !> Get the error report on a specified error
  !> @param[inout] this
  !> @param[in] index
  !> @param[out] report
  SUBROUTINE eip_get_error_report(this, index, report)
    CLASS(eis_parser), INTENT(IN) :: this
    !> Index of error to get report on. Must be between 1 and
    !> the result of eip_get_error_count
    INTEGER, INTENT(IN) :: index
    !> Allocatable string variable containing the error report
    !> will be reallocated to be exactly long enough to store
    !> the error report whether allocated or not
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: report

    CALL this%err_handler%get_error_report(index, report)
  END SUBROUTINE eip_get_error_report



  !> @brief
  !> Get error information for a single error. This contains the 
  !> same information as the error report but is not formatted for use
  !> @param[inout] this
  !> @param[in] index
  !> @param[out] error_cause
  !> @param[out] error_cause_location
  !> @param[out] error_phase
  !> @param[out] error_type
  SUBROUTINE eip_get_error_info(this, index, error_cause, &
      error_cause_location, error_phase, error_type)

    CLASS(eis_parser), INTENT(IN) :: this
    !> Index of error to get report on. Must be between 1 and
    !> the result of eip_get_error_count
    INTEGER, INTENT(IN) :: index
    !> Reports the string representation of the part of the string that
    !> caused the error. Optional, default is not return this info
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(INOUT) :: error_cause
    !> Reports the character offset location of the cause of the error
    !> Optional, default is not report this info
    INTEGER, INTENT(INOUT), OPTIONAL :: error_cause_location
    !> Reports the string representation of where in the parsing phase the
    !> error occured. Optional, default is not report this info
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(INOUT) :: error_phase
    !> Reports the string representation of the type of error that was
    !> encountered during the parsing. Optional, default is no report this info
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(INOUT) :: error_type
    CHARACTER(LEN=:), ALLOCATABLE :: ec, eps, ets
    INTEGER :: ecl

    CALL this%err_handler%get_error_cause(index, ec, ecl)
    CALL this%err_handler%get_error_string(index, ets, eps)

    IF (PRESENT(error_cause)) CALL MOVE_ALLOC(ec, error_cause)
    IF (PRESENT(error_cause_location)) error_cause_location = ecl
    IF (PRESENT(error_phase)) CALL MOVE_ALLOC(eps, error_phase)
    IF (PRESENT(error_type)) CALL MOVE_ALLOC(ets, error_type)

    IF (ALLOCATED(ec)) DEALLOCATE(ec)
    IF (ALLOCATED(eps)) DEALLOCATE(eps)
    IF (ALLOCATED(ets)) DEALLOCATE(ets)

  END SUBROUTINE eip_get_error_info


  !> @brief
  !> Get a string representation of a stack
  !> @param[in] this
  !> @param[in] stack_in
  !> @param[out] str_out
  SUBROUTINE eip_get_tokens(this, stack_in, str_out)
    CLASS(eis_parser), INTENT(IN) :: this
    CLASS(eis_stack), INTENT(IN) :: stack_in !< Stack input
    !> String equivalent of the stack (RPN notation)
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: str_out

    CALL get_tokens(stack_in, str_out)

  END SUBROUTINE eip_get_tokens


  !> @brief
  !> Get a string representation of the AST version of the stack
  !> @param[in] this
  !> @param[in] stack_in
  !> @param[out] str_out
  SUBROUTINE eip_visualize_stack(this, stack_in, str_out)
    CLASS(eis_parser), INTENT(IN) :: this
    CLASS(eis_stack), INTENT(IN) :: stack_in !< Stack input
    !> String to hold the output of the stack as a dot file
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: str_out

    CALL eis_visualise_stack(stack_in, str_out)

  END SUBROUTINE eip_visualize_stack

END MODULE eis_parser_mod
