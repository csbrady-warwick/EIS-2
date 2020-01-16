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

!While this file is derived from a pre-GPL version of EPOCH's parser to avoid
!license issues, many improvements have been made to the parser since then.
!While the author for all of these routines is c.s.brady@warwick.ac.uk, much
!of the background for the text parser is based of work done by other EPOCH
!developers, most notably k.bennett@warwick.ac.uk

MODULE eis_parser_mod

  USE, INTRINSIC :: ISO_C_BINDING
  USE eis_string_store_mod
  USE eis_core_functions_mod
  USE eis_error_mod
  USE eis_eval_stack_mod
  USE eis_header
  USE eis_parser_constants
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
    LOGICAL :: owns = .FALSE.
    CONTAINS
    FINAL :: ph_destructor
  END TYPE parser_holder

  !>Type holding a pointer to a stack for interoperability interface
  TYPE :: stack_holder
    LOGICAL :: owns_stack = .FALSE.
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
    TYPE(eis_string_store) :: string_param_store
    TYPE(eis_error_handler), POINTER :: err_handler => NULL()
    LOGICAL :: owns_err_handler = .FALSE.
    INTEGER, PUBLIC :: interop_id = -1
    INTEGER :: last_block_type, last_block_value, last_charindex
    CHARACTER(LEN=:), ALLOCATABLE :: last_block_text
    LOGICAL :: is_init = .FALSE.
    LOGICAL :: should_simplify = .TRUE.
    LOGICAL :: should_minify = .FALSE.
    LOGICAL :: allow_text = .FALSE.
    TYPE(eis_stack) :: stack, brackets
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
    PROCEDURE :: add_variable_i4 => eip_add_variable_i4
    PROCEDURE :: add_variable_i8 => eip_add_variable_i8
    PROCEDURE :: add_variable_r4 => eip_add_variable_r4
    PROCEDURE :: add_variable_r8 => eip_add_variable_r8
    PROCEDURE :: add_variable_c => eip_add_variable_c
    PROCEDURE :: add_constant_now => eip_add_constant_now
    PROCEDURE :: add_constant_defer => eip_add_constant_defer
    PROCEDURE :: add_constant_i4 => eip_add_constant_i4
    PROCEDURE :: add_constant_i8 => eip_add_constant_i8
    PROCEDURE :: evaluate_stack => eip_evaluate_stack
    PROCEDURE :: evaluate_string => eip_evaluate_string
    PROCEDURE :: get_registry_symbol_count => eip_get_registry_symbol_count
    PROCEDURE :: get_registry_symbol => eip_get_registry_symbol

    GENERIC, PUBLIC :: add_function => add_function_now, add_function_defer
    GENERIC, PUBLIC :: add_variable => add_variable_now, add_variable_defer
    GENERIC, PUBLIC :: add_pointer_variable => add_variable_i4, &
        add_variable_i8, add_variable_r4, add_variable_r8, add_variable_c
    GENERIC, PUBLIC :: add_constant => add_constant_now, add_constant_defer
    GENERIC, PUBLIC :: add_integer_constant => add_constant_i4, add_constant_i8
    GENERIC, PUBLIC :: add_stack_variable => add_stack_variable_stack, &
        add_stack_variable_string, add_stack_variable_defer
    PROCEDURE, PUBLIC :: add_emplaced_function &
        => eip_add_emplaced_function
    PROCEDURE, PUBLIC :: add_emplaced_stack_function &
        => eip_add_emplaced_stack_function
    PROCEDURE, PUBLIC :: add_emplaced_c_function &
        => eip_add_emplaced_interop_function
    PROCEDURE, PUBLIC :: add_emplaced_c_stack_function &
        => eip_add_emplaced_interop_stack_function
    PROCEDURE, PUBLIC :: add_emplaced_variable => eip_add_emplaced_variable
    PROCEDURE, PUBLIC :: add_functor => eip_add_functor
    PROCEDURE, PUBLIC :: add_functor_pointer => eip_add_functor_ptr
    PROCEDURE, PUBLIC :: tokenize => eip_tokenize
    PROCEDURE, PUBLIC :: tokenize_number => eip_tokenize_number
    PROCEDURE, PUBLIC :: set_result_function => eip_set_eval_function
    GENERIC, PUBLIC :: evaluate => evaluate_string, evaluate_stack
    PROCEDURE, PUBLIC :: simplify => eip_simplify
    PROCEDURE, PUBLIC :: minify => eip_minify
    PROCEDURE, PUBLIC :: undefer => eip_undefer
    PROCEDURE, PUBLIC :: emplace => eip_emplace
    PROCEDURE, PUBLIC :: print_errors => eip_print_errors
    PROCEDURE, PUBLIC :: get_error_count => eip_get_error_count
    PROCEDURE, PUBLIC :: get_error_report => eip_get_error_report
    PROCEDURE, PUBLIC :: get_error_info => eip_get_error_info
    PROCEDURE, PUBLIC :: flush_errors => eip_flush_errors
    PROCEDURE, PUBLIC :: get_tokens => eip_get_tokens
    PROCEDURE, PUBLIC :: visualize_stack => eip_visualize_stack
    PROCEDURE, PUBLIC :: visualise_stack => eip_visualize_stack
    PROCEDURE, PUBLIC :: get_symbol_info => eip_get_symbol_info
    PROCEDURE, PUBLIC :: optimise => eip_optimise
    PROCEDURE, PUBLIC :: optimize => eip_optimise
    PROCEDURE, PUBLIC :: get_text => eip_get_text
    PROCEDURE, PUBLIC :: combine_stacks => eip_combine_stacks

    PROCEDURE, PUBLIC :: get_global_symbol_count => eip_get_global_symbol_count
    PROCEDURE, PUBLIC :: get_global_symbol => eip_get_global_symbol
    PROCEDURE, PUBLIC :: get_local_symbol_count => eip_get_local_symbol_count
    PROCEDURE, PUBLIC :: get_local_symbol => eip_get_local_symbol
    PROCEDURE, PUBLIC :: get_symbol_count => eip_get_symbol_count
    PROCEDURE, PUBLIC :: get_symbol => eip_get_symbol
    PROCEDURE, PUBLIC :: namespace_is_included => eip_namespace_is_included
    PROCEDURE, PUBLIC :: symbol_needs_namespace => eip_symbol_needs_namespace
    PROCEDURE, PUBLIC :: get_structure_as_markdown &
        => eip_get_structure_as_markdown

    FINAL :: eip_destructor
  END TYPE eis_parser

  PRIVATE
  PUBLIC :: eis_stack, eis_status_no_simplify, eis_status_no_emplace
  PUBLIC :: eis_parser, stack_holder, parser_holder, interop_parser_count
  PUBLIC :: interop_stack_count, interop_parsers, interop_stacks
  PUBLIC :: eis_get_interop_parser, eis_get_interop_stack
  PUBLIC :: eis_add_interop_parser, eis_add_interop_stack
  PUBLIC :: eis_release_interop_parser, eis_release_interop_stack
  PUBLIC :: eis_fast_evaluate, eis_iter_evaluate

CONTAINS

  PURE ELEMENTAL SUBROUTINE ph_destructor(this)
    TYPE(parser_holder), INTENT(INOUT) :: this
    IF (ASSOCIATED(this%contents) .AND. this%owns) DEALLOCATE(this%contents)
  END SUBROUTINE ph_destructor
  PURE ELEMENTAL SUBROUTINE sh_destructor(this)
    TYPE(stack_holder), INTENT(INOUT) :: this
    IF (ASSOCIATED(this%contents) .AND. this%owns_stack) &
        DEALLOCATE(this%contents)
  END SUBROUTINE sh_destructor
  PURE ELEMENTAL SUBROUTINE eip_destructor(this)
    TYPE(eis_parser), INTENT(INOUT) :: this
    IF (ASSOCIATED(this%err_handler) .AND. this%owns_err_handler) THEN
      DEALLOCATE(this%err_handler)
    END IF
  END SUBROUTINE eip_destructor



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
  !> @param[in] owns
  !> @return eis_add_interop_parser
  FUNCTION eis_add_interop_parser(parser, owns)
    !> Parser to make interoperable
    CLASS(eis_parser), POINTER, INTENT(INOUT) :: parser
    LOGICAL, INTENT(IN), OPTIONAL :: owns
    !> Index of parser after storage
    INTEGER :: eis_add_interop_parser
    TYPE(parser_holder), DIMENSION(:), ALLOCATABLE :: temp

    IF (parser%interop_id > -1) THEN
      eis_add_interop_parser = parser%interop_id
      RETURN
    END IF

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
    parser%interop_id = interop_parser_count
    interop_parsers(interop_parser_count)%contents => parser
    interop_parsers(interop_parser_count)%owns = .FALSE.
    IF (PRESENT(owns)) interop_parsers(interop_parser_count)%owns = owns
    eis_add_interop_parser = interop_parser_count
  END FUNCTION eis_add_interop_parser



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Create an interoperable stack
  !> @param[in] index
  !> @param[in] parser_index
  !> @param[in] owns
  !> @return eis_get_interop_stack
  FUNCTION eis_add_interop_stack(stack, parser_index, owns)
    !> Stack to make interoperable
    TYPE(eis_stack), POINTER, INTENT(INOUT) :: stack
    !> Index of the interoperable parser that generated the stack
    INTEGER, INTENT(IN) :: parser_index
    !> Whether or not the interoperability layer owns the canonical
    !> reference to the stack
    LOGICAL, INTENT(IN), OPTIONAL :: owns
    INTEGER :: eis_add_interop_stack
    TYPE(stack_holder), DIMENSION(:), ALLOCATABLE :: temp

    IF (stack%interop_id > -1) THEN
      eis_add_interop_stack = stack%interop_id
      RETURN
    END IF

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
    stack%interop_id = interop_stack_count
    interop_stacks(interop_stack_count)%contents => stack
    interop_stacks(interop_stack_count)%parser => &
        interop_parsers(parser_index)%contents
    interop_stacks(interop_stack_count)%owns_stack = .FALSE.
    IF (PRESENT(owns)) &
        interop_stacks(interop_stack_count)%owns_stack = owns
    eis_add_interop_stack = interop_stack_count

  END FUNCTION eis_add_interop_stack



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Release an interoperable parser
  !> @param[in] index
  !> @param[in] gone
  SUBROUTINE eis_release_interop_parser(index, gone)
    !> Parser index to release
    INTEGER, INTENT(IN) :: index
    !> Has the parser already been deleted
    LOGICAL, INTENT(IN), OPTIONAL :: gone
    LOGICAL :: is_gone

    is_gone = .FALSE.
    IF (PRESENT(gone)) is_gone = gone

    IF (index < 1 .OR. index > interop_parser_count) RETURN
    IF (index == interop_parser_count) &
        interop_parser_count = interop_parser_count - 1
    IF (.NOT. gone) interop_parsers(index)%contents%interop_id = -1
    IF (interop_parsers(index)%owns) DEALLOCATE(interop_parsers(index)&
        %contents)
    interop_parsers(index)%contents => NULL()

  END SUBROUTINE eis_release_interop_parser



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Release an interoperable stack
  !> @param[in] index
  !> @param[in] gone
  SUBROUTINE eis_release_interop_stack(index, gone)
    !> Stack index to release
    INTEGER, INTENT(IN) :: index
    !Has the stack already been deleted
    LOGICAL, INTENT(IN), OPTIONAL :: gone
    LOGICAL :: is_gone

    is_gone = .FALSE.
    IF (PRESENT(gone)) is_gone = gone

    IF (index < 1 .OR. index > interop_stack_count) RETURN
    IF (index == interop_stack_count) &
        interop_stack_count = interop_stack_count - 1
    IF (.NOT. is_gone) interop_stacks(index)%contents%interop_id = -1
    IF (interop_stacks(index)%owns_stack) THEN
      CALL deallocate_stack(interop_stacks(index)%contents)
      DEALLOCATE(interop_stacks(index)%contents)
      interop_stacks(index)%contents => NULL()
    END IF

  END SUBROUTINE eis_release_interop_stack


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Copy an interoperable stack
  !> @param[in] index
  !> @return eis_copy_interop_stack
  FUNCTION eis_copy_interop_stack(index)
    INTEGER, INTENT(IN) :: index !< Source stack
    INTEGER :: eis_copy_interop_stack !< UID of copied stack
    TYPE(eis_parser), POINTER :: parser
    TYPE(eis_stack), POINTER :: old, new

    eis_copy_interop_stack = -1
    old => eis_get_interop_stack(index, parser = parser)
    IF (ASSOCIATED(old)) THEN
      ALLOCATE(new, SOURCE = old)
      eis_copy_interop_stack = eis_add_interop_stack(new, parser%interop_id, &
          owns = .TRUE.)
    END IF

  END FUNCTION eis_copy_interop_stack


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Initialise a parser
  !> @param[inout] this - Self pointer
  !> @param[inout] errcode - Error code from initialising parser
  !> @param[in] should_simplify - Should the parser autosimplify. Optional,
  !> default .TRUE.
  !> @param[in] should_minify - Should the parser autominify. Optional,
  !> default .FALSE
  !> @param[in] allow_text - Should the parser allow text blocks. Optional
  !> default .FALSE.
  !> @param[in] no_import - Should the parser not import default namepaces
  !> Optional, default .FALSE.
  !> @param[in] physics - What (if any) physics module should be loaded
  !> Optional, default no physics
  !> @param[in] err_handler - Pass in a pointer to an externally supplied error
  !> handler
  SUBROUTINE eip_init(this, errcode, should_simplify, should_minify, &
      allow_text, no_import, physics, err_handler)

    CLASS(eis_parser) :: this
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    LOGICAL, INTENT(IN), OPTIONAL :: should_simplify, should_minify, no_import
    LOGICAL, INTENT(IN), OPTIONAL :: allow_text
    INTEGER, INTENT(IN), OPTIONAL :: physics
    CLASS(eis_error_handler), POINTER, INTENT(IN), OPTIONAL :: err_handler
    INTEGER(eis_error) :: err
    REAL(eis_num), PARAMETER :: pi = 3.141592653589793238462643383279503_eis_num
    REAL(eis_num) :: c
    LOGICAL :: no_import_l

    IF (this%owns_err_handler .AND. ASSOCIATED(this%err_handler)) &
        DEALLOCATE(this%err_handler)

    !If you specify a error handler then that error handler is used
    !If you specify a NULL error handler than an error handler is created
    !If you specify no error handler then an error handler is created
    !unless there is already an error handler in place in which case it is left
    !unchanged
    IF (PRESENT(err_handler)) THEN
      IF (ASSOCIATED(err_handler)) THEN
        this%err_handler => err_handler
        this%owns_err_handler = .FALSE.
      ELSE IF (ASSOCIATED(this%err_handler)) THEN
        !Do nothing
      ELSE
        ALLOCATE(this%err_handler)
        this%owns_err_handler = .TRUE.
      END IF
    ELSE
      ALLOCATE(this%err_handler)
      this%owns_err_handler = .TRUE.
    END IF

    no_import_l = .FALSE.
    IF (PRESENT(should_simplify)) this%should_simplify = should_simplify
    IF (PRESENT(should_minify)) this%should_minify = should_minify
    IF (PRESENT(physics)) this%physics_units = physics
    IF (PRESENT(no_import)) no_import_l = no_import
    IF (PRESENT(allow_text)) this%allow_text = allow_text

    CALL this%err_handler%init(errcode)
    IF (this%is_init) RETURN
    this%is_init = .TRUE.

    IF (.NOT. global_setup) THEN
      err = eis_err_none
      global_setup = .TRUE.
      CALL global_registry%add_operator('+', eis_uplus, eis_assoc_ra, 4, err, &
        unary = .TRUE., err_handler = this%err_handler, &
        description = 'Unary plus')
      CALL global_registry%add_operator('-', eis_uminus, eis_assoc_ra, 4, err, &
          unary = .TRUE., err_handler = this%err_handler, &
          description = 'Unary minus')
      CALL global_registry%add_operator('+', eis_bplus, eis_assoc_a, 2, err, &
          err_handler = this%err_handler, description = 'Addition operator')
      CALL global_registry%add_operator('-', eis_bminus, eis_assoc_la, 2, err, &
          err_handler = this%err_handler, description = 'Subtraction operator')
      CALL global_registry%add_operator('*', eis_times, eis_assoc_a, 3, err, &
          err_handler = this%err_handler, &
          description = 'Multiplication operator')
      CALL global_registry%add_operator('/', eis_divide, eis_assoc_la, 3, err, &
          err_handler = this%err_handler, description = 'Division operator')
      CALL global_registry%add_operator('^', eis_pow, eis_assoc_ra, 4, err, &
          err_handler = this%err_handler, description = 'Raise to power &
          &operator')
      CALL global_registry%add_operator('e', eis_expo, eis_assoc_la, 4, err, &
          err_handler = this%err_handler, description = 'Exponentiation &
          &operator')
      CALL global_registry%add_operator('lt', eis_lt, eis_assoc_la, 1, err, &
          err_handler = this%err_handler, description = 'less than operator')
      CALL global_registry%add_operator('<', eis_lt, eis_assoc_la, 1, err, &
          err_handler = this%err_handler, description = 'less than operator')
      CALL global_registry%add_operator('le', eis_le, eis_assoc_la, 1, err, &
          err_handler = this%err_handler, description = 'less than or equal to &
          &operator')
      CALL global_registry%add_operator('<=', eis_le, eis_assoc_la, 1, err, &
          err_handler = this%err_handler, description = 'less than or equal to &
          &operator')
      CALL global_registry%add_operator('gt', eis_gt, eis_assoc_la, 1, err, &
          err_handler = this%err_handler, description = 'greater than operator')
      CALL global_registry%add_operator('>', eis_gt, eis_assoc_la, 1, err, &
          err_handler = this%err_handler, description = 'greater than operator')
      CALL global_registry%add_operator('ge', eis_ge, eis_assoc_la, 1, err, &
          err_handler = this%err_handler, description = 'greater than or equal &
          &to operator')
      CALL global_registry%add_operator('>=', eis_ge, eis_assoc_la, 1, err, &
          err_handler = this%err_handler, description = 'greater than or equal &
          &to operator')
      CALL global_registry%add_operator('eq', eis_eq, eis_assoc_la, 1, err, &
          err_handler = this%err_handler, description = 'equal to operator')
      CALL global_registry%add_operator('==', eis_eq, eis_assoc_la, 1, err, &
          err_handler = this%err_handler, description = 'equal to operator')
      CALL global_registry%add_operator('ne', eis_neq, eis_assoc_la, 1, err, &
          err_handler = this%err_handler, description = 'not equal to operator')
      CALL global_registry%add_operator('/=', eis_neq, eis_assoc_la, 1, err, &
          err_handler = this%err_handler, description = 'not equal to operator')
      CALL global_registry%add_operator('!=', eis_neq, eis_assoc_la, 1, err, &
          err_handler = this%err_handler, description = 'not equal to operator')
      CALL global_registry%add_operator('and', eis_and, eis_assoc_la, 0, err, &
          err_handler = this%err_handler, description = 'logical and')
      CALL global_registry%add_operator('or', eis_or, eis_assoc_la, 0, err, &
          err_handler = this%err_handler, description = 'logical or')

      CALL global_registry%add_constant('math.pi', pi, err, &
          err_handler = this%err_handler, description = 'Pi, the ratio of a &
          &circle''s circumference to it''s diameter')

      CALL global_registry%add_constant('scale.yotta', 1.0e24_eis_num, err, &
          err_handler = this%err_handler, description = '10^24')
      CALL global_registry%add_constant('scale.zetta', 1.0e21_eis_num, err, &
          err_handler = this%err_handler, description = '10^21')
      CALL global_registry%add_constant('scale.exa', 1.0e18_eis_num, err, &
          err_handler = this%err_handler, description = '10^18')
      CALL global_registry%add_constant('scale.peta', 1.0e15_eis_num, err, &
          err_handler = this%err_handler, description = '10^15')
      CALL global_registry%add_constant('scale.tera', 1.0e12_eis_num, err, &
          err_handler = this%err_handler, description = '10^12')
      CALL global_registry%add_constant('scale.giga', 1.0e9_eis_num, err, &
          err_handler = this%err_handler, description = '10^9')
      CALL global_registry%add_constant('scale.mega', 1.0e6_eis_num, err, &
          err_handler = this%err_handler, description = '10^6')
      CALL global_registry%add_constant('scale.kilo', 1.0e3_eis_num, err, &
          err_handler = this%err_handler, description = '10^3')
      CALL global_registry%add_constant('scale.hecto', 1.0e2_eis_num, err, &
          err_handler = this%err_handler, description = '10^2')
      CALL global_registry%add_constant('scale.deca', 1.0e1_eis_num, err, &
          err_handler = this%err_handler, description = '10^1')
      CALL global_registry%add_constant('scale.deci', 1.0e-1_eis_num, err, &
          err_handler = this%err_handler, description = '10^-1')
      CALL global_registry%add_constant('scale.centi', 1.0e-2_eis_num, err, &
          err_handler = this%err_handler, description = '10^-2')
      CALL global_registry%add_constant('scale.milli', 1.0e-3_eis_num, err, &
          err_handler = this%err_handler, description = '10^-3')
      CALL global_registry%add_constant('scale.micro', 1.0e-6_eis_num, err, &
          err_handler = this%err_handler, description = '10^-6')
      CALL global_registry%add_constant('scale.nano', 1.0e-9_eis_num, err, &
          err_handler = this%err_handler, description = '10^-9')
      CALL global_registry%add_constant('scale.pico', 1.0e-12_eis_num, err, &
          err_handler = this%err_handler, description = '10^-12')
      CALL global_registry%add_constant('scale.femto', 1.0e-15_eis_num, err, &
          err_handler = this%err_handler, description = '10^-15')
      CALL global_registry%add_constant('scale.atto', 1.0e-18_eis_num, err, &
          err_handler = this%err_handler, description = '10^-18')
      CALL global_registry%add_constant('scale.zepto', 1.0e-21_eis_num, err, &
          err_handler = this%err_handler, description = '10^-21')
      CALL global_registry%add_constant('scale.yocto', 1.0e-24_eis_num, err, &
          err_handler = this%err_handler, description = '10^-24')

      CALL global_registry%add_function('math.abs', eis_abs, 1, err, &
          err_handler = this%err_handler, description = '`abs(a)` - Returns &
          &absolute value of a')
      CALL global_registry%add_function('math.floor', eis_floor, 1, err, &
          err_handler = this%err_handler, description = '`floor(a)` - Returns &
          &the nearest integer to a rounding towards zero')
      CALL global_registry%add_function('math.ceil', eis_ceil, 1, err, &
          err_handler = this%err_handler, description = '`ceil(a)` - Returns &
          &the nearest integer to a rounding away from zero')
      CALL global_registry%add_function('math.ceiling', eis_ceil, 1, err, &
          err_handler = this%err_handler, description = '`ceiling(a)` - &
          &Returns the nearest integer to a rounding away from zero')
      CALL global_registry%add_function('math.nint', eis_nint, 1, err, &
          err_handler = this%err_handler, description = '`nint(a)` - Returns &
          &the nearest integer to a rounding to nearest integer')
      CALL global_registry%add_function('math.trunc', eis_aint, 1, err, &
          err_handler = this%err_handler, description = '`trunc(a)` - Returns &
          &the nearest integer to a by ignoring the non-integer part')
      CALL global_registry%add_function('math.truncate', eis_aint, 1, err, &
          err_handler = this%err_handler, description = '`truncate(a)` - &
          &Returns the nearest integer to a by ignoring the non-integer part')
      CALL global_registry%add_function('math.aint', eis_aint, 1, err, &
          err_handler = this%err_handler, description = '`aint(a)` - Returns &
          &the nearest integer to a by ignoring the non-integer part')
      CALL global_registry%add_function('math.sqrt', eis_sqrt, 1, err, &
          err_handler = this%err_handler, description = '`sqrt(a)` - Returns &
          &the square root of a')
      CALL global_registry%add_function('math.sin', eis_sin, 1, err, &
          err_handler = this%err_handler, description = '`sin(a)` - Returns &
          &the sine of a, specifying a in radians')
      CALL global_registry%add_function('math.cos', eis_cos, 1, err, &
          err_handler = this%err_handler, description = '`cos(a)` - Returns &
          &the cosine of a, specifying a in radians')
      CALL global_registry%add_function('math.tan', eis_tan, 1, err, &
          err_handler = this%err_handler, description = '`tan(a)` - Returns &
          &the tangent of a, specifying a in radians')
      CALL global_registry%add_function('math.asin', eis_asin, 1, err, &
          err_handler = this%err_handler, description = '`asin(a)` - Returns &
          &the inverse sin of a, specifying the resulting angle in radians')
      CALL global_registry%add_function('math.acos', eis_acos, 1, err, &
          err_handler = this%err_handler, description = '`acos(a)` - Returns &
          &the inverse cosin of a, specifying the resulting angle in radians')
      CALL global_registry%add_function('math.atan', eis_atan, 1, err, &
          err_handler = this%err_handler, description = '`atan(a)` - Returns &
          &the inverse tangent of a, specifying the resulting angle in radians')
      CALL global_registry%add_function('math.atan2', eis_atan2, 2, err, &
          err_handler = this%err_handler, description = '`atan2(y, x)` - 2 &
          &argument arctangent. Returns the angle in radians between the &
          &positive x axis and the line joining the origin to the point (x,y)')
      CALL global_registry%add_function('math.sinh', eis_sinh, 1, err, &
          err_handler = this%err_handler, description = '`sinh(a)` - Returns &
          &the hyperbolic sine of a')
      CALL global_registry%add_function('math.cosh',eis_cosh, 1, err, &
          err_handler = this%err_handler, description = '`cosh(a)` - Returns &
          &the hyperbolic cosine of a')
      CALL global_registry%add_function('math.tanh',eis_tanh, 1, err, &
          err_handler = this%err_handler, description = '`tanh(a)` - Returns &
          &the hyperbolic tangent of a')
      CALL global_registry%add_function('math.asinh', eis_asinh, 1, err, &
          err_handler = this%err_handler, description = '`asinh(a)` - Returns &
          &xthe inverse hyperbolic sine of a')
      CALL global_registry%add_function('math.acosh',eis_acosh, 1, err, &
          err_handler = this%err_handler, description = '`acosh(a)` - Returns &
          &the inverse hyperbolic cosine of a')
      CALL global_registry%add_function('math.atanh',eis_atanh, 1, err, &
          err_handler = this%err_handler, description = '`atanh(a)` - Returns &
          &the inverse hyperbolic tangent of a')
      CALL global_registry%add_function('math.exp', eis_exp, 1, err, &
          err_handler = this%err_handler, description = '`exp(a)` - Returns &
          &the base e exponential of a')
      CALL global_registry%add_function('math.loge', eis_loge, 1, err, &
          err_handler = this%err_handler, description ='`loge(a)` - Returns &
          &the base e logarithm of a')
      CALL global_registry%add_function('math.log10', eis_log10, 1, err, &
          err_handler = this%err_handler, description ='`log10(a)` - Returns &
          &the base 10 logarithm of a')
      CALL global_registry%add_function('math.log_base', eis_log_base, 2, err, &
          err_handler = this%err_handler, description ='`log_base(value, &
          &base)` Returns the base "base" logarithm of "value"')
#ifdef F2008
      CALL global_registry%add_function('math.bessel_j', eis_bessel_j, 2, err, &
          err_handler = this%err_handler, description = '`bessel_j(n, x)` - &
          &Returns the Bessel function of the first kind of order n of x')
      CALL global_registry%add_function('math.bessel_y', eis_bessel_y, 2, err, &
          err_handler = this%err_handler, description  = '`bessel_y(n, x)` - &
          &Returns the Bessel function of the second kind of order n of x')
      CALL global_registry%add_function('math.erf', eis_erf, 1, err, &
          err_handler = this%err_handler, description = '`erf(x)` - Return &
          &the error function of x')
      CALL global_registry%add_function('math.erfc', eis_erfc, 1, err, &
          err_handler = this%err_handler, description = '`erfc(x)` - Return &
          &the complementary error function of x')
      CALL global_registry%add_function('math.erfc_scaled', eis_erfc_scaled, &
          1, err, err_handler = this%err_handler, description = &
          '`erfc_scaled(x)` - Returns the exponentially scaled complementary &
          &error function of x')
      CALL global_registry%add_function('math.gamma', eis_gamma_fn, &
          1, err, err_handler = this%err_handler, description = '`gamma(x)` - &
          &Returns the Bernoulli gamma function of x')
      CALL global_registry%add_function('math.log_gamma', eis_log_gamma_fn, &
          1, err, err_handler = this%err_handler, description = '`log_gamma&
          &(x)` - Returns the logarithm of the gamma function of x' )
#endif

      CALL global_registry%add_function('utility.gauss', eis_gauss, 3, err, &
          err_handler = this%err_handler, description = '`gauss(x,x0,w)` - &
          &Gaussian of the form exp(((x-x0)/w)^2). The FWHM is 2wsqrt(ln(2))')
      CALL global_registry%add_function('utility.semigauss', eis_semigauss, 4, &
          err, err_handler = this%err_handler, description = '`semigauss(x, A, &
          &A0, w)`. Gives a semi-Gaussian profile in the variable x. The &
          &other parameters are the maximum value of the profile (A), the value&
          & of the function at x=0 (A0) and the characteristic rise of the &
          &function')
      CALL global_registry%add_function('utility.supergauss', eis_supergauss, &
          4, err, err_handler = this%err_handler, description = '`supergauss(&
          &x,x0,w,n)` - Returns a superGaussian function of the form &
          &exp(((x-x0)/w)^n)')
      CALL global_registry%add_function('utility.interpolate', eis_interpol, &
          -1, err, err_handler = this%err_handler, description = 'interpolate(&
          &p, (x0, y0), (x1, y1), ...) - get a value from a specified &
          &interpolation function. Specify a point p and a series of control &
          &points as (x,y) pairs. The nearest x points to p will be selected &
          &and the corresponding y value will be selected by linear &
          &interpolation')

      CALL global_registry%add_function('logic.if', eis_if, &
          3, err, err_handler = this%err_handler, description = '`if(a,b,c)` - &
          & If a is not equal to zero returns b, otherwise returns c')

      !Unit indepdendent physical constants
      CALL global_registry%add_constant('physics.na', &
          6.02214076e23_eis_num, err, err_handler = this%err_handler, &
          description = 'Avogadro''s constant')

      !SI Physical constants
      CALL global_registry%add_constant('physics.si.c', 2.99792458e8_eis_num, &
          err, err_handler = this%err_handler, description = 'Speed of light &
          &(m s^-1)')
      CALL global_registry%add_constant('physics.si.G', 6.67408e-11_eis_num, &
          err, err_handler = this%err_handler, description = 'Gravitational &
          &constant (m^3 kg^-1 s^-2)')
      CALL global_registry%add_constant('physics.si.h', &
          6.62607015e-34_eis_num, err, err_handler = this%err_handler, &
          description = 'Planck''s constant (m^2 kg s^-2)')
      CALL global_registry%add_constant('physics.si.q0', &
          1.602176565e-19_eis_num, err, err_handler = this%err_handler, &
          description = 'Elementary charge (C)')
      CALL global_registry%add_constant('physics.si.epsilon0', &
          8.854187817620389850536563031710750e-12_eis_num, err, &
          err_handler = this%err_handler, description = 'Vacuum &
          &permittivity (F m^-1)')
      CALL global_registry%add_constant('physics.si.mu0', &
          4.e-7_eis_num * pi, err, err_handler = this%err_handler, &
          description = 'Vacuum permeability (H m^-1)')
      CALL global_registry%add_constant('physics.si.kb', &
          1.3806488e-23_eis_num, err, err_handler = this%err_handler, &
          description = 'Boltzmann''s constant (m^2 kg s^-2 K^-1)')
      CALL global_registry%add_constant('physics.si.me', &
          9.10938291e-31_eis_num, err, err_handler = this%err_handler, &
          description = 'Mass of electron (kg)')
      CALL global_registry%add_constant('physics.si.mp', &
          1.6726219e-27_eis_num, err, err_handler = this%err_handler, &
          description = 'Mass of proton (kg)')
      CALL global_registry%add_constant('physics.si.mn', &
          1.674929e-27_eis_num, err, err_handler = this%err_handler, &
          description = 'Mass of neutron (kg)')

      !CGS physical constants
      c = 2.99792458e10_eis_num
      CALL global_registry%add_constant('physics.cgs.gauss.c', &
          c, err, err_handler = this%err_handler, &
          description = 'Speed of light (cm s^-1)')
      CALL global_registry%add_constant('physics.cgs.gauss.G', &
          6.67428e-8_eis_num, err, err_handler = this%err_handler, &
          description = 'Gravitational constant (cm^3 g^-1 s^-2)')
      CALL global_registry%add_constant('physics.cgs.gauss.h', &
          6.62606885e-27_eis_num, err, err_handler = this%err_handler, &
          description = 'Planck''s constant (erg s)')
      CALL global_registry%add_constant('physics.cgs.gauss.q0', &
          4.80320427e-10_eis_num, err, err_handler = this%err_handler, &
          description = 'Elementary charge (Fr)')
      CALL global_registry%add_constant('physics.cgs.gauss.epsilon0', &
          1.0_eis_num, err, err_handler = this%err_handler, &
          description = 'Vacuum permittivity (unnecessary)')
      CALL global_registry%add_constant('physics.cgs.gauss.mu0', &
          1.0_eis_num/c**2, err, err_handler = this%err_handler, &
          description = 'Vacuum permiability (unnecessary)')
      CALL global_registry%add_constant('physics.cgs.gauss.kb', &
          1.3806504e-16_eis_num, err, err_handler = this%err_handler, &
          description = 'Boltzmann''s constant (erg K^-1)')
      CALL global_registry%add_constant('physics.cgs.gauss.mu', &
          1.67377e-27_eis_num, err, err_handler = this%err_handler)
      CALL global_registry%add_constant('physics.cgs.gauss.me', &
          9.10938215e-28_eis_num, err, err_handler = this%err_handler, &
          description = 'Mass of electrion (g)')
      CALL global_registry%add_constant('physics.cgs.gauss.mp', &
          1.6726219e-24_eis_num, err, err_handler = this%err_handler, &
          description = 'Mass of proton (g)')
      CALL global_registry%add_constant('physics.cgs.gauss.mn', &
          1.674929e-24_eis_num, err, err_handler = this%err_handler, &
          description = 'Mass of neutron (g)')

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
  !> @param[in] this
  !> @param[in] symbol
  !> @param[in] unary
  !> @param[out] symbol_type
  !> @param[out] cap_bits
  !> @param[out] deferred
  !> @param[out] description
  !> @param[out] is_hidden
  !> @param[out] expected_params
  !> @return exists
  FUNCTION eip_get_symbol_info(this, symbol, unary, symbol_type, cap_bits, &
      deferred, description, is_hidden, expected_params) RESULT(exists)

    CLASS(eis_parser), INTENT(INOUT) :: this
    !> Name of the symbol to get information about
    CHARACTER(LEN=*), INTENT(IN) :: symbol
    !> Is this a unary operator
    LOGICAL, INTENT(IN), OPTIONAL :: unary
    !> Type of the found symbol
    INTEGER, INTENT(OUT), OPTIONAL :: symbol_type
    !> Cap bits of the found symbol
    INTEGER(eis_bitmask), INTENT(OUT), OPTIONAL :: cap_bits
    !> Is this symbol currently deferred
    LOGICAL, INTENT(OUT), OPTIONAL :: deferred
    !> Description associated with the symbol
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT), OPTIONAL :: description
    !> Is this symbol meant to be hidden
    LOGICAL, INTENT(OUT), OPTIONAL :: is_hidden
    !> Number of expected parameters
    INTEGER, INTENT(OUT), OPTIONAL :: expected_params
    !> Does the symbol exist in the symbol list
    LOGICAL :: exists
    TYPE(eis_stack_element) :: iblock
    TYPE(eis_stack_co_element):: icoblock
    LOGICAL :: is_unary
    CHARACTER(LEN=:), ALLOCATABLE :: str

    is_unary = .FALSE.
    IF (PRESENT(unary)) is_unary = unary

    IF (.NOT. is_unary) THEN
      CALL this%registry%fill_block(symbol, iblock, icoblock, is_unary, &
          description = str, hidden = is_hidden)
      IF (iblock%ptype == eis_pt_bad) THEN
        CALL global_registry%fill_block(symbol, iblock, icoblock, is_unary, &
            description = str, hidden = is_hidden)
      END IF
      IF (iblock%ptype == eis_pt_bad) is_unary = .TRUE.
    END IF

    IF (is_unary) THEN
      CALL this%registry%fill_block(symbol, iblock, icoblock, is_unary, &
          description = str, hidden = is_hidden)
      IF (iblock%ptype == eis_pt_bad) THEN
        CALL global_registry%fill_block(symbol, iblock, icoblock, is_unary, &
            description = str, hidden = is_hidden)
      END IF
    END IF

    exists = iblock%ptype /= eis_pt_bad
    IF (ALLOCATED(str) .AND. PRESENT(description)) ALLOCATE(description, &
        SOURCE = str)
    IF (ALLOCATED(str)) DEALLOCATE(str)
    IF (PRESENT(symbol_type)) symbol_type = iblock%ptype
    IF (PRESENT(cap_bits)) cap_bits = icoblock%cap_bits
    IF (PRESENT(deferred)) deferred = &
        (iblock%ptype == eis_pt_deferred_variable &
        .OR. iblock%ptype == eis_pt_deferred_function)
    IF (PRESENT(expected_params)) expected_params = icoblock%expected_params

  END FUNCTION eip_get_symbol_info



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to test if a given namespace has been included
  !> @param[in] this
  !> @param[in] namespace
  !> @param[in] in_global
  !> @return eip_namespace_is_included
  FUNCTION eip_namespace_is_included(this, namespace, in_global)

    CLASS(eis_parser), INTENT(INOUT) :: this
    !> Name of the namespace to get information about
    CHARACTER(LEN=*), INTENT(IN) :: namespace
    !> Should the test be in the global registry
    LOGICAL, INTENT(IN), OPTIONAL :: in_global
    LOGICAL :: eip_namespace_is_included !< Is this namespace included?
    LOGICAL :: use_global

    use_global = .FALSE.
    IF (PRESENT(in_global)) use_global = in_global

    IF (use_global) THEN
      eip_namespace_is_included = global_registry%is_included(namespace)
    ELSE
      eip_namespace_is_included = this%registry%is_included(namespace)
    END IF
    
  END FUNCTION eip_namespace_is_included



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to test if a given symbol specified with full namespace
  !> qualification needs the namespace to resolve.
  !> @param[in] this
  !> @param[in] symbol
  !> @param[in] in_global
  !> @return eip_symbol_needs_namespace
  FUNCTION eip_symbol_needs_namespace(this, symbol, in_global)

    CLASS(eis_parser), INTENT(INOUT) :: this
    !> Name of the symbol
    CHARACTER(LEN=*), INTENT(IN) :: symbol
    !> Should the test be in the global registry
    LOGICAL, INTENT(IN), OPTIONAL :: in_global
    LOGICAL :: eip_symbol_needs_namespace !< Does this symbol need the namespace
    LOGICAL :: use_global
    INTEGER :: ldotloc

    ldotloc = INDEX(symbol,'.', back = .TRUE.)
    IF (ldotloc == 0 .OR. ldotloc == LEN(symbol)) THEN
      eip_symbol_needs_namespace = .FALSE.
    ELSE
      use_global = .FALSE.
      IF (PRESENT(in_global)) use_global = in_global

      IF (use_global) THEN
        eip_symbol_needs_namespace = &
            .NOT. global_registry%is_included(symbol(1:ldotloc-1))
      ELSE
        eip_symbol_needs_namespace = &
            .NOT. this%registry%is_included(symbol(1:ldotloc-1))
      END IF
    END IF

  END FUNCTION eip_symbol_needs_namespace



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get the number of symbols in a specified registry
  !> @param[in] this
  !> @param[in] registry
  !> @return symbol_count
  FUNCTION eip_get_registry_symbol_count(this, registry) &
      RESULT(symbol_count)

    CLASS(eis_parser), INTENT(INOUT) :: this
    !> Registry to get the symbol for
    CLASS(eis_registry), INTENT(INOUT) :: registry
    !> Number of symbols
    INTEGER :: symbol_count

    symbol_count = registry%get_name_count()

  END FUNCTION eip_get_registry_symbol_count



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get a symbol from the registry
  !> @param[in] this
  !> @param[in] registry
  !> @param[in] index
  !> @param[out] symbol
  !> @return symbol_count
  SUBROUTINE eip_get_registry_symbol(this, registry, index, symbol)

    CLASS(eis_parser), INTENT(INOUT) :: this
    !> Registry to get the symbol from
    CLASS(eis_registry), INTENT(INOUT) :: registry
    !> Index of the symbol to retrieve
    INTEGER, INTENT(IN) :: index
    !> Retreived symbol
    CHARACTER(LEN=:), INTENT(OUT), ALLOCATABLE :: symbol

    CALL registry%get_name(index, symbol)

  END SUBROUTINE eip_get_registry_symbol



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to load a given block with information from the registry
  !> @param[inout] this
  !> @param[in] name
  !> @param[out] iblock
  !> @param[out] icoblock
  !> @param[in] unary
  SUBROUTINE eip_load_block(this, name, iblock, icoblock, unary)

    CLASS(eis_parser), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name !< Name to look up in registry
    !> Block to fill with registry information
    TYPE(eis_stack_element), INTENT(OUT) :: iblock
    !> Coblock to fill with registry information
    TYPE(eis_stack_co_element), INTENT(OUT) :: icoblock
    !> Can this be a unary operator
    LOGICAL, INTENT(IN), OPTIONAL :: unary
    INTEGER(eis_i8) :: work
    REAL(eis_num) :: value
    LOGICAL :: can_be_unary
    INTEGER :: slen

    iblock%ptype = eis_pt_bad
    iblock%value = 0
    iblock%numerical_data = 0.0_eis_num
    IF (ALLOCATED(icoblock%text)) DEALLOCATE(icoblock%text)
    ALLOCATE(icoblock%text, SOURCE = TRIM(name))
    work = 0_eis_i8

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

    IF (.NOT. PRESENT(unary)) THEN
      can_be_unary = .NOT. (this%last_block_type == eis_pt_variable &
            .OR. this%last_block_type == eis_pt_pointer_variable &
            .OR. this%last_block_type == eis_pt_constant &
            .OR. this%last_block_type == eis_pt_stored_variable &
            .OR. this%last_block_type == eis_pt_function &
            .OR. this%last_block_type == eis_pt_emplaced_function &
            .OR. (this%last_block_type == eis_pt_parenthesis &
            .AND. this%last_block_value == eis_paren_right_bracket))
    ELSE
      can_be_unary = unary
    END IF

    CALL this%registry%fill_block(name, iblock, icoblock, can_be_unary)
    IF (iblock%ptype /= eis_pt_bad) RETURN

    CALL global_registry%fill_block(name, iblock, icoblock, can_be_unary)
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
  !> Function to set a stack's eval_fn to a specified function
  !> @param[inout] this
  !> @param[in] function_in
  !> @param[inout] output
  !> @param[inout] err
  !> @param[in] filename
  !> @param[in] line_number
  !> @param[in] char_offset
  !> @param[in] cap_bits
  SUBROUTINE eip_set_eval_function(this, function_in, output, err, &
      filename, line_number, char_offset, cap_bits)

    CLASS(eis_parser) :: this
    !> Function to be called when the stack is evaluated
    PROCEDURE(parser_result_function) :: function_in
    !> Stack to contain the output. If stack is not empty then new values
    !> are pushed to the end of the stack. This will usually cause multi valued
    !> results
    TYPE(eis_stack), INTENT(INOUT) :: output
    !> Error code for errors during tokenize
    INTEGER(eis_error), INTENT(INOUT) :: err
    !> The filename of the file containing the expression
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: filename
    !> The line number of the expression within the file
    INTEGER, INTENT(IN), OPTIONAL :: line_number
    !> The character offset of the expression within the line
    INTEGER, INTENT(IN), OPTIONAL :: char_offset
    !> Capability bits for the expression
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits

    CALL initialise_stack(output)
    output%eval_fn => function_in
    IF (PRESENT(filename)) ALLOCATE(output%filename, SOURCE = filename)
    IF (PRESENT(line_number)) THEN
      output%line_number = line_number
    ELSE
      output%line_number = -1
    END IF
    IF (PRESENT(char_offset)) THEN
      output%char_offset = char_offset
    ELSE
      output%char_offset = 0
    END IF
    IF (PRESENT(cap_bits)) THEN
      output%cap_bits = cap_bits
    ELSE
      output%cap_bits = 0_eis_bitmask
    END IF

  END SUBROUTINE eip_set_eval_function


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to tokenize an expression to a stack
  !> @param[inout] this
  !> @param[in] expression_in
  !> @param[inout] output
  !> @param[inout] err
  !> @param[in] simplify
  !> @param[in] minify
  !> @param[in] filename
  !> @param[in] line_number
  !> @param[in] char_offset
  !> @param[in] append
  !> @param[in] allow_text
  SUBROUTINE eip_tokenize(this, expression_in, output, err, simplify, minify, &
      filename, line_number, char_offset, append, allow_text)

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
    !> The filename of the file containing the expression. Optional, default
    !> do not use filename when reporting error
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: filename
    !> The line number of the expression within the file. Optional, default
    !> do not use line number when reporting error
    INTEGER, INTENT(IN), OPTIONAL :: line_number
    !> The character offset of the expression within the line. Optional,
    !> default 0
    INTEGER, INTENT(IN), OPTIONAL :: char_offset
    !> Whether the newly parsed value should be appended to
    !> the values in the stack or no. Optional, default false
    LOGICAL, INTENT(IN), OPTIONAL :: append
    !> Should the parser allow text blocks. Optional, default false
    LOGICAL, INTENT(IN), OPTIONAL :: allow_text
    LOGICAL :: maybe_e, should_simplify, should_minify, should_dealloc
    CHARACTER(LEN=:), ALLOCATABLE :: current, expression
    INTEGER :: current_type, current_pointer, i, ptype
    INTEGER(eis_bitmask) :: cap_bits
    INTEGER :: charindex
    LOGICAL :: atext

    TYPE(eis_stack_element) :: iblock
    TYPE(eis_stack_co_element) :: icoblock

    should_minify = this%should_minify
    IF (PRESENT(minify)) should_minify = minify
    should_simplify = this%should_simplify
    IF (PRESENT(simplify)) should_simplify = simplify
    atext = this%allow_text
    IF (PRESENT(allow_text)) atext = allow_text

    IF (.NOT. this%is_init) CALL this%init(err)
    should_dealloc = .TRUE.
    IF (PRESENT(append)) should_dealloc = .NOT. append
    IF (ALLOCATED(output%filename)) DEALLOCATE(output%filename)

    IF (PRESENT(filename)) ALLOCATE(output%filename, SOURCE = filename)
    IF (PRESENT(line_number)) THEN
      output%line_number = line_number
    ELSE
      output%line_number = -1
    END IF
    IF (PRESENT(char_offset)) THEN
      output%char_offset = char_offset
    ELSE
      output%char_offset = 0
    END IF

    IF (ASSOCIATED(output%eval_fn)) NULLIFY(output%eval_fn)
    IF (should_dealloc) CALL deallocate_stack(output)
    IF (.NOT. output%init) CALL initialise_stack(output)

    ALLOCATE(expression, SOURCE = TRIM(expression_in))

    CALL initialise_stack(this%stack)
    CALL initialise_stack(this%brackets)

    ALLOCATE(CHARACTER(LEN=LEN(expression))::current)
    IF (ALLOCATED(output%full_line)) DEALLOCATE(output%full_line)
    ALLOCATE(output%full_line, SOURCE = expression)

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
            cap_bits, charindex + output%char_offset, charindex, output, err, &
            filename, line_number, expression, atext)
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
            == eis_pt_constant) .OR. (iblock%ptype == eis_pt_pointer_variable)
      END IF
    END DO

    CALL this%tokenize_subexpression_infix(current, iblock, icoblock, &
        cap_bits, charindex + output%char_offset,  charindex, output, err, &
        filename, line_number, expression, atext)
    output%cap_bits = IOR(output%cap_bits, cap_bits)

    IF (err == eis_err_none) THEN
      DO i = this%stack%stack_point, 1, -1
        IF (this%stack%entries(i)%ptype == eis_pt_function) THEN
          IF (this%stack%co_entries(i)%expected_params > 0) THEN
            err = IOR(err, eis_err_wrong_parameters)
            CALL this%err_handler%add_error(eis_err_parser, err, &
                this%stack%co_entries(i)%text, &
                this%stack%co_entries(i)%charindex, &
                filename = filename, line_number = line_number)
          ELSE
            this%stack%entries(i)%actual_params = 0
          END IF
        END IF
        CALL pop_to_stack(this%stack, output)
      END DO
    ELSE
      err = IOR(err, eis_err_parser)
    END IF
    CALL deallocate_stack(this%stack)
    CALL deallocate_stack(this%brackets)
    DEALLOCATE(current)
    DEALLOCATE(expression)

    IF (err /= eis_err_none) RETURN

    IF (should_simplify) CALL this%simplify(output, err, &
        host_params = C_NULL_PTR)
    IF (should_minify) CALL this%minify(output, err)

  END SUBROUTINE eip_tokenize



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to tokenize a numer to a stack
  !> @param[inout] this
  !> @param[in] value_in
  !> @param[inout] output
  !> @param[inout] err
  !> @param[in] append
  SUBROUTINE eip_tokenize_number(this, value_in, output, err, append)

    CLASS(eis_parser) :: this
    !> Numerical value to convert to stack
    REAL(eis_num), INTENT(IN) :: value_in
    !> Stack to contain the output. If stack is not empty then new values
    !> are pushed to the end of the stack. This will usually cause multi valued
    !> results
    TYPE(eis_stack), INTENT(INOUT)  :: output
    !> Error code for errors during tokenize
    INTEGER(eis_error), INTENT(INOUT) :: err
    !> Whether the newly parsed value should be appended to
    !> the values in the stack or no. Optional, default false
    LOGICAL, INTENT(IN), OPTIONAL :: append
    LOGICAL :: should_dealloc
    TYPE(eis_stack_element) :: iblock
    TYPE(eis_stack_co_element) :: icoblock

    IF (PRESENT(append)) should_dealloc = .NOT. append
    IF (should_dealloc) CALL deallocate_stack(output)
    IF (.NOT. output%init) CALL initialise_stack(output)

    iblock%ptype = eis_pt_constant
    iblock%value = 0
    iblock%numerical_data = value_in
    ALLOCATE(CHARACTER(LEN=25)::icoblock%text)
    WRITE(icoblock%text, '(ES25.17E3)') value_in

    CALL push_to_stack(output, iblock, icoblock)


  END SUBROUTINE eip_tokenize_number





  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to evaluate a stack
  !> @param[inout] this
  !> @param[inout] stack
  !> @param[inout] errcode
  !> @param[in] host_params
  !> @return eip_evaluate_stack
  FUNCTION eip_evaluate_stack(this, stack, result, errcode, host_params)
    CLASS(eis_parser) :: this
    CLASS(eis_stack), INTENT(INOUT) :: stack !< Stack to evaluate
    !> Allocatable array holding all the results from the evaluation.
    !> Will only be reallocated if it is too small to hold all the results
    REAL(eis_num), DIMENSION(:), ALLOCATABLE :: result
    !> Error code returned by the evaluation
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Host code parameters provided. Optional, default no values (C_PTR_NULL)
    TYPE(C_PTR), INTENT(IN), OPTIONAL :: host_params
    !> Number of results returned by the evaluation
    INTEGER :: eip_evaluate_stack
    TYPE(C_PTR) :: params

    IF (PRESENT(host_params)) THEN
      params = host_params
    ELSE
      params = C_NULL_PTR
    END IF

    errcode = eis_err_none

    IF (stack%has_emplaced) THEN
      errcode = IOR(errcode, eis_err_has_emplaced)
      CALL this%err_handler%add_error(eis_err_evaluator, errcode)
      eip_evaluate_stack = 0.0_eis_num
      RETURN
    END IF

    IF (stack%has_deferred) THEN
      CALL this%undefer(stack, errcode, host_params = params)
      IF (errcode /= eis_err_none) RETURN
      stack%has_deferred = .FALSE.
    END IF

    eip_evaluate_stack = ees_evaluate(this%evaluator, stack, result, params, &
        errcode, this%err_handler)

    stack%sanity_checked = (errcode == eis_err_none)

  END FUNCTION eip_evaluate_stack


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to evaluate a string straight to a result
  !> @param[inout] this
  !> @param[in] str
  !> @param[inout] errcode
  !> @param[in] host_params
  !> @param[in] filename
  !> @param[in] line_number
  !> @param[in] char_offset
  !> @param[out] cap_bits
  !> @param[in] allow_text
  !> @return eip_evaluate_string
  FUNCTION eip_evaluate_string(this, str, result, errcode, host_params, &
      simplify, minify, filename, line_number, char_offset, &
      cap_bits, allow_text)
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
    TYPE(C_PTR), INTENT(IN), OPTIONAL :: host_params
    !> Logical determining if the expression should be simplified before
    !> evaluation. Not usually beneficial for single direct string to value
    !> evaluations. Optional, default same as set during init
    LOGICAL, INTENT(IN), OPTIONAL :: simplify
    !> Logical determining if the expression should be minified before
    !> evaluation. Not usually beneficial for single direct string to value
    !> evaluations. Optional, default same as set during init
    LOGICAL, INTENT(IN), OPTIONAL :: minify
    !> Filename containing the original text
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: filename
    !> Line number of the original text
    INTEGER, INTENT(IN), OPTIONAL :: line_number
    !> Character position offset in parse
    INTEGER, INTENT(IN), OPTIONAL :: char_offset
    !> Capability bits of the parsed stack
    INTEGER(eis_bitmask), INTENT(OUT), OPTIONAL :: cap_bits
    !> Should the returned stack be able to handle text elements
    LOGICAL, INTENT(IN), OPTIONAL :: allow_text
    !> Number of results returned by the evaluation
    INTEGER :: eip_evaluate_string
    TYPE(eis_stack) :: stack
    TYPE(C_PTR) :: params

    IF (PRESENT(host_params)) THEN
      params = host_params
    ELSE
      params = C_NULL_PTR
    END IF

    errcode = eis_err_none

    CALL this%tokenize(str, stack, errcode, simplify, minify, filename, &
        line_number, char_offset, allow_text = allow_text)
    IF (errcode /= eis_err_none) RETURN
    IF (stack%has_emplaced) CALL this%emplace(stack, errcode, &
        host_params = host_params)
    IF (errcode == eis_err_none) THEN
      eip_evaluate_string = this%evaluate(stack, result, errcode, &
          host_params = host_params)
      IF (PRESENT(cap_bits)) cap_bits = stack%cap_bits
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
  !> @param[in] host_params
  SUBROUTINE eip_undefer(this, stack, errcode, host_params)
    CLASS(eis_parser) :: this
    !> Stack to undefer
    CLASS(eis_stack), INTENT(INOUT) :: stack
    !> Error code from undefering stage
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Optional host code parameters that might be needed during
    TYPE(C_PTR), INTENT(IN), OPTIONAL :: host_params
    INTEGER :: ipt, stored_params, stored_charpos
    INTEGER(eis_bitmask) :: cap_bits
    CHARACTER(LEN=:), ALLOCATABLE :: str
    TYPE(C_PTR) :: params

    IF (PRESENT(host_params)) THEN
      params = host_params
    ELSE
      params = C_NULL_PTR
    END IF

    cap_bits = 0_eis_bitmask

    DO ipt = 1, stack%stack_point
      IF (stack%co_entries(ipt)%defer) THEN
        !This copy is a workaround, I do not believe it is required per standard
        ALLOCATE(str, SOURCE = stack%co_entries(ipt)%text)
        stored_params = stack%entries(ipt)%actual_params
        stored_charpos = stack%co_entries(ipt)%charindex
        CALL this%load_block(str, stack%entries(ipt), &
            stack%co_entries(ipt))
        stack%entries(ipt)%actual_params = stored_params
        stack%co_entries(ipt)%charindex = stored_charpos
        IF (stack%co_entries(ipt)%defer) THEN
          errcode = IOR(errcode, eis_err_has_deferred)
          CALL this%err_handler%add_error(eis_err_parser, errcode, &
              str, stack%co_entries(ipt)%charindex)
        END IF
        IF (stack%entries(ipt)%ptype == eis_pt_stored_variable) THEN
          CALL this%registry%copy_in_stored(stack%entries(ipt)%value, &
              stack, this%err_handler, ipt)
        END IF
        DEALLOCATE(str)
        stack%cap_bits = IOR(stack%cap_bits, stack%co_entries(ipt)%cap_bits)
      END IF
    END DO

    IF (this%should_minify) CALL this%minify(stack, errcode)
    IF (this%should_simplify) CALL this%simplify(stack, errcode, &
        host_params = params)

  END SUBROUTINE eip_undefer



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to simplify a stack. This ignores the default parser settings
  !> and always simplifies the stack
  !> @param[inout] this
  !> @param[inout] stack
  !> @param[inout] errcode
  !> @param[in] host_params
  SUBROUTINE eip_simplify(this, stack, errcode, host_params)
    CLASS(eis_parser) :: this
    !> Stack to simplify
    CLASS(eis_stack), INTENT(INOUT) :: stack
    !> Error code from the simplification step
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Optional host code parameters. Can be used by stack elements
    !> to determine if they can be simplified or not
    TYPE(C_PTR), INTENT(IN), OPTIONAL :: host_params
    TYPE(C_PTR) :: params

    IF (PRESENT(host_params)) THEN
      params = host_params
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
  !> @param[in] host_params
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
  !> @param[in] host_params
  !> @param[out] remaining_functions
  !> @param[inout] capbits
  !> @param[inout] errcode
  RECURSIVE SUBROUTINE eip_emplace_node(this, tree_node, host_params, &
      remaining_functions, capbits, errcode)
    CLASS(eis_parser) :: this
    TYPE(eis_tree_item), INTENT(INOUT) :: tree_node !< Node to operate on
    TYPE(C_PTR), INTENT(IN) :: host_params !< Host code specified parameters
    !> Logical determining if there are remaining unemplaced functions below
    !> this node
    LOGICAL, INTENT(INOUT) :: remaining_functions
    !> Capability bits for this node.
    INTEGER(eis_bitmask), INTENT(INOUT) :: capbits
    !> Error code for this node
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    TYPE(eis_tree_item), POINTER :: new_node
    INTEGER(eis_error) :: errcode_l
    INTEGER :: inode, nparams, sp
    TYPE(eis_stack), TARGET :: temp_stack
    TYPE(eis_stack), POINTER :: emplace_stack
    REAL(eis_num), DIMENSION(:), ALLOCATABLE :: results, params
    REAL(eis_num_c), DIMENSION(:), ALLOCATABLE :: params_interop
    PROCEDURE(parser_late_bind_fn), POINTER :: late_bind_fn
    PROCEDURE(parser_late_bind_interop_fn), POINTER :: late_bind_fn_c
    PROCEDURE(parser_late_bind_stack_fn), POINTER :: stack_late_bind_fn
    PROCEDURE(parser_late_bind_stack_interop_fn), POINTER :: &
        stack_late_bind_fn_c
    INTEGER(eis_status) :: status_code
    TYPE(eis_stack), DIMENSION(:), ALLOCATABLE :: stacks
    TYPE(eis_stack), POINTER :: inter_stack
    INTEGER(eis_status_c), DIMENSION(:), ALLOCATABLE :: per_stack_params
    INTEGER(C_INT), DIMENSION(:), ALLOCATABLE :: interop_param_stack_ids
    INTEGER :: rcount
    INTEGER(C_INT) :: interop_stack_id
    CHARACTER(LEN=1, KIND=C_CHAR), DIMENSION(:), ALLOCATABLE, TARGET :: &
        interop_name

    status_code = eis_status_none
    errcode_l = eis_err_none
    interop_stack_id = -1

    IF (ASSOCIATED(tree_node%nodes)) THEN
      DO inode = 1, SIZE(tree_node%nodes)
        CALL this%emplace_node(tree_node%nodes(inode), host_params, &
            remaining_functions, capbits, errcode)
      END DO
    END IF

    !This is not an stored function, no emplacement necessary
    IF (tree_node%value%ptype /= eis_pt_emplaced_function &
        .AND. tree_node%value%ptype /= eis_pt_emplaced_variable) RETURN

    CALL this%registry%get_stored_emplacement(tree_node%value%value, &
        late_bind_fn, late_bind_fn_c, stack_late_bind_fn, stack_late_bind_fn_c)

    !If you are trying to use a non-interoperable parser with an interoperable
    !function this is an error. Report and leave
    IF (this%interop_id < 1 .AND. (ASSOCIATED(late_bind_fn_c) &
        .OR. ASSOCIATED(stack_late_bind_fn_c))) THEN
      errcode = eis_err_interop
      IF (ALLOCATED(tree_node%co_value%text)) THEN
        CALL this%err_handler%add_error(eis_err_emplacer, errcode, &
            tree_node%co_value%text, tree_node%co_value%charindex)
      ELSE
        CALL this%err_handler%add_error(eis_err_emplacer, errcode)
      END IF
      RETURN
    END IF

    !If we have an interoperable function then we will want a C string version
    !of the name
    IF (ASSOCIATED(late_bind_fn_c) .OR. ASSOCIATED(stack_late_bind_fn_c)) THEN
      IF (ALLOCATED(tree_node%co_value%text)) THEN
        ALLOCATE(interop_name(LEN(tree_node%co_value%text)+1))
        CALL eis_f_c_string(tree_node%co_value%text, &
            LEN(tree_node%co_value%text)+1, interop_name)
      ELSE
        ALLOCATE(interop_name(1))
        CALL eis_f_c_string("", 1, interop_name)
      END IF
    END IF

    !Get stacks for the parameters
    nparams = 0
    IF (ASSOCIATED(tree_node%nodes)) THEN
      nparams = SIZE(tree_node%nodes)
      ALLOCATE(stacks(nparams))
      errcode_l = eis_err_none
      DO inode = 1, nparams
        CALL initialise_stack(stacks(inode))
        CALL eis_tree_to_stack(tree_node%nodes(nparams - inode + 1), &
            stacks(inode))
      END DO
    ELSE
      ALLOCATE(stacks(nparams))
    END IF

    !Fortran stack bind function
    IF (ASSOCIATED(stack_late_bind_fn)) THEN
      errcode_l = eis_err_none
      IF (ALLOCATED(tree_node%co_value%text)) THEN
        CALL stack_late_bind_fn(tree_node%co_value%text, nparams, stacks, &
            host_params, temp_stack, status_code, errcode_l)
      ELSE
        CALL stack_late_bind_fn("", nparams, stacks, &
            host_params, temp_stack, status_code, errcode_l)
      END IF
      emplace_stack => temp_stack
      IF (errcode_l /= eis_err_none) THEN
        errcode = IOR(errcode, errcode_l)
        IF (ALLOCATED(tree_node%co_value%text)) THEN
          CALL this%err_handler%add_error(eis_err_emplacer, errcode_l, &
              tree_node%co_value%text, tree_node%co_value%charindex)
        ELSE
          CALL this%err_handler%add_error(eis_err_emplacer, errcode_l)
        END IF
      END IF
    END IF

    !C stack binding function
    IF (ASSOCIATED(stack_late_bind_fn_c)) THEN
      ALLOCATE(per_stack_params(nparams))
      per_stack_params = eis_status_none
      ALLOCATE(interop_param_stack_ids(nparams))
      DO inode = 1, nparams
        ALLOCATE(inter_stack, SOURCE = stacks(inode))
        interop_param_stack_ids(inode) = &
            eis_add_interop_stack(inter_stack, this%interop_id, owns = .TRUE.)
      END DO
      errcode_l = eis_err_none
      CALL stack_late_bind_fn_c(C_LOC(interop_name), nparams, &
          interop_param_stack_ids, host_params, interop_stack_id, &
          per_stack_params, status_code, errcode_l)
      emplace_stack => eis_get_interop_stack(interop_stack_id)
      !Now go through and release the stacks unless the interop function said
      !to keep them. Go backwards so the interop list shrinks properly
      DO inode = nparams, 1, -1
        IF (IAND(per_stack_params(inode), eis_status_retain_stack) == 0) THEN
          CALL eis_release_interop_stack(interop_param_stack_ids(inode))
        END IF
      END DO
      DEALLOCATE(per_stack_params)
      DEALLOCATE(interop_param_stack_ids)
      IF (errcode_l /= eis_err_none) THEN
        errcode = IOR(errcode, errcode_l)
        IF (ALLOCATED(tree_node%co_value%text)) THEN
          CALL this%err_handler%add_error(eis_err_emplacer, errcode_l, &
              tree_node%co_value%text, tree_node%co_value%charindex)
        ELSE
          CALL this%err_handler%add_error(eis_err_emplacer, errcode_l)
        END IF
      END IF
    END IF

    !Now deal with emplaced functions that want numerical rather than stack
    !parameters

    !First generate the parameters
    IF (ASSOCIATED(tree_node%nodes)) THEN
      ALLOCATE(params(nparams))
      errcode_l = eis_err_none
      DO inode = 1, nparams
        rcount = this%evaluate(stacks(inode), results, errcode_l, &
            host_params = host_params)
        params(inode) = results(1)
        errcode = IOR(errcode, errcode_l)
      END DO
      IF (errcode_l /= eis_err_none) THEN
        errcode = IOR(errcode, errcode_l)
        IF (ALLOCATED(tree_node%co_value%text)) THEN
          CALL this%err_handler%add_error(eis_err_emplacer, errcode_l, &
              tree_node%co_value%text, tree_node%co_value%charindex)
        ELSE
          CALL this%err_handler%add_error(eis_err_emplacer, errcode_l)
        END IF
      END IF
    ELSE
      ALLOCATE(params(nparams))
    END IF

    !Fortran bind function
    IF (ASSOCIATED(late_bind_fn)) THEN
      CALL initialise_stack(temp_stack)
      emplace_stack => temp_stack
      IF (ALLOCATED(tree_node%co_value%text)) THEN
        CALL late_bind_fn(tree_node%co_value%text, nparams, params, &
            host_params, temp_stack, status_code, errcode_l)
      ELSE
        CALL late_bind_fn("", nparams, params, &
            host_params, temp_stack, status_code, errcode_l)
      END IF
      errcode = IOR(errcode, errcode_l)
      IF (ALLOCATED(tree_node%co_value%text)) THEN
        CALL this%err_handler%add_error(eis_err_emplacer, errcode_l, &
            tree_node%co_value%text, tree_node%co_value%charindex)
      ELSE
        CALL this%err_handler%add_error(eis_err_emplacer, errcode_l)
      END IF
    END IF

    !Interop bind function
    IF (ASSOCIATED(late_bind_fn_c)) THEN
      ALLOCATE(params_interop(nparams))
      params_interop = REAL(params, eis_num_c)
      errcode_l = eis_err_none
      CALL late_bind_fn_c(C_LOC(interop_name), nparams, params_interop, &
          host_params, interop_stack_id, status_code, errcode_l)
      errcode = IOR(errcode, errcode_l)
      emplace_stack => eis_get_interop_stack(interop_stack_id) 
      DEALLOCATE(params_interop)
      IF (errcode /= eis_err_none) THEN
        IF (ALLOCATED(tree_node%co_value%text)) THEN
          CALL this%err_handler%add_error(eis_err_emplacer, errcode_l, &
              tree_node%co_value%text, tree_node%co_value%charindex)
        ELSE
          CALL this%err_handler%add_error(eis_err_emplacer, errcode_l)
        END IF
      END IF
    END IF
    DEALLOCATE(params)
    DO inode = 1, SIZE(stacks)
      CALL deallocate_stack(stacks(inode))
    END DO
    DEALLOCATE(stacks)

    remaining_functions = .TRUE.
    IF (.NOT. ASSOCIATED(emplace_stack)) RETURN
    IF (.NOT. emplace_stack%init) RETURN
    IF (emplace_stack%stack_point == 0) RETURN

    IF (emplace_stack%has_emplaced) THEN
      CALL this%emplace(emplace_stack, errcode_l, host_params = host_params)
      errcode = IOR(errcode, errcode_l)
    END IF

    capbits = IOR(capbits, emplace_stack%cap_bits)
    !If emplacement is not forbidden by status then build new node
    IF (IAND(status_code, eis_status_no_emplace) == 0) THEN
      sp = emplace_stack%stack_point + 1
      ALLOCATE(new_node)
      CALL eis_build_node(emplace_stack, sp, new_node)
      DEALLOCATE(tree_node%nodes)
      tree_node = new_node
      new_node%nodes => NULL()
      DEALLOCATE(new_node)
      remaining_functions = .FALSE.
    ELSE
      remaining_functions = .TRUE.
    END IF

    IF (interop_stack_id > 0 &
        .AND. IAND(status_code, eis_status_retain_stack) == 0) THEN
      CALL eis_release_interop_stack(interop_stack_id)
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
  !> This allows multiple eplacements with different "host_params" to produce
  !> different stacks
  !> @param[inout] this
  !> @param[inout] stack
  !> @param[inout] errcode
  !> @param[in] host_params
  !> @param[out] destination
  SUBROUTINE eip_emplace(this, stack, errcode, host_params, destination)
    CLASS(eis_parser) :: this
    CLASS(eis_stack), INTENT(INOUT), TARGET :: stack !< Stack to emplace
    !> Error code from emplacement
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Host code specified parameters. Optional, default C_PTR_NULL
    TYPE(C_PTR), INTENT(IN), OPTIONAL :: host_params
    !> Destination for final emplaced stack. Optional, default is build in 
    !> "stack"
    CLASS(eis_stack), INTENT(INOUT), OPTIONAL, TARGET :: destination
    TYPE(eis_tree_item), POINTER :: root
    INTEGER :: sp
    INTEGER(eis_bitmask) :: capbits
    LOGICAL :: remaining_functions
    CLASS(eis_stack), POINTER :: sptr
    TYPE(C_PTR) :: params

    IF (PRESENT(host_params)) THEN
      params = host_params
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
    CALL this%emplace_node(root, params, remaining_functions, capbits, errcode)
    CALL deallocate_stack(sptr)
    CALL initialise_stack(sptr)
    CALL eis_tree_to_stack(root, sptr)
    sptr%cap_bits = capbits
    DEALLOCATE(root)
    sptr%has_emplaced = remaining_functions

    IF (this%should_minify) CALL this%minify(sptr, errcode)
    IF (this%should_simplify) CALL this%simplify(sptr, errcode, &
        host_params = params)

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
  !> @param[in] description
  !> @param[in] hidden
  !> @param[in] text_params
  SUBROUTINE eip_add_function_defer(this, name, errcode,  cap_bits, &
      expected_params, can_simplify, global, description, hidden, &
      text_params)

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
    !> Description of this symbol
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Is this symbol hidden in document generation
    LOGICAL, INTENT(IN), OPTIONAL :: hidden
    !> Can this functor have text parameters
    LOGICAL, INTENT(IN), OPTIONAL :: text_params

    CALL this%add_function(name, eis_dummy, errcode, cap_bits, &
        expected_params, can_simplify, .TRUE., global, description, hidden, &
        text_params)

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
  !> @param[in] description
  !> @param[in] hidden
  !> @param[in] text_params
  SUBROUTINE eip_add_function_now(this, name, fn, errcode,  cap_bits, &
      expected_params, can_simplify, defer, global, description, hidden, &
      text_params)

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
    !> Description of this symbol
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Is this symbol hidden in document generation
    LOGICAL, INTENT(IN), OPTIONAL :: hidden
    !> Can this functor have text parameters
    LOGICAL, INTENT(IN), OPTIONAL :: text_params
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
          can_simplify, cap_bits, err_handler = this%err_handler, &
          defer = defer, description = description, hidden = hidden, &
          can_have_text_params = text_params)
    ELSE
      CALL this%registry%add_function(name, fn, params, errcode, can_simplify, &
          cap_bits, err_handler = this%err_handler, defer = defer, &
          description = description, hidden = hidden, &
          can_have_text_params = text_params)
    END IF

  END SUBROUTINE eip_add_function_now



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add a functor to the parser
  !> @details
  !> Functors are like functions but carry persistent information with them.
  !> When you call this function the functor that you supply is copied. If your
  !> functor is large enough that you don't want to make copies then use
  !> add_functor_pointer
  !> @param[inout] this
  !> @param[in] name
  !> @param[in] functor
  !> @param[inout] errcode
  !> @param[in] cap_bits
  !> @param[in] expected_params
  !> @param[in] can_simplify
  !> @param[in] defer
  !> @param[in] global
  !> @param[in] description
  !> @param[in] hidden
  !> @param[in] text_params
  SUBROUTINE eip_add_functor(this, name, functor, errcode,  cap_bits, &
      expected_params, can_simplify, defer, global, &
      description, hidden, text_params)

    CLASS(eis_parser) :: this
    !> Name to register function with. Will be used in expressions to
    !> call the function
    CHARACTER(LEN=*), INTENT(IN) :: name
    !> Functor to use when the block is accessed
    CLASS(eis_functor) :: functor
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
    !> same as calling eip_add_functor_defer
    LOGICAL, INTENT(IN), OPTIONAL :: defer
    !> Whether to add this function to the global list of functions for all 
    !> parsers or just for this parser. Optional, default this parser only
    LOGICAL, INTENT(IN), OPTIONAL :: global
    !> Description of this symbol
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Is this symbol hidden in document generation
    LOGICAL, INTENT(IN), OPTIONAL :: hidden
    !> Can this functor have text parameters
    LOGICAL, INTENT(IN), OPTIONAL :: text_params
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
      CALL global_registry%add_functor(name, functor, params, errcode, &
          can_simplify, cap_bits, err_handler = this%err_handler, &
          defer = defer, description = description, hidden = hidden, &
          can_have_text_params = text_params)
    ELSE
      CALL this%registry%add_functor(name, functor, params, errcode, &
          can_simplify, cap_bits, err_handler = this%err_handler, &
          defer = defer, description = description, hidden = hidden, &   
          can_have_text_params = text_params)
    END IF

  END SUBROUTINE eip_add_functor



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add a functor to the parser using a pointer to the functor.
  !> @details
  !> Unlike add_functor no copy is made of the functor passed to this function
  !> the pointer is used as is. By default EIS assumes ownership of the pointer
  !> and it is automatically deleted when the parser wants to discard it. If the
  !> host code is maintaining the pointer itself, set the "owns" parameter to
  !> .FALSE. then the parser will discard the pointer without deleting it.
  !> If you want the same functor under different names without making copies
  !> then at most only one of then should own the functor. Care must be taken
  !> because reusing the name that is associated with the ownership of the
  !> functor will invalidate all of the other names. Unassociated pointers
  !> cause this routine to return immediately
  !> @param[inout] this
  !> @param[in] name
  !> @param[in] functor
  !> @param[inout] errcode
  !> @param[in] cap_bits
  !> @param[in] expected_params
  !> @param[in] can_simplify
  !> @param[in] defer
  !> @param[in] global
  !> @param[in] description
  !> @param[in] hidden
  !> @param[in] owns
  !> @param[in] text_params
  SUBROUTINE eip_add_functor_ptr(this, name, functor, errcode,  cap_bits, &
      expected_params, can_simplify, defer, global, &
      description, hidden, owns, text_params)

    CLASS(eis_parser) :: this
    !> Name to register function with. Will be used in expressions to
    !> call the function
    CHARACTER(LEN=*), INTENT(IN) :: name
    !> Functor to use when the block is accessed
    CLASS(eis_functor), POINTER :: functor
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
    !> same as calling eip_add_functor_defer
    LOGICAL, INTENT(IN), OPTIONAL :: defer
    !> Whether to add this function to the global list of functions for all 
    !> parsers or just for this parser. Optional, default this parser only
    LOGICAL, INTENT(IN), OPTIONAL :: global
    !> Description of this symbol
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Is this symbol hidden in document generation
    LOGICAL, INTENT(IN), OPTIONAL :: hidden
    !> Should the parser take ownership of this pointer? Optional, default TRUE
    LOGICAL, INTENT(IN), OPTIONAL :: owns
    !> Can this functor have text parameters
    LOGICAL, INTENT(IN), OPTIONAL :: text_params
    INTEGER :: params
    LOGICAL :: is_global

    IF (.NOT. ASSOCIATED(functor)) RETURN

    IF (PRESENT(expected_params)) THEN
      params = expected_params
    ELSE
      params = -1
    END IF

    is_global = .FALSE.
    IF (PRESENT(global)) is_global = global

    IF (is_global) THEN
      CALL global_registry%add_functor_pointer(name, functor, params, errcode, &
          can_simplify, cap_bits, err_handler = this%err_handler, &
          defer = defer, description = description, hidden = hidden, &
          owns = owns, can_have_text_params = text_params)
    ELSE
      CALL this%registry%add_functor_pointer(name, functor, params, errcode, &
          can_simplify, cap_bits, err_handler = this%err_handler, &
          defer = defer, description = description, hidden = hidden, &
          owns = owns, can_have_text_params = text_params)
    END IF

  END SUBROUTINE eip_add_functor_ptr


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
  !> @param[in] description
  !> @param[in] hidden
  SUBROUTINE eip_add_variable_defer(this, name, errcode, cap_bits, &
      can_simplify, global, description, hidden)

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
    !> Description of this symbol
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Is this symbol hidden in document generation
    LOGICAL, INTENT(IN), OPTIONAL :: hidden

    CALL this%add_variable(name, eis_dummy, errcode, cap_bits, can_simplify, &
        .TRUE., global, description, hidden)

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
  !> @param[in] description
  !> @param[in] hidden
  SUBROUTINE eip_add_variable_now(this, name, fn, errcode, cap_bits, &
      can_simplify, defer, global, description, hidden)

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
    !> Description of this symbol
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Is this symbol hidden in document generation
    LOGICAL, INTENT(IN), OPTIONAL :: hidden
    LOGICAL :: is_global, simplify

    is_global = .FALSE.
    IF (PRESENT(global)) is_global = global
    simplify = .FALSE.
    IF (PRESENT(can_simplify)) simplify = can_simplify

    IF (is_global) THEN
      CALL global_registry%add_variable(name, fn, errcode, simplify, &
          cap_bits, err_handler = this%err_handler, defer = defer, &
          description = description, hidden = hidden)
    ELSE
      CALL this%registry%add_variable(name, fn, errcode, simplify, &
          cap_bits, err_handler = this%err_handler, defer = defer, &
          description = description, hidden = hidden)
    END IF

  END SUBROUTINE eip_add_variable_now



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add a variable to the parser using a pointer
  !> @param[inout] this
  !> @param[in] ptr
  !> @param[in] fn
  !> @param[inout] errcode
  !> @param[in] cap_bits
  !> @param[in] can_simplify
  !> @param[in] defer
  !> @param[in] global
  !> @param[in] description
  !> @param[in] hidden
  SUBROUTINE eip_add_variable_i4(this, name, ptr, errcode, cap_bits, &
      can_simplify, defer, global, description, hidden)

    CLASS(eis_parser) :: this
    !> Name to register variable with. Will be used in expressions to
    !> call the function
    CHARACTER(LEN=*), INTENT(IN) :: name
    !> Variable to be associated with the variable
#ifdef F2008
    INTEGER(INT32), TARGET :: ptr
#else
    INTEGER(INT32), POINTER :: ptr
#endif
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
    !> Description of this symbol
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Is this symbol hidden in document generation
    LOGICAL, INTENT(IN), OPTIONAL :: hidden
    LOGICAL :: is_global, simplify

    is_global = .FALSE.
    IF (PRESENT(global)) is_global = global
    simplify = .FALSE.
    IF (PRESENT(can_simplify)) simplify = can_simplify

    IF (is_global) THEN
      CALL global_registry%add_variable(name, eis_dummy, errcode, &
          simplify, cap_bits, err_handler = this%err_handler, &
          defer = defer, i32data = ptr, description = description)
    ELSE
      CALL this%registry%add_variable(name, eis_dummy, errcode, simplify, &
          cap_bits, err_handler = this%err_handler, defer = defer, &
          i32data = ptr, description = description)
    END IF

  END SUBROUTINE eip_add_variable_i4



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add a variable to the parser using a pointer
  !> @param[inout] this
  !> @param[in] ptr
  !> @param[in] fn
  !> @param[inout] errcode
  !> @param[in] cap_bits
  !> @param[in] can_simplify
  !> @param[in] defer
  !> @param[in] global
  !> @param[in] description
  !> @param[in] hidden
  SUBROUTINE eip_add_variable_i8(this, name, ptr, errcode, cap_bits, &
      can_simplify, defer, global, description, hidden)

    CLASS(eis_parser) :: this
    !> Name to register variable with. Will be used in expressions to
    !> call the function
    CHARACTER(LEN=*), INTENT(IN) :: name
    !> Variable to be associated with the variable
#ifdef F2008
    INTEGER(INT64), TARGET :: ptr
#else
    INTEGER(INT64), POINTER :: ptr
#endif
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
    !> Description of this symbol
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Is this symbol hidden in document generation
    LOGICAL, INTENT(IN), OPTIONAL :: hidden
    LOGICAL :: is_global, simplify

    is_global = .FALSE.
    IF (PRESENT(global)) is_global = global
    simplify = .FALSE.
    IF (PRESENT(can_simplify)) simplify = can_simplify

    IF (is_global) THEN
      CALL global_registry%add_variable(name, eis_dummy, errcode, &
          simplify, cap_bits, err_handler = this%err_handler, &
          defer = defer, i64data = ptr, description = description, &
          hidden = hidden)
    ELSE
      CALL this%registry%add_variable(name, eis_dummy, errcode, simplify, &
          cap_bits, err_handler = this%err_handler, defer = defer, &
          i64data = ptr, description = description, hidden = hidden)
    END IF

  END SUBROUTINE eip_add_variable_i8



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add a variable to the parser using a pointer
  !> @param[inout] this
  !> @param[in] ptr
  !> @param[in] fn
  !> @param[inout] errcode
  !> @param[in] cap_bits
  !> @param[in] can_simplify
  !> @param[in] defer
  !> @param[in] global
  !> @param[in] description
  !> @param[in] hidden
  SUBROUTINE eip_add_variable_r4(this, name, ptr, errcode, cap_bits, &
      can_simplify, defer, global, description, hidden)

    CLASS(eis_parser) :: this
    !> Name to register variable with. Will be used in expressions to
    !> call the function
    CHARACTER(LEN=*), INTENT(IN) :: name
    !> Variable to be associated with the variable
#ifdef F2008
    REAL(REAL32), TARGET :: ptr
#else
    REAL(REAL32), POINTER :: ptr
#endif
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
    !> Description of this symbol
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Is this symbol hidden in document generation
    LOGICAL, INTENT(IN), OPTIONAL :: hidden
    LOGICAL :: is_global, simplify

    is_global = .FALSE.
    IF (PRESENT(global)) is_global = global
    simplify = .FALSE.
    IF (PRESENT(can_simplify)) simplify = can_simplify

    IF (is_global) THEN
      CALL global_registry%add_variable(name, eis_dummy, errcode, &
          simplify, cap_bits, err_handler = this%err_handler, &
          defer = defer, r32data = ptr, description = description, &
          hidden = hidden)
    ELSE
      CALL this%registry%add_variable(name, eis_dummy, errcode, simplify, &
          cap_bits, err_handler = this%err_handler, defer = defer, &
          r32data = ptr, description = description, hidden = hidden)
    END IF

  END SUBROUTINE eip_add_variable_r4



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add a variable to the parser using a pointer
  !> @param[inout] this
  !> @param[in] ptr
  !> @param[in] fn
  !> @param[inout] errcode
  !> @param[in] cap_bits
  !> @param[in] can_simplify
  !> @param[in] defer
  !> @param[in] global
  !> @param[in] description
  !> @param[in] hidden
  SUBROUTINE eip_add_variable_r8(this, name, ptr, errcode, cap_bits, &
      can_simplify, defer, global, description, hidden)

    CLASS(eis_parser) :: this
    !> Name to register variable with. Will be used in expressions to
    !> call the function
    CHARACTER(LEN=*), INTENT(IN) :: name      
    !> Variable to be associated with the variable
#ifdef F2008
    REAL(REAL64), TARGET :: ptr
#else
    REAL(REAL64), POINTER :: ptr
#endif
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
    !> Description of this symbol
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Is this symbol hidden in document generation
    LOGICAL, INTENT(IN), OPTIONAL :: hidden
    LOGICAL :: is_global, simplify

    is_global = .FALSE.
    IF (PRESENT(global)) is_global = global
    simplify = .FALSE.
    IF (PRESENT(can_simplify)) simplify = can_simplify

    IF (is_global) THEN
      CALL global_registry%add_variable(name, eis_dummy, errcode, &
          simplify, cap_bits, err_handler = this%err_handler, &
          defer = defer, r64data = ptr, description = description, &
          hidden = hidden)
    ELSE
      CALL this%registry%add_variable(name, eis_dummy, errcode, simplify, &
          cap_bits, err_handler = this%err_handler, defer = defer, &
          r64data = ptr, description = description, hidden = hidden)
    END IF

  END SUBROUTINE eip_add_variable_r8



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add a variable to the parser using a C pointer
  !> @param[inout] this
  !> @param[in] ptr
  !> @param[in] isinteger
  !> @param[in] is64bit
  !> @param[inout] errcode
  !> @param[in] cap_bits
  !> @param[in] can_simplify
  !> @param[in] defer
  !> @param[in] global
  !> @param[in] description
  !> @param[in] hidden
  SUBROUTINE eip_add_variable_c(this, name, ptr, isinteger, is64bit, errcode, &
      cap_bits, can_simplify, defer, global, description, hidden)

    CLASS(eis_parser) :: this
    !> Name to register variable with. Will be used in expressions to
    !> call the function
    CHARACTER(LEN=*), INTENT(IN) :: name
    !> Variable to be associated with the variable
    TYPE(C_PTR) :: ptr
    !> Is this an integer
    LOGICAL, INTENT(IN) :: isinteger
    !> Is this variable 64 bit
    LOGICAL, INTENT(IN) :: is64bit
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
    !> Description of this symbol
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Is this symbol hidden in document generation
    LOGICAL, INTENT(IN), OPTIONAL :: hidden
    LOGICAL :: is_global, simplify
    INTEGER(INT32), POINTER :: i32p
    INTEGER(INT64), POINTER :: i64p
    REAL(REAL32), POINTER :: r32p
    REAL(REAL64), POINTER :: r64p

    is_global = .FALSE.
    IF (PRESENT(global)) is_global = global
    simplify = .FALSE.
    IF (PRESENT(can_simplify)) simplify = can_simplify

    IF (is_global) THEN
      IF (isinteger) THEN
        IF (is64bit) THEN
          CALL C_F_POINTER(ptr, i64p)
          CALL global_registry%add_variable(name, eis_dummy, errcode, &
              simplify, cap_bits, err_handler = this%err_handler, &
              defer = defer, i64data = i64p, description = description, &
              hidden = hidden)
        ELSE
          CALL C_F_POINTER(ptr, i32p)
          CALL global_registry%add_variable(name, eis_dummy, errcode, &
              simplify, cap_bits, err_handler = this%err_handler, &
              defer = defer, i32data = i32p, description = description, &
              hidden = hidden)
        END IF
      ELSE
        IF (is64bit) THEN
          CALL C_F_POINTER(ptr, r64p)
          CALL global_registry%add_variable(name, eis_dummy, errcode, &
              simplify, cap_bits, err_handler = this%err_handler, &
              defer = defer, r64data = r64p, description = description, &
              hidden = hidden)
        ELSE
          CALL C_F_POINTER(ptr, r32p)
          CALL global_registry%add_variable(name, eis_dummy, errcode, &
              simplify, cap_bits, err_handler = this%err_handler, &
              defer = defer, r32data = r32p, description = description, &
              hidden = hidden)
        END IF
      END IF
    ELSE
      IF (isinteger) THEN
        IF (is64bit) THEN
          CALL C_F_POINTER(ptr, i64p)
          CALL this%registry%add_variable(name, eis_dummy, errcode, &
              simplify, cap_bits, err_handler = this%err_handler, &
              defer = defer, i64data = i64p, description = description, &
              hidden = hidden)
        ELSE
          CALL C_F_POINTER(ptr, i32p)
          CALL this%registry%add_variable(name, eis_dummy, errcode, &
              simplify, cap_bits, err_handler = this%err_handler, &
              defer = defer, i32data = i32p, description = description, &
              hidden = hidden)
        END IF
      ELSE
        IF (is64bit) THEN
          CALL C_F_POINTER(ptr, r64p)
          CALL this%registry%add_variable(name, eis_dummy, errcode, &
              simplify, cap_bits, err_handler = this%err_handler, &
              defer = defer, r64data = r64p, description = description, &
              hidden = hidden)
        ELSE
          CALL C_F_POINTER(ptr, r32p)
          CALL this%registry%add_variable(name, eis_dummy, errcode, &
              simplify, cap_bits, err_handler = this%err_handler, &
              defer = defer, r32data = r32p, description = description, &
              hidden = hidden)
        END IF
      END IF
    END IF

  END SUBROUTINE eip_add_variable_c



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
  !> @param[in] description
  !> @param[in] hidden
  SUBROUTINE eip_add_constant_defer(this, name, errcode, &
      can_simplify, global, cap_bits, description, hidden)

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
    !> Description to go with constant
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Is this symbol hidden in document generation
    LOGICAL, INTENT(IN), OPTIONAL :: hidden

    CALL this%add_constant(name, 0.0_eis_num, errcode, cap_bits, can_simplify, &
        .TRUE., global, description, hidden)

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
  !> @param[in] description
  !> @param[in] hidden
  SUBROUTINE eip_add_constant_now(this, name, value, errcode, cap_bits, &
      can_simplify, defer, global, description, hidden)

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
    !> Description to go with constant
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Is this symbol hidden in document generation
    LOGICAL, INTENT(IN), OPTIONAL :: hidden
    LOGICAL :: is_global

    is_global = .FALSE.
    IF (PRESENT(global)) is_global = global

    IF (is_global) THEN
      CALL global_registry%add_constant(name, value, errcode, can_simplify, &
          cap_bits, err_handler = this%err_handler, defer = defer, &
          description = description, hidden = hidden)
    ELSE
      CALL this%registry%add_constant(name, value, errcode, can_simplify, &
          cap_bits, err_handler = this%err_handler, defer = defer, &
          description = description, hidden = hidden)
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
  !> @param[in] description
  !> @param[in] hidden
  SUBROUTINE eip_add_constant_i4(this, name, value, errcode, cap_bits, &
      can_simplify, defer, global, description, hidden)

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
    !> Description to go with constant
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Is this symbol hidden in document generation
    LOGICAL, INTENT(IN), OPTIONAL :: hidden

    CALL this%add_constant(name, REAL(value, eis_num), errcode, cap_bits, &
        can_simplify, defer, global, description, hidden)

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
  !> @param[in] description
  !> @param[in] hidden
  SUBROUTINE eip_add_constant_i8(this, name, value, errcode, cap_bits, &
      can_simplify, defer, global, description, hidden)

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
    !> Description to go with constant
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Is this symbol hidden in document generation
    LOGICAL, INTENT(IN), OPTIONAL :: hidden

    CALL this%add_constant(name, REAL(value, eis_num), errcode, cap_bits, &
        can_simplify, defer, global, description, hidden)

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
  !> @param[in] description
  !> @param[in] hidden
  SUBROUTINE eip_add_stack_variable_defer(this, name, errcode, global, &
      description, hidden)

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
    !> Description to go with variable
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Is this symbol hidden in document generation
    LOGICAL, INTENT(IN), OPTIONAL :: hidden

    CALL this%add_stack_variable(name, stack, errcode, .TRUE., global, &
        description = description, hidden = hidden)

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
  !> @param[in] description
  !> @param[in] hidden
  SUBROUTINE eip_add_stack_variable_stack(this, name, stack, errcode, defer, &
      global, description, hidden)

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
    !> Description to go with variable
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Is this symbol hidden in document generation
    LOGICAL, INTENT(IN), OPTIONAL :: hidden
    LOGICAL :: is_global
        
    is_global = .FALSE.
    IF (PRESENT(global)) is_global = global

    IF (is_global) THEN
      CALL global_registry%add_stack_variable(name, stack, errcode, &
          err_handler = this%err_handler, defer = defer, &
          description = description, hidden = hidden)
    ELSE
      CALL this%registry%add_stack_variable(name, stack, errcode, &
          err_handler = this%err_handler, defer = defer, &
          description = description, hidden = hidden)
    END IF

  END SUBROUTINE eip_add_stack_variable_stack


  !> @brief
  !> Add a stack variable from a string expression to the parser.
  !> @param[inout] this
  !> @param[in] name
  !> @param[in] string
  !> @param[inout] errcode
  !> @param[in] global
  !> @param[in] description
  !> @param[in] hidden
  SUBROUTINE eip_add_stack_variable_string(this, name, string, errcode, &
      global, description, hidden)

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
    !> Description to go with constant
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Is this symbol hidden in document generation
    LOGICAL, INTENT(IN), OPTIONAL :: hidden
    LOGICAL :: is_global
    TYPE(eis_stack) :: stack

    is_global = .FALSE.
    IF (PRESENT(global)) is_global = global

    CALL initialise_stack(stack)
    CALL this%tokenize(string, stack, errcode, simplify = .FALSE.)
    IF (is_global) THEN
      CALL global_registry%add_stack_variable(name, stack, errcode, &
          err_handler = this%err_handler, description = description, &
          hidden = hidden)
    ELSE
      CALL this%registry%add_stack_variable(name, stack, errcode, &
          err_handler = this%err_handler, description = description, &
          hidden = hidden)
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
  !> @param[in] description
  !> @param[in] hidden
  SUBROUTINE eip_add_emplaced_function(this, name, def_fn, errcode, &
      expected_params, description, hidden)

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
    !> Description to go with constant
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Is this symbol hidden in document generation
    LOGICAL, INTENT(IN), OPTIONAL :: hidden

    CALL this%registry%add_emplaced_function(name, errcode, &
        err_handler = this%err_handler, &
        expected_parameters = expected_params, description = description, &
        hidden = hidden, lb_fn = def_fn)

  END SUBROUTINE eip_add_emplaced_function




  !> @brief
  !> Add an interoperable emplaced function to the parser. Emplaced functions
  !> cannot be added globally or deferred
  !> @param[inout] this
  !> @param[in] name
  !> @param[in] def_fn
  !> @param[inout] errcode
  !> @param[in] expected_params
  !> @param[in] description
  !> @param[in] hidden
  SUBROUTINE eip_add_emplaced_interop_function(this, name, def_fn, errcode, &
      expected_params, description, hidden)

    CLASS(eis_parser) :: this
    !> Name to associated with the emplaced function
    CHARACTER(LEN=*), INTENT(IN) :: name
    !> Function to be called when emplacing the function
    PROCEDURE(parser_late_bind_interop_fn) :: def_fn
    !> Error code associated with the storing of the emplaced function
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Number of parameters expected for the emplaced function. Optional, 
    !> default = -1 (variadic function)
    INTEGER, INTENT(IN), OPTIONAL :: expected_params
    !> Description to go with constant
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Is this symbol hidden in document generation
    LOGICAL, INTENT(IN), OPTIONAL :: hidden

    CALL this%registry%add_emplaced_function(name, errcode, &
        err_handler = this%err_handler, &
        expected_parameters = expected_params, description = description, &
        hidden = hidden, clb_fn = def_fn)

  END SUBROUTINE eip_add_emplaced_interop_function



  !> @brief
  !> Add a stack emplaced function to the parser. Emplaced functions
  !> cannot be added globally or deferred
  !> @param[inout] this
  !> @param[in] name
  !> @param[in] def_fn
  !> @param[inout] errcode
  !> @param[in] expected_params
  !> @param[in] description
  !> @param[in] hidden
  SUBROUTINE eip_add_emplaced_stack_function(this, name, def_fn, errcode, &
      expected_params, description, hidden)

    CLASS(eis_parser) :: this
    !> Name to associated with the emplaced function
    CHARACTER(LEN=*), INTENT(IN) :: name
    !> Stack function to be called when emplacing the function
    PROCEDURE(parser_late_bind_stack_fn) :: def_fn
    !> Error code associated with the storing of the emplaced function
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Number of parameters expected for the emplaced function. Optional, 
    !> default = -1 (variadic function)
    INTEGER, INTENT(IN), OPTIONAL :: expected_params
    !> Description to go with constant
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Is this symbol hidden in document generation
    LOGICAL, INTENT(IN), OPTIONAL :: hidden

    CALL this%registry%add_emplaced_function(name, errcode, &
        err_handler = this%err_handler, &
        expected_parameters = expected_params, description = description, &
        hidden = hidden, lbs_fn = def_fn)

  END SUBROUTINE eip_add_emplaced_stack_function



  !> @brief
  !> Add a stack emplaced function to the parser. Emplaced functions
  !> cannot be added globally or deferred
  !> @param[inout] this
  !> @param[in] name
  !> @param[in] def_fn
  !> @param[inout] errcode
  !> @param[in] expected_params
  !> @param[in] description
  !> @param[in] hidden
  SUBROUTINE eip_add_emplaced_interop_stack_function(this, name, def_fn, &
      errcode, expected_params, description, hidden)

    CLASS(eis_parser) :: this
    !> Name to associated with the emplaced function
    CHARACTER(LEN=*), INTENT(IN) :: name
    !> Stack function to be called when emplacing the function
    PROCEDURE(parser_late_bind_stack_interop_fn) :: def_fn
    !> Error code associated with the storing of the emplaced function
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Number of parameters expected for the emplaced function. Optional, 
    !> default = -1 (variadic function)
    INTEGER, INTENT(IN), OPTIONAL :: expected_params
    !> Description to go with constant
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Is this symbol hidden in document generation
    LOGICAL, INTENT(IN), OPTIONAL :: hidden

    CALL this%registry%add_emplaced_function(name, errcode, &
        err_handler = this%err_handler, &
        expected_parameters = expected_params, description = description, &
        hidden = hidden, clbs_fn = def_fn)

  END SUBROUTINE eip_add_emplaced_interop_stack_function



  !> @brief
  !> Add an emplaced variable to the parser. Emplaced variables
  !> cannot be added globally or deferred
  !> @param[inout] this
  !> @param[in] name
  !> @param[in] def_fn
  !> @param[inout] errcode
  !> @param[in] expected_params
  !> @param[in] description
  !> @param[in] hidden
  SUBROUTINE eip_add_emplaced_variable(this, name, def_fn, errcode, &
      description, hidden)

    CLASS(eis_parser) :: this
    !> Name to associated with the emplaced variable
    CHARACTER(LEN=*), INTENT(IN) :: name
    !> Function to be called when emplacing the variable
    PROCEDURE(parser_late_bind_fn) :: def_fn
    !> Error code associated with the storing of the emplaced variable
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Description to go with variable
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Is this symbol hidden in document generation
    LOGICAL, INTENT(IN), OPTIONAL :: hidden

    CALL this%registry%add_emplaced_function(name, errcode, &
        err_handler = this%err_handler, expected_parameters = 0, &
        description = description, hidden = hidden, lb_fn = def_fn)

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
  !> @param[in] trim_charindex
  !> @param[inout] output
  !> @param[inout] err
  !> @param[in] filename
  !> @param[in] line_number
  !> @param[in] full_line
  !> @param[in] allow_text
  SUBROUTINE eip_tokenize_subexpression_infix(this, current, iblock, icoblock, &
      cap_bits, charindex, trim_charindex, output, err, filename, line_number, &
      full_line, allow_text)

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
    !> Character index for current in the trimmed string
    !> used for error reporting
    INTEGER, INTENT(IN) :: trim_charindex
    !> Output stack
    CLASS(eis_stack), INTENT(INOUT) :: output
    !> Error code from parsing this element
    INTEGER(eis_error), INTENT(INOUT) :: err
    !> Filename containing error. Optional, default do not use
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: filename
    !> Line number in file containing error. Optional, default
    !> do not use
    INTEGER, INTENT(IN), OPTIONAL :: line_number
    !> Full version of the line being parsed
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: full_line
    !> Should the parser allow text blocks 
    LOGICAL, INTENT(IN) :: allow_text
    TYPE(eis_stack_element) :: block2
    TYPE(eis_stack_co_element) :: coblock2
    INTEGER :: istr
    LOGICAL :: stack_empty, str_handled
    CHARACTER(LEN=:), ALLOCATABLE :: str

    cap_bits = 0_eis_bitmask
    IF (ICHAR(current(1:1)) == 0) RETURN

    ! Populate the block
    CALL this%load_block(current, iblock, icoblock)
    cap_bits = icoblock%cap_bits
    icoblock%charindex = charindex
    icoblock%full_line_pos = trim_charindex
    IF (iblock%ptype == eis_pt_bad) THEN
      err = eis_err_not_found
      CALL this%err_handler%add_error(eis_err_parser, err, current, charindex, &
        filename = filename, line_number = line_number, full_line = full_line, &
        full_line_pos = trim_charindex)
      RETURN
    END IF

    IF (iblock%ptype == eis_pt_null) RETURN

    IF (iblock%ptype == eis_pt_character) THEN
      IF (allow_text) THEN
        IF (this%last_block_type == eis_pt_parenthesis &
            .OR. this%last_block_type == eis_pt_parenthesis &
            .OR. this%last_block_type == eis_pt_null) THEN
          !Turn the block into a numerical constant with a value
          !of the index of the string to look up
          iblock%ptype = eis_pt_constant
          iblock%can_simplify = .FALSE.
          iblock%value = eis_pt_character
          !First check for already existing string to avoid repetition
          !Consider switching to some kind of O(ln(n)) or O(1) store
          DO istr = 1, this%string_param_store%get_size()
            str_handled = this%string_param_store%get(istr, str)
            IF (.NOT. str_handled) CYCLE
            IF (str == icoblock%text(2:LEN(icoblock%text)-1)) THEN
              iblock%numerical_data = istr
              str_handled = .TRUE.
              EXIT
            END IF
            str_handled = .FALSE.
          END DO
          IF (ALLOCATED(str)) DEALLOCATE(str)
          IF (.NOT. str_handled) THEN
            iblock%numerical_data = this%string_param_store%store(&
                icoblock%text(2:LEN(icoblock%text)-1))
          END IF
          IF (this%stack%stack_point > 1) THEN
            this%stack%entries(this%stack%stack_point-1)%has_string_params &
                = .TRUE.
          END IF 
        ELSE
          err = IOR(err, eis_err_malformed)
          CALL this%err_handler%add_error(eis_err_parser, err, current, &
            charindex, filename = filename, line_number = line_number, &
            full_line = full_line, full_line_pos = trim_charindex)
          RETURN
        END IF
      ELSE
        err = IOR(err, eis_err_bad_value)
        CALL this%err_handler%add_error(eis_err_parser, err, current, &
          charindex, filename = filename, line_number = line_number, &
          full_line = full_line, full_line_pos = trim_charindex)
        RETURN
      END IF
    END IF

    !Character variables must be followed by either a comma or a close bracket
    IF (this%last_block_type == eis_pt_character .AND. &
          .NOT. (iblock%ptype == eis_pt_separator &
          .OR. (iblock%ptype == eis_pt_parenthesis &
          .AND. iblock%value == eis_paren_right_bracket))) THEN
      err = IOR(err, eis_err_bad_value)
      CALL this%err_handler%add_error(eis_err_parser, err, current, &
        charindex, filename = filename, line_number = line_number, &
        full_line = full_line, full_line_pos = trim_charindex)
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
        CALL pop_to_stack(this%stack, output)
        CALL pop_to_null(this%brackets)
      ELSE
        err = IOR(err, eis_err_malformed)
        CALL this%err_handler%add_error(eis_err_parser, err, &
            this%last_block_text, this%last_charindex, &
            filename = filename, line_number = line_number, &
            full_line = full_line, full_line_pos = trim_charindex)
        RETURN
      END IF
    END IF

    !Open brackets must not be preceeded by a constant or variable
    IF (iblock%ptype == eis_pt_parenthesis &
        .AND. iblock%value == eis_paren_left_bracket) THEN
      IF (this%last_block_type == eis_pt_variable &
          .OR. this%last_block_type == eis_pt_pointer_variable &
          .OR. this%last_block_type == eis_pt_constant) THEN
        err = IOR(err, eis_err_bracketed_constant)
        CALL this%err_handler%add_error(eis_err_parser, err, &
            this%last_block_text, this%last_charindex, &
            filename = filename, line_number = line_number, &
            full_line = full_line, full_line_pos = trim_charindex)
        RETURN
      END IF
    END IF

    !If previous block was an operator than almost anything else is valid
    !except a separator or a right bracket. Don't need to check for characters
    !because this will be checked as a parameter to the operator
    IF (this%last_block_type == eis_pt_operator .AND. &
        (iblock%ptype == eis_pt_separator &
        .OR. (iblock%ptype == eis_pt_parenthesis &
        .AND. iblock%value == eis_paren_right_bracket))) THEN
      err = IOR(err, eis_err_malformed)
      CALL this%err_handler%add_error(eis_err_parser, err, &
          this%last_block_text, this%last_charindex, filename = filename, &
          line_number = line_number, full_line = full_line, &
          full_line_pos = trim_charindex)
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
              charindex, filename = filename, line_number = line_number, &
              full_line = full_line, full_line_pos = trim_charindex)
        END IF
      ELSE !No ternary operators so must be unary
        IF (.NOT. (this%last_block_type == eis_pt_null &
            .OR. this%last_block_type == eis_pt_separator &
            .OR. this%last_block_type == eis_pt_operator &
            .OR. (this%last_block_type == eis_pt_parenthesis &
            .AND. this%last_block_value == eis_paren_left_bracket))) THEN
          err = IOR(err, eis_err_malformed)
          CALL this%err_handler%add_error(eis_err_parser, err, current, &
              charindex, filename = filename, line_number = line_number, &
              full_line = full_line, full_line_pos = trim_charindex)
        END IF
      END IF
    END IF

    output%has_deferred = output%has_deferred .OR. icoblock%defer

    IF (iblock%ptype == eis_pt_variable &
        .OR. iblock%ptype == eis_pt_pointer_variable &
        .OR. iblock%ptype == eis_pt_constant) THEN
      CALL push_to_stack(output, iblock, icoblock)

    ELSE IF (iblock%ptype == eis_pt_stored_variable) THEN
      IF (.NOT. icoblock%defer) THEN
        CALL this%registry%copy_in_stored(iblock%value, output, &
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
                        this%stack%co_entries(this%stack%stack_point)%charindex&
                        , filename = filename, line_number = line_number, &
                        full_line = full_line, full_line_pos = trim_charindex)
                  ELSE
                    CALL this%err_handler%add_error(eis_err_parser, err, &
                        "{unknown}", charindex)
                  END IF
                END IF

                IF ((this%stack%entries(this%stack%stack_point) &
                    %has_string_params .AND. .NOT. &
                    this%stack%co_entries(this%stack%stack_point)% &
                    can_have_string_params)) THEN
                  err = IOR(err, eis_err_text)
                  IF (ALLOCATED(this%stack%co_entries)) THEN
                    CALL this%err_handler%add_error(eis_err_parser, err, &
                        this%stack%co_entries(this%stack%stack_point)%text, &
                        this%stack%co_entries(this%stack%stack_point)%charindex&
                        , filename = filename, line_number = line_number, &
                        full_line = full_line, full_line_pos = trim_charindex)
                  ELSE
                    CALL this%err_handler%add_error(eis_err_parser, err, &
                        "{unknown}", charindex)
                  END IF
                END IF
                CALL pop_to_stack(this%stack, output)
                CALL pop_to_null(this%brackets)
              END IF
            END IF
            EXIT
          ELSE
            CALL pop_to_stack(this%stack, output, stack_empty)
            IF (stack_empty) THEN
              err = IOR(err, eis_err_extra_bracket)
              CALL this%err_handler%add_error(eis_err_parser, err, &
                  ")", charindex, filename = filename, &
                  line_number = line_number, full_line = full_line, &
                  full_line_pos = trim_charindex)
              RETURN
            END IF
          END IF
        END DO
      END IF

    ELSE IF (iblock%ptype == eis_pt_function .OR. iblock%ptype &
        == eis_pt_emplaced_function) THEN
      IF (iblock%ptype == eis_pt_emplaced_function) THEN
          output%has_emplaced = .TRUE.
      END IF
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
            CALL pop_to_stack(this%stack, output)
          ELSE
            IF (block2%value /= eis_paren_left_bracket) THEN
              err = IOR(err, eis_err_malformed)
              CALL this%err_handler%add_error(eis_err_parser, err, current, &
                  charindex, filename = filename, line_number = line_number, &
                  full_line = full_line, full_line_pos = trim_charindex)
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
              CALL pop_to_stack(this%stack, output)
              CYCLE
            ELSE
              CALL push_to_stack(this%stack, iblock, icoblock)
              EXIT
            END IF
          ELSE
            IF (icoblock%precedence &
                < coblock2%precedence) THEN
              CALL pop_to_stack(this%stack, output)
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
      IF (iblock%ptype == eis_pt_constant &
          .AND. iblock%value == eis_pt_character) &
          this%last_block_type = eis_pt_character
      this%last_block_value = iblock%value
      this%last_charindex = charindex
      IF (ALLOCATED(this%last_block_text)) THEN
        DEALLOCATE(this%last_block_text)
        ALLOCATE(this%last_block_text, SOURCE = current)
      END IF
    END IF

  END SUBROUTINE eip_tokenize_subexpression_infix



  !> @brief
  !> Combine multiple stacks. Optionally combine as parameters to a function
  !> or operator
  !> @param[inout] this
  !> @param[in] stacks
  !> @param[inout] output
  !> @param[in] append
  !> @param[out] errcode
  !> @param[in] function_str
  SUBROUTINE eip_combine_stacks(this, stacks, output, errcode, append, &
      function_str)
    CLASS(eis_parser), INTENT(INOUT) :: this
    !> Array of stacks to combine
    TYPE(eis_stack), DIMENSION(:), INTENT(IN) :: stacks
    !> Stack to hold the combined stack
    TYPE(eis_stack), INTENT(INOUT) :: output
    !> Error code
    INTEGER(eis_error), INTENT(OUT) :: errcode
    !> Should the new stacks be appended to the existing stack?
    LOGICAL, INTENT(IN), OPTIONAL :: append
    !> Optional string for function to use to combine the stacks
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: function_str
    LOGICAL :: should_append
    INTEGER :: iel, expected_params
    TYPE(eis_stack_element) :: iblock
    TYPE(eis_stack_co_element) :: icoblock

    errcode = eis_err_none

    should_append = .FALSE.
    IF (PRESENT(append)) should_append = .FALSE.
    IF (.NOT. should_append .OR. .NOT. output%init) &
        CALL initialise_stack(output)
    IF (SIZE(stacks) == 0) RETURN

    IF (PRESENT(function_str)) THEN
      CALL this%load_block(function_str, iblock, icoblock)
      IF (iblock%ptype /= eis_pt_function &
          .AND. iblock%ptype /= eis_pt_operator &
          .AND. iblock%ptype /= eis_pt_emplaced_function) RETURN
      IF (icoblock%expected_params >= 0 &
          .AND. icoblock%expected_params /= SIZE(stacks)) RETURN
    END IF

    DO iel = 1, SIZE(stacks)
      IF (.NOT. stacks(iel)%init) THEN
        errcode = eis_err_bad_stack
        RETURN
      END IF
      CALL append_stack(output, stacks(iel))
    END DO

    IF (PRESENT(function_str)) THEN
      iblock%actual_params = SIZE(stacks)
      CALL push_to_stack(output, iblock, icoblock)
    END IF

  END SUBROUTINE eip_combine_stacks



  !> @brief
  !> Print all of the errors to stdout
  !> @param[inout] this
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
  !> @param[out] error_filename
  !> @param[out] error_line_number
  SUBROUTINE eip_get_error_info(this, index, error_cause, &
      error_cause_location, error_phase, error_type, error_filename, &
      error_line_number)

    CLASS(eis_parser), INTENT(IN) :: this
    !> Index of error to get report on. Must be between 1 and
    !> the result of eip_get_error_count
    INTEGER, INTENT(IN) :: index
    !> Reports the string representation of the part of the string that
    !> caused the error. Optional, default is not return this info
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT) :: error_cause
    !> Reports the character offset location of the cause of the error
    !> Optional, default is not report this info
    INTEGER, INTENT(OUT), OPTIONAL :: error_cause_location
    !> Reports the string representation of where in the parsing phase the
    !> error occured. Optional, default is not report this info
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT) :: error_phase
    !> Reports the string representation of the type of error that was
    !> encountered during the parsing. Optional, default is no report this info
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT) :: error_type
    !> Reports the file name of the file which contains the error
    !> Optional, default is not report this info
    CHARACTER(LEN=:), ALLOCATABLE, OPTIONAL, INTENT(OUT) :: error_filename
    !> Reports the line number within the file which contains the error
    !> Optional, default is not report this info
    INTEGER, INTENT(OUT), OPTIONAL :: error_line_number
    CHARACTER(LEN=:), ALLOCATABLE :: ec, eps, ets, efn
    INTEGER :: ecl, eln

    CALL this%err_handler%get_error_cause(index, ec, ecl,filename=efn, &
        line_number = eln)
    CALL this%err_handler%get_error_string(index, ets, eps)

    IF (PRESENT(error_cause)) CALL MOVE_ALLOC(ec, error_cause)
    IF (PRESENT(error_cause_location)) error_cause_location = ecl
    IF (PRESENT(error_phase)) CALL MOVE_ALLOC(eps, error_phase)
    IF (PRESENT(error_type)) CALL MOVE_ALLOC(ets, error_type)
    IF (PRESENT(error_filename)) CALL MOVE_ALLOC(efn, error_type)
    IF (PRESENT(error_line_number)) error_line_number = eln

    IF (ALLOCATED(ec)) DEALLOCATE(ec)
    IF (ALLOCATED(eps)) DEALLOCATE(eps)
    IF (ALLOCATED(ets)) DEALLOCATE(ets)

  END SUBROUTINE eip_get_error_info



  !> @brief
  !> Flush all of the errors in the list of errors
  !> @param[inout] this
  SUBROUTINE eip_flush_errors(this)
    CLASS(eis_parser) :: this
    CALL this%err_handler%flush_errors()
  END SUBROUTINE eip_flush_errors



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
  !> @param[in] nformat
  SUBROUTINE eip_visualize_stack(this, stack_in, str_out, nformat)
    CLASS(eis_parser), INTENT(IN) :: this
    CLASS(eis_stack), INTENT(IN) :: stack_in !< Stack input
    !> String to hold the output of the stack as a dot file
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: str_out
    !> Optional character string to format numerical literals
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: nformat

    CALL eis_visualise_stack(stack_in, str_out, nformat)

  END SUBROUTINE eip_visualize_stack



  !> @brief
  !> Evaluate a stack without using objects
  !> @param[inout] stack
  !> @param[out] result
  !> @param[inout] errcode
  !> @param[in] host_params
  !> @param[in] eval
  !> @return eis_fast_evaluate
  FUNCTION eis_fast_evaluate(stack, result, errcode, host_params, eval)
    TYPE(eis_stack), INTENT(INOUT) :: stack !< Stack to evaluate
    !> Allocatable array holding all the results from the evaluation.
    !> Will only be reallocated if it is too small to hold all the results
    REAL(eis_num), DIMENSION(:), INTENT(INOUT), ALLOCATABLE :: result
    !> Error code describing any errors that occured
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Host code parameters provided. Optional, default no values (C_PTR_NULL)
    TYPE(C_PTR), INTENT(IN), OPTIONAL :: host_params
    !> Evaluator object. Optional, default use internal
    TYPE(eis_eval_stack), INTENT(INOUT), OPTIONAL, TARGET :: eval
    !> Number of results returned by the evaluation
    INTEGER :: eis_fast_evaluate
    TYPE(C_PTR) :: params
    TYPE(eis_eval_stack), SAVE, TARGET :: eval_common
    TYPE(eis_eval_stack), POINTER :: eval_ptr

    IF (PRESENT(host_params)) THEN
      params = host_params
    ELSE
      params = C_NULL_PTR
    END IF

    IF (PRESENT(eval)) THEN
      eval_ptr => eval
    ELSE
      eval_ptr => eval_common
    END IF

    eis_fast_evaluate = ees_evaluate_fast(eval_ptr, stack, result, params, &
        errcode)

  END FUNCTION eis_fast_evaluate



  !> @brief
  !> Evaluate a stack without using objects and using iterator function
  !> @param[inout] stack
  !> @param[out] result
  !> @param[inout] errcode
  !> @param[in] host_params
  !> @param[in] iter_fn
  !> @param[in] store_fn
  !> @param[in] eval
  !> @return eis_fast_evaluate
  SUBROUTINE eis_iter_evaluate(stack, host_params, iter_fn, store_fn, eval)
    TYPE(eis_stack), INTENT(INOUT) :: stack !< Stack to evaluate
    !> Host code parameters provided.
    TYPE(C_PTR), INTENT(IN) :: host_params
    !> Function to advance host_params to next iteration and return whether or
    !> not to keep iterating
    PROCEDURE(parser_param_update_fn) :: iter_fn
    !> Function to store the results of the evaluation
    PROCEDURE(parser_store_data_fn) :: store_fn
    TYPE(eis_eval_stack), INTENT(INOUT), OPTIONAL, TARGET :: eval
    !> Number of results returned by the evaluation
    TYPE(eis_eval_stack), SAVE, TARGET :: eval_common
    TYPE(eis_eval_stack), POINTER :: eval_ptr

    IF (PRESENT(eval)) THEN
      eval_ptr => eval
    ELSE
      eval_ptr => eval_common
    END IF

    CALL ees_evaluate_iter(eval_ptr, stack, host_params, iter_fn, store_fn)

  END SUBROUTINE eis_iter_evaluate



  !> @brief
  !> Optimise the global and local registries
  !> @param[inout] this
  SUBROUTINE eip_optimise(this)
    CLASS(eis_parser), INTENT(INOUT) :: this

    CALL this%registry%optimise()
    CALL global_registry%optimise()

  END SUBROUTINE eip_optimise



  !> @brief
  !> Optimise the global and local registries
  !> @param[inout] this
  !> @param[in] index
  !> @param[in] str
  !> @param[out] errcode
  SUBROUTINE eip_get_text(this, index, str, errcode)
    CLASS(eis_parser), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: index !< Index of string to retrieve
    !> String to retieve
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: str
    !> Error code
    INTEGER(eis_error), INTENT(OUT) :: errcode

    errcode = eis_err_none
    IF (.NOT. this%string_param_store%get(index, str)) THEN
      errcode = eis_err_bad_value
    END IF
  END SUBROUTINE eip_get_text



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get the number of symbols in the global registry
  !> @param[in] this
  !> @return symbol_count
  FUNCTION eip_get_global_symbol_count(this) &
      RESULT(symbol_count)

    CLASS(eis_parser), INTENT(INOUT) :: this
    !> Number of symbols
    INTEGER :: symbol_count

    symbol_count = this%get_registry_symbol_count(global_registry)

  END FUNCTION eip_get_global_symbol_count



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get a symbol from the global registry
  !> @param[in] this
  !> @param[in] index
  !> @param[out] symbol
  !> @return symbol_count
  SUBROUTINE eip_get_global_symbol(this, index, symbol)

    CLASS(eis_parser), INTENT(INOUT) :: this
    !> Index of the symbol to retrieve
    INTEGER, INTENT(IN) :: index
    !> Retreived symbol
    CHARACTER(LEN=:), INTENT(OUT), ALLOCATABLE :: symbol

    CALL this%get_registry_symbol(global_registry, index, symbol)

  END SUBROUTINE eip_get_global_symbol



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get the number of symbols in the parser local registry
  !> @param[in] this
  !> @return symbol_count
  FUNCTION eip_get_local_symbol_count(this) &
      RESULT(symbol_count)

    CLASS(eis_parser), INTENT(INOUT) :: this
    !> Number of symbols
    INTEGER :: symbol_count

    symbol_count = this%get_registry_symbol_count(this%registry)

  END FUNCTION eip_get_local_symbol_count



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get a symbol from the parser local registry
  !> @param[in] this
  !> @param[in] index
  !> @param[out] symbol
  SUBROUTINE eip_get_local_symbol(this, index, symbol)

    CLASS(eis_parser), INTENT(INOUT) :: this
    !> Index of the symbol to retrieve
    INTEGER, INTENT(IN) :: index
    !> Retreived symbol
    CHARACTER(LEN=:), INTENT(OUT), ALLOCATABLE :: symbol

    CALL this%get_registry_symbol(this%registry, index, symbol)

  END SUBROUTINE eip_get_local_symbol



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get the number of symbols in both local and global registries
  !> @param[in] this
  !> @return symbol_count
  FUNCTION eip_get_symbol_count(this) &
      RESULT(symbol_count)

    CLASS(eis_parser), INTENT(INOUT) :: this
    !> Number of symbols
    INTEGER :: symbol_count

    symbol_count = this%get_global_symbol_count() &
        + this%get_local_symbol_count()

  END FUNCTION eip_get_symbol_count



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get a symbol from either the local or the global registry
  !> @param[in] this
  !> @param[in] index
  !> @param[out] symbol
  SUBROUTINE eip_get_symbol(this, index, symbol)

    CLASS(eis_parser), INTENT(INOUT) :: this
    !> Index of the symbol to retrieve
    INTEGER, INTENT(IN) :: index
    !> Retreived symbol
    CHARACTER(LEN=:), INTENT(OUT), ALLOCATABLE :: symbol
    INTEGER :: gsc

    gsc = this%get_global_symbol_count()

    IF (index <= gsc) THEN
      CALL this%get_global_symbol(index, symbol)
    ELSE
      CALL this%get_local_symbol(index - gsc, symbol)
    END IF

  END SUBROUTINE eip_get_symbol



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get a representation of the keys in the parser as markdown
  !> @param[in] this
  !> @param[out] markdown
  !> @param[in] title
  !> @param[in] function_show_name
  SUBROUTINE eip_get_structure_as_markdown(this, markdown, title, &
      function_show_name)

    CLASS(eis_parser), INTENT(INOUT) :: this
    !> Markdown variable
    CHARACTER(LEN=:), INTENT(OUT), ALLOCATABLE :: markdown
    !> Title of the document. Optional, default no title
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: title
    !> Built in functions are given descriptions that include the
    !> name so printing the name is confusing. Specify eis_fsn_none to never 
    !> show the function name, eis_fsn_always to always show the name and
    !> eis_fsn_auto to automatically determine if the first characters of the
    !> description are the name
    INTEGER, INTENT(IN), OPTIONAL :: function_show_name
    CHARACTER(LEN=:), ALLOCATABLE :: sname, desc, name2
    INTEGER :: ct, stype, fsn, dotloc, expected_params
    LOGICAL :: exists, pname, any_printed, is_hidden

    IF (PRESENT(title)) THEN
      CALL eis_append_string(markdown,'# '//title)
    END IF

    fsn = eis_fsn_auto
    IF (PRESENT(function_show_name)) fsn = function_show_name

    CALL eis_append_string(markdown,'')
    CALL eis_append_string(markdown,'## Functions')
    CALL eis_append_string(markdown,'')
    any_printed = .FALSE.
    DO ct = 1, this%get_symbol_count()
      CALL this%get_symbol(ct, sname)
      exists = this%get_symbol_info(sname, description = desc, &
          symbol_type = stype, is_hidden = is_hidden, &
          expected_params = expected_params)
      IF (is_hidden) CYCLE
      IF ((stype == eis_pt_function .OR. stype == eis_pt_emplaced_function) &
          .AND. expected_params /= 0) THEN
        any_printed = .TRUE.
        IF (this%symbol_needs_namespace(sname)) THEN
          ALLOCATE(name2, SOURCE = sname)
        ELSE
          dotloc = INDEX(sname, '.', BACK = .TRUE.)
          IF (dotloc > 0 .AND. dotloc < LEN(sname)) THEN
            ALLOCATE(name2, SOURCE = sname(dotloc+1:))
          ELSE
            ALLOCATE(name2, SOURCE = sname)
          END IF
        END IF
        IF (fsn == eis_fsn_never) THEN
          pname = .FALSE.
        ELSE IF (fsn == eis_fsn_always) THEN
          pname = .TRUE.
        ELSE
          IF (ALLOCATED(desc)) THEN
            pname = .TRUE.
            IF (LEN(desc) >= LEN(name2)) THEN
              IF (desc(1:LEN(name2)) == name2) pname = .FALSE.
            END IF
            IF (LEN(desc) > LEN(name2)) THEN
              IF (desc(1:1) == '`' .AND. desc(2:LEN(name2)+1) == name2) &
                  pname = .FALSE.
            END IF
          ELSE
            pname = .TRUE.
          END IF
        END IF
        IF (pname) THEN
          IF (ALLOCATED(desc)) THEN
            CALL eis_append_string(markdown, '* `' // name2 // '` - ' // desc)
          ELSE
            CALL eis_append_string(markdown, '* `' // name2 // '` - &
            &No description')
          END IF
        ELSE
          CALL eis_append_string(markdown, '* ' // desc)
        END IF
        CALL eis_append_string(markdown, '')
        DEALLOCATE(name2)
      END IF
    END DO
    IF (.NOT. any_printed) CALL eis_append_string(markdown, 'None found')

    CALL eis_append_string(markdown,'')
    CALL eis_append_string(markdown,'## Operators')
    CALL eis_append_string(markdown,'')
    any_printed = .FALSE.
    DO ct = 1, this%get_symbol_count()
      CALL this%get_symbol(ct, sname)
      exists = this%get_symbol_info(sname, description = desc, &
          symbol_type = stype, is_hidden = is_hidden)
      IF (is_hidden) CYCLE
      IF (stype == eis_pt_operator) THEN
        any_printed = .TRUE.
        IF (this%symbol_needs_namespace(sname)) THEN
          ALLOCATE(name2, SOURCE = sname)
        ELSE
          dotloc = INDEX(sname, '.', BACK = .TRUE.)
          IF (dotloc > 0 .AND. dotloc < LEN(sname)) THEN
            ALLOCATE(name2, SOURCE = sname(dotloc+1:))
          ELSE
            ALLOCATE(name2, SOURCE = sname)
          END IF
        END IF
        IF (ALLOCATED(desc)) THEN
          CALL eis_append_string(markdown, '* `' // name2 // '` - ' // desc)
        ELSE
          CALL eis_append_string(markdown, '* `' // name2 &
              // '` - No description')
        END IF
        CALL eis_append_string(markdown, '')
        DEALLOCATE(name2)
      END IF
    END DO
    IF (.NOT. any_printed) CALL eis_append_string(markdown, 'None found')

    CALL eis_append_string(markdown,'')
    CALL eis_append_string(markdown,'## Constants')
    CALL eis_append_string(markdown,'')
    any_printed = .FALSE.
    DO ct = 1, this%get_symbol_count()
      CALL this%get_symbol(ct, sname)
      exists = this%get_symbol_info(sname, description = desc, &
          symbol_type = stype, is_hidden = is_hidden)
      IF (is_hidden) CYCLE
      IF (stype == eis_pt_constant) THEN
        any_printed = .TRUE.
        IF (this%symbol_needs_namespace(sname)) THEN
          ALLOCATE(name2, SOURCE = sname)
        ELSE
          dotloc = INDEX(sname, '.', BACK = .TRUE.)
          IF (dotloc > 0 .AND. dotloc < LEN(sname)) THEN
            ALLOCATE(name2, SOURCE = sname(dotloc+1:))
          ELSE
            ALLOCATE(name2, SOURCE = sname)
          END IF
        END IF
        IF (ALLOCATED(desc)) THEN
          CALL eis_append_string(markdown, '* `' // name2 // '` - ' // desc)
        ELSE
          CALL eis_append_string(markdown, '* `' // name2 &
              // '` - No description')
        END IF
        CALL eis_append_string(markdown, '')
        DEALLOCATE(name2)
      END IF
    END DO
    IF (.NOT. any_printed) CALL eis_append_string(markdown, 'None found')

    CALL eis_append_string(markdown,'')
    CALL eis_append_string(markdown,'## Variables')
    CALL eis_append_string(markdown,'')
    any_printed = .FALSE.
    DO ct = 1, this%get_symbol_count()
      CALL this%get_symbol(ct, sname)
      exists = this%get_symbol_info(sname, description = desc, &
          symbol_type = stype, is_hidden = is_hidden)
      IF (is_hidden) CYCLE
      IF (stype == eis_pt_variable .OR. stype == eis_pt_stored_variable &
          .OR. stype == eis_pt_emplaced_variable &
          .OR. stype == eis_pt_pointer_variable &
          .OR. (stype == eis_pt_function &
          .OR. stype == eis_pt_emplaced_function) .AND. expected_params == 0) &
          THEN
        any_printed = .TRUE.
        IF (this%symbol_needs_namespace(sname)) THEN
          ALLOCATE(name2, SOURCE = sname)
        ELSE
          dotloc = INDEX(sname, '.', BACK = .TRUE.)
          IF (dotloc > 0 .AND. dotloc < LEN(sname)) THEN
            ALLOCATE(name2, SOURCE = sname(dotloc+1:))
          ELSE
            ALLOCATE(name2, SOURCE = sname)
          END IF
        END IF
        IF (ALLOCATED(desc)) THEN
          CALL eis_append_string(markdown, '* `' // name2 // '` - ' // desc)
        ELSE
          CALL eis_append_string(markdown, '* `' // name2 &
              // '` - No description')
        END IF
        CALL eis_append_string(markdown, '')
        DEALLOCATE(name2)
      END IF
    END DO
    IF (.NOT. any_printed) CALL eis_append_string(markdown, 'None found')

  END SUBROUTINE eip_get_structure_as_markdown

END MODULE eis_parser_mod
