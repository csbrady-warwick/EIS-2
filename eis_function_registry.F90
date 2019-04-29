MODULE eis_function_registry_mod

  USE eis_header
  USE eis_named_store_mod

  IMPLICIT NONE

  TYPE :: eis_function_entry
    PROCEDURE(parser_eval_fn), POINTER, NOPASS :: fn_ptr => NULL()
    LOGICAL :: can_simplify = .TRUE.
    REAL(eis_num) :: value = 0.0_eis_num
    INTEGER :: ptype = c_pt_null
    INTEGER :: associativity = c_assoc_null
    INTEGER :: precedence = 0
    INTEGER :: expected_parameters = -1
    INTEGER :: output_parameters = 1
  END TYPE eis_function_entry

  TYPE :: eis_registry
    PRIVATE
    TYPE(named_store) :: const_table
    TYPE(named_store) :: var_table
    TYPE(named_store) :: fn_table
    TYPE(named_store) :: op_table
    TYPE(named_store) :: uop_table
    CONTAINS
    
    PROCEDURE, PUBLIC :: add_constant => eir_add_constant
    PROCEDURE, PUBLIC :: add_variable => eir_add_variable
    PROCEDURE, PUBLIC :: add_function => eir_add_function
    PROCEDURE, PUBLIC :: add_operator => eir_add_operator
    PROCEDURE, PUBLIC :: fill_block => eir_fill_block
  END TYPE eis_registry

  PRIVATE
  PUBLIC :: eis_registry

CONTAINS



  SUBROUTINE eir_add_constant(this, name, value, can_simplify)

    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    REAL(eis_num), INTENT(IN) :: value
    LOGICAL, INTENT(IN), OPTIONAL :: can_simplify
    TYPE(eis_function_entry) :: temp
    
    temp%ptype = c_pt_constant
    temp%value = value
    IF (PRESENT(can_simplify)) temp%can_simplify = can_simplify

    CALL this%const_table%store(name, temp)

  END SUBROUTINE eir_add_constant



  SUBROUTINE eir_add_variable(this, name, fn, can_simplify)

    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    PROCEDURE(parser_eval_fn) :: fn
    LOGICAL, INTENT(IN), OPTIONAL :: can_simplify
    TYPE(eis_function_entry) :: temp

    temp%ptype = c_pt_variable
    temp%fn_ptr => fn
    IF (PRESENT(can_simplify)) temp%can_simplify = can_simplify

    CALL this%const_table%store(name, temp)

  END SUBROUTINE eir_add_variable



  SUBROUTINE eir_add_function(this, name, fn, expected_parameters, &
      can_simplify, output_parameters)

    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    PROCEDURE(parser_eval_fn) :: fn
    INTEGER, INTENT(IN) :: expected_parameters
    LOGICAL, OPTIONAL, INTENT(IN) :: can_simplify
    INTEGER, OPTIONAL, INTENT(IN) :: output_parameters
    TYPE(eis_function_entry) :: temp

    temp%fn_ptr => fn
    temp%ptype = c_pt_function
    temp%expected_parameters = expected_parameters
    IF (PRESENT(can_simplify)) temp%can_simplify = can_simplify
    IF (PRESENT(output_parameters)) temp%output_parameters = output_parameters

    CALL this%fn_table%store(name, temp)

  END SUBROUTINE eir_add_function



  SUBROUTINE eir_add_operator(this, name, fn, associativity, precedence, &
      can_simplify, unary)

    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    PROCEDURE(parser_eval_fn) :: fn
    INTEGER, INTENT(IN) :: associativity, precedence
    LOGICAL, OPTIONAL :: can_simplify
    LOGICAL, OPTIONAL :: unary
    TYPE(eis_function_entry) :: temp
    LOGICAL :: l_unary

    temp%fn_ptr => fn
    temp%ptype = c_pt_operator
    temp%associativity = associativity
    temp%precedence = precedence
    IF (PRESENT(can_simplify)) temp%can_simplify = can_simplify

    l_unary = .FALSE.
    IF (PRESENT(unary)) l_unary = unary

    IF (.NOT. l_unary) THEN
      temp%expected_parameters = 2
      CALL this%op_table%store(name, temp)
    ELSE
      temp%expected_parameters = 1
      CALL this%uop_table%store(name, temp)
    END IF

  END SUBROUTINE eir_add_operator



  SUBROUTINE eir_fill_block(this, name, block_in, unary_ops)

    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    TYPE(eis_stack_element), INTENT(INOUT) :: block_in
    LOGICAL, INTENT(IN) :: unary_ops
    CLASS(*), POINTER :: gptr
    TYPE(eis_function_entry), POINTER :: temp

    temp => NULL()
    gptr => this%const_table%get(name)
    IF (.NOT. ASSOCIATED(gptr)) gptr => this%var_table%get(name)
    IF (.NOT. ASSOCIATED(gptr)) gptr => this%fn_table%get(name)
    IF (unary_ops) THEN
      IF (.NOT. ASSOCIATED(gptr)) gptr => this%uop_table%get(name)
    ELSE
      IF (.NOT. ASSOCIATED(gptr)) gptr => this%op_table%get(name)
    END IF

    IF (ASSOCIATED(gptr)) THEN
      SELECT TYPE(co => gptr)
        CLASS IS (eis_function_entry)
          temp => co
      END SELECT
    END IF

    IF (ASSOCIATED(temp)) THEN
      block_in%ptype = temp%ptype
      block_in%associativity = temp%associativity
      block_in%precedence = temp%precedence
      block_in%params = temp%expected_parameters
      block_in%actual_params = temp%expected_parameters
      block_in%output_params = temp%output_parameters
      block_in%can_simplify = temp%can_simplify
      block_in%eval_fn => temp%fn_ptr
      block_in%numerical_data = temp%value
    ELSE
      block_in%ptype = c_pt_bad
    END IF

  END SUBROUTINE eir_fill_block

END MODULE eis_function_registry_mod
