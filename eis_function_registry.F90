MODULE eis_function_registry_mod

  USE eis_header
  USE eis_named_store_mod

  IMPLICIT NONE

  TYPE :: eis_function_entry
    PROCEDURE(parser_eval_fn), POINTER, NOPASS :: fn_ptr => NULL()
    PROCEDURE(parser_eval_fn), POINTER, NOPASS :: unary_fn => NULL()
    INTEGER :: ptype = c_pt_null
    INTEGER :: associativity = c_assoc_null
    INTEGER :: precedence = 0
    INTEGER :: expected_parameters = -1
  END TYPE eis_function_entry

  TYPE :: eis_registry
    PRIVATE
    TYPE(named_store) :: const_table
    TYPE(named_store) :: fn_table
    TYPE(named_store) :: op_table
    TYPE(named_store) :: uop_table
    CONTAINS
    PROCEDURE, PUBLIC :: add_constant => eir_add_constant
    PROCEDURE, PUBLIC :: add_function => eir_add_function
    PROCEDURE, PUBLIC :: add_operator => eir_add_operator
    PROCEDURE, PUBLIC :: fill_block => eir_fill_block
  END TYPE eis_registry

  PRIVATE
  PUBLIC :: eis_registry

CONTAINS



  SUBROUTINE eir_add_constant(this, name, fn)

    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    PROCEDURE(parser_eval_fn) :: fn
    TYPE(eis_function_entry) :: temp

    temp%fn_ptr => fn
    temp%ptype = c_pt_function

    CALL this%const_table%store(name, temp)

  END SUBROUTINE eir_add_constant



  SUBROUTINE eir_add_function(this, name, fn, expected_parameters)

    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    PROCEDURE(parser_eval_fn) :: fn
    INTEGER, INTENT(IN) :: expected_parameters
    TYPE(eis_function_entry) :: temp

    temp%fn_ptr => fn
    temp%ptype = c_pt_function
    temp%expected_parameters = expected_parameters

    CALL this%fn_table%store(name, temp)

  END SUBROUTINE eir_add_function



  SUBROUTINE eir_add_operator(this, name, fn, associativity, precedence, unary)

    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    PROCEDURE(parser_eval_fn) :: fn
    INTEGER, INTENT(IN) :: associativity, precedence
    LOGICAL, OPTIONAL :: unary
    TYPE(eis_function_entry) :: temp
    LOGICAL :: l_unary

    temp%fn_ptr => fn
    temp%ptype = c_pt_operator
    temp%associativity = associativity
    temp%precedence = precedence

    l_unary = .FALSE.
    IF (PRESENT(unary)) l_unary = unary

    IF (.NOT. l_unary) THEN
      CALL this%op_table%store(name, temp)
    ELSE
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
    ELSE
      block_in%ptype = c_pt_bad
    END IF

  END SUBROUTINE eir_fill_block

END MODULE eis_function_registry_mod
