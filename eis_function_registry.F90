MODULE eis_function_registry_mod

  USE eis_header
  USE eis_stack_mod
  USE eis_named_store_mod
  USE eis_ordered_store_mod

  IMPLICIT NONE

  TYPE :: late_bind_fn_holder
    PROCEDURE(parser_late_bind_fn), POINTER, NOPASS :: contents
  END TYPE late_bind_fn_holder

  TYPE :: eis_function_entry
    PROCEDURE(parser_eval_fn), POINTER, NOPASS :: fn_ptr => NULL()
    LOGICAL :: can_simplify = .TRUE.
    INTEGER(eis_i4) :: value = 0_eis_i4
    REAL(eis_num) :: numerical_data = 0.0_eis_num
    INTEGER :: ptype = c_pt_null
    INTEGER :: associativity = c_assoc_null
    INTEGER :: precedence = 0
    INTEGER :: expected_parameters = -1
    INTEGER(eis_bitmask) :: cap_bits = 0_eis_bitmask
  END TYPE eis_function_entry

  TYPE :: eis_registry
    PRIVATE
    TYPE(named_store) :: generic_table !< Contains all other functions etc.
    TYPE(named_store) :: uop_table !< Contains named unary operators
    TYPE(ordered_store) :: stack_variable_registry
    TYPE(ordered_store) :: stack_function_registry !< Contains deferred stacks

    CONTAINS
    
    PROCEDURE, PUBLIC :: add_constant => eir_add_constant
    PROCEDURE, PUBLIC :: add_variable => eir_add_variable
    PROCEDURE, PUBLIC :: add_function => eir_add_function
    PROCEDURE, PUBLIC :: add_operator => eir_add_operator
    PROCEDURE, PUBLIC :: add_stack_variable => eir_add_stack_var
    PROCEDURE, PUBLIC :: add_stack_function => eir_add_stack_function
    PROCEDURE, PUBLIC :: fill_block => eir_fill_block
    PROCEDURE, PUBLIC :: copy_in_stored => eir_copy_in
  END TYPE eis_registry

  PRIVATE
  PUBLIC :: eis_registry

CONTAINS



  SUBROUTINE eir_add_constant(this, name, value, errcode, can_simplify, &
      cap_bits)

    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    REAL(eis_num), INTENT(IN) :: value
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    LOGICAL, INTENT(IN), OPTIONAL :: can_simplify
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits
    TYPE(eis_function_entry) :: temp
    
    temp%ptype = c_pt_constant
    temp%numerical_data = value
    IF (PRESENT(can_simplify)) temp%can_simplify = can_simplify
    IF (PRESENT(cap_bits)) temp%cap_bits = cap_bits

!    CALL this%const_table%store(name, temp)
    CALL this%generic_table%store(name, temp)

  END SUBROUTINE eir_add_constant



  SUBROUTINE eir_add_variable(this, name, fn, errcode, can_simplify, &
      cap_bits)

    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    PROCEDURE(parser_eval_fn) :: fn
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    LOGICAL, INTENT(IN), OPTIONAL :: can_simplify
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits
    TYPE(eis_function_entry) :: temp

    temp%ptype = c_pt_variable
    temp%fn_ptr => fn
    IF (PRESENT(can_simplify)) temp%can_simplify = can_simplify
    IF (PRESENT(cap_bits)) temp%cap_bits = cap_bits

    CALL this%generic_table%store(name, temp)

  END SUBROUTINE eir_add_variable



  SUBROUTINE eir_add_function(this, name, fn, expected_parameters, errcode, &
      can_simplify, cap_bits)

    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    PROCEDURE(parser_eval_fn) :: fn
    INTEGER, INTENT(IN) :: expected_parameters
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    LOGICAL, OPTIONAL, INTENT(IN) :: can_simplify
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits
    TYPE(eis_function_entry) :: temp

    temp%fn_ptr => fn
    temp%ptype = c_pt_function
    temp%expected_parameters = expected_parameters
    IF (PRESENT(can_simplify)) temp%can_simplify = can_simplify
    IF (PRESENT(cap_bits)) temp%cap_bits = cap_bits

    CALL this%generic_table%store(name, temp)

  END SUBROUTINE eir_add_function



  SUBROUTINE eir_add_operator(this, name, fn, associativity, precedence, &
      errcode, can_simplify, unary, cap_bits)

    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    PROCEDURE(parser_eval_fn) :: fn
    INTEGER, INTENT(IN) :: associativity, precedence
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    LOGICAL, OPTIONAL :: can_simplify
    LOGICAL, OPTIONAL :: unary
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits
    TYPE(eis_function_entry) :: temp
    LOGICAL :: l_unary

    temp%fn_ptr => fn
    temp%ptype = c_pt_operator
    temp%associativity = associativity
    temp%precedence = precedence
    IF (PRESENT(can_simplify)) temp%can_simplify = can_simplify
    IF (PRESENT(cap_bits)) temp%cap_bits = cap_bits

    l_unary = .FALSE.
    IF (PRESENT(unary)) l_unary = unary

    IF (.NOT. l_unary) THEN
      temp%expected_parameters = 2
      CALL this%generic_table%store(name, temp)
    ELSE
      temp%expected_parameters = 1
      CALL this%uop_table%store(name, temp)
    END IF

  END SUBROUTINE eir_add_operator



  SUBROUTINE eir_add_stack_var(this, name, stack, errcode)
    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    TYPE(eis_stack), INTENT(IN) :: stack
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    TYPE(eis_function_entry) :: temp
    INTEGER(eis_i4) :: index

    index = this%stack_variable_registry%store(stack)
    temp%ptype = c_pt_stored_variable
    temp%value = index
    CALL this%generic_table%store(name, temp)

  END SUBROUTINE eir_add_stack_var



  SUBROUTINE eir_add_stack_function(this, name, def_fn, errcode)
    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    PROCEDURE(parser_late_bind_fn) :: def_fn
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    TYPE(eis_function_entry) :: temp
    TYPE(late_bind_fn_holder) :: holder
    INTEGER(eis_i4) :: index

    holder%contents => def_fn
    index = this%stack_function_registry%store(holder)
    temp%ptype = c_pt_stored_function
    temp%value = index
    CALL this%generic_table%store(name, temp)

  END SUBROUTINE eir_add_stack_function



  SUBROUTINE eir_fill_block(this, name, block_in, coblock_in, unary_ops, &
      cap_bits)

    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    TYPE(eis_stack_element), INTENT(INOUT) :: block_in
    TYPE(eis_stack_co_element), INTENT(INOUT) :: coblock_in
    LOGICAL, INTENT(IN) :: unary_ops
    INTEGER(eis_bitmask), INTENT(OUT) :: cap_bits
    CLASS(*), POINTER :: gptr
    TYPE(eis_function_entry), POINTER :: temp

    temp => NULL()
    gptr => NULL()
    cap_bits = 0_eis_bitmask
    IF (unary_ops) THEN
      gptr => this%uop_table%get(name)
    END IF
    IF (.NOT. ASSOCIATED(gptr)) gptr => this%generic_table%get(name)

    IF (ASSOCIATED(gptr)) THEN
      SELECT TYPE(co => gptr)
        CLASS IS (eis_function_entry)
          temp => co
      END SELECT
    END IF

    IF (ASSOCIATED(temp)) THEN
      block_in%ptype = temp%ptype
      coblock_in%associativity = temp%associativity
      coblock_in%precedence = temp%precedence
      coblock_in%expected_params = temp%expected_parameters
      block_in%actual_params = temp%expected_parameters
      block_in%can_simplify = temp%can_simplify
      block_in%eval_fn => temp%fn_ptr
      block_in%numerical_data = temp%numerical_data
      block_in%value = temp%value
      cap_bits = temp%cap_bits
    ELSE
      block_in%ptype = c_pt_bad
    END IF

  END SUBROUTINE eir_fill_block



  SUBROUTINE eir_copy_in(this, index, output)
    CLASS(eis_registry) :: this
    INTEGER(eis_i4), INTENT(IN) :: index
    TYPE(eis_stack), INTENT(INOUT) :: output
    CLASS(*), POINTER :: gptr
    TYPE(eis_stack), POINTER :: temp

    temp => NULL()
    gptr => this%stack_variable_registry%get(index)
    IF (ASSOCIATED(gptr)) THEN
      SELECT TYPE(co => gptr)
        CLASS IS (eis_stack)
          temp => co
      END SELECT
      IF (ASSOCIATED(temp)) THEN
        CALL append_stack(output, temp)
      ELSE
        PRINT *,'Unable to convert pointer'
      END IF
    ELSE
      PRINT *,'No pointer found from index' , index
    END IF

  END SUBROUTINE eir_copy_in

END MODULE eis_function_registry_mod
