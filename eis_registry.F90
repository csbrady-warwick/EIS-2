MODULE eis_registry_mod

  USE eis_header
  USE eis_stack_mod
  USE eis_named_store_mod
  USE eis_ordered_store_mod
  USE eis_error_mod

  IMPLICIT NONE

  TYPE :: late_bind_fn_holder
    PROCEDURE(parser_late_bind_fn), POINTER, NOPASS :: contents
  END TYPE late_bind_fn_holder

  TYPE :: string_holder
    CHARACTER(LEN=:), ALLOCATABLE :: text
    CONTAINS
    FINAL :: sh_destructor
  END TYPE string_holder

  TYPE :: eis_namespace
    PRIVATE
    TYPE(named_store) :: namespaces
    TYPE(named_store) :: generic_store
    TYPE(ordered_store) :: included_namespaces
    CONTAINS
    PROCEDURE, PUBLIC :: store => ern_add_item
    PROCEDURE, PUBLIC :: include_namespace => ern_include_namespace
    PROCEDURE, PUBLIC :: get => ern_get_item
  END TYPE eis_namespace

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
    LOGICAL :: defer = .FALSE.
  END TYPE eis_function_entry

  TYPE :: eis_registry
    PRIVATE
    TYPE(eis_namespace) :: stored_items
    TYPE(named_store) :: uop_table !< Contains named unary operators
    TYPE(ordered_store) :: stack_variable_registry
    TYPE(ordered_store) :: stack_function_registry !< Contains deferred stacks

    CONTAINS

    PROCEDURE, PUBLIC :: include_namespace => eir_include_namespace    
    PROCEDURE, PUBLIC :: add_constant => eir_add_constant
    PROCEDURE, PUBLIC :: add_variable => eir_add_variable
    PROCEDURE, PUBLIC :: add_function => eir_add_function
    PROCEDURE, PUBLIC :: add_operator => eir_add_operator
    PROCEDURE, PUBLIC :: add_stack_variable => eir_add_stack_var
    PROCEDURE, PUBLIC :: add_emplaced_function => eir_add_emplaced_function
    PROCEDURE, PUBLIC :: fill_block => eir_fill_block
    PROCEDURE, PUBLIC :: copy_in_stored => eir_copy_in
    PROCEDURE, PUBLIC :: get_stored_emplacement => eir_get_stored
    
  END TYPE eis_registry

  PRIVATE
  PUBLIC :: eis_registry

CONTAINS

  SUBROUTINE sh_destructor(this)
    TYPE(string_holder), INTENT(INOUT) :: this
    IF (ALLOCATED(this%text)) DEALLOCATE(this%text)
  END SUBROUTINE sh_destructor



  RECURSIVE SUBROUTINE ern_add_item(this, name, item)
    CLASS(eis_namespace), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    CLASS(*), INTENT(IN) :: item
    CLASS(*), POINTER :: gptr
    TYPE(eis_namespace), POINTER :: new, old
    INTEGER :: dotloc

    dotloc = SCAN(name,'.')
    IF (dotloc == 0) THEN
      CALL this%generic_store%store(name, item)
    ELSE IF (dotloc == 1) THEN
      CALL this%generic_store%store(name(2:), item)
    ELSE IF (dotloc == LEN(name)) THEN
      CALL this%generic_store%store(name(1:LEN(name)-1), item)
    ELSE
      gptr => this%namespaces%get(name(1:dotloc-1))
      IF (ASSOCIATED(gptr)) THEN
        SELECT TYPE (co => gptr)
          CLASS IS (eis_namespace)
            old => co
        END SELECT
        CALL old%store(name(dotloc+1:), item)
      ELSE
        ALLOCATE(new)
        CALL new%store(name(dotloc+1:), item)
        gptr => new
        CALL this%namespaces%hold(name(1:dotloc-1), gptr)
      END IF
    END IF

  END SUBROUTINE ern_add_item



  RECURSIVE SUBROUTINE ern_include_namespace(this, namespace, only_final)
    CLASS(eis_namespace), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: namespace
    LOGICAL, INTENT(IN), OPTIONAL :: only_final
    LOGICAL :: final_i
    INTEGER :: dotloc, ind, strend
    TYPE(string_holder) :: sh
    CLASS(*), POINTER :: gptr
    TYPE(eis_namespace), POINTER :: old

    final_i = .TRUE.
    IF (PRESENT(only_final)) final_i = only_final
    IF (final_i) THEN
      ALLOCATE(sh%text, SOURCE = namespace)
      ind = this%included_namespaces%store(sh)
    ELSE
      dotloc = LEN(namespace) + 1
      DO WHILE (dotloc > 0)
        ALLOCATE(sh%text, SOURCE = namespace(1:dotloc-1))
        ind = this%included_namespaces%store(sh)
        DEALLOCATE(sh%text)
        dotloc = SCAN(namespace(1:dotloc-1), '.', .TRUE.)
      END DO
    END IF

  END SUBROUTINE ern_include_namespace



  RECURSIVE FUNCTION ern_get_item(this, name) RESULT(item)
    CLASS(eis_namespace), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER :: dotloc, ins_count, c_ins
    CLASS(*), POINTER :: item, gptr
    CLASS(string_holder), POINTER :: txt
    CLASS(eis_namespace), POINTER :: old

    dotloc = SCAN(name, '.')
    item => NULL()

    IF (dotloc == 0) THEN
      item => this%generic_store%get(name)
      ins_count = this%included_namespaces%get_size()
      c_ins = 1
      DO WHILE (.NOT. ASSOCIATED(item) .AND. c_ins <= ins_count)
        gptr => this%included_namespaces%get(c_ins)
        SELECT TYPE (co => gptr)
          CLASS IS (string_holder)
            txt => co
        END SELECT
        item => this%get(txt%text//'.'//name)
        c_ins = c_ins + 1
      END DO
    ELSE IF (dotloc == LEN(name) .OR. dotloc == 1) THEN
      item => NULL()
    ELSE
      gptr => this%namespaces%get(name(1:dotloc-1))
      IF (.NOT. ASSOCIATED(gptr)) RETURN
      SELECT TYPE (co => gptr)
        CLASS IS (eis_namespace)
          old => co
      END SELECT
      item => old%get(name(dotloc+1:))
    END IF

  END FUNCTION ern_get_item



  SUBROUTINE eir_include_namespace(this, namespace)
    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: namespace

    CALL this%stored_items%include_namespace(namespace)

  END SUBROUTINE eir_include_namespace



  SUBROUTINE eir_add_constant(this, name, value, errcode, can_simplify, &
      cap_bits, defer, err_handler)

    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    REAL(eis_num), INTENT(IN) :: value
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    LOGICAL, INTENT(IN), OPTIONAL :: can_simplify
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits
    LOGICAL, INTENT(IN), OPTIONAL :: defer
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler

    TYPE(eis_function_entry) :: temp
    
    temp%ptype = c_pt_constant
    temp%numerical_data = value
    IF (PRESENT(can_simplify)) temp%can_simplify = can_simplify
    IF (PRESENT(cap_bits)) temp%cap_bits = cap_bits
    IF (PRESENT(defer)) temp%defer = defer

    CALL this%stored_items%store(name, temp)

  END SUBROUTINE eir_add_constant



  SUBROUTINE eir_add_variable(this, name, fn, errcode, can_simplify, &
      cap_bits, defer, err_handler)

    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    PROCEDURE(parser_eval_fn) :: fn
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    LOGICAL, INTENT(IN), OPTIONAL :: can_simplify
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits
    LOGICAL, INTENT(IN), OPTIONAL :: defer
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    TYPE(eis_function_entry) :: temp

    temp%ptype = c_pt_variable
    temp%fn_ptr => fn
    IF (PRESENT(can_simplify)) temp%can_simplify = can_simplify
    IF (PRESENT(cap_bits)) temp%cap_bits = cap_bits
    IF (PRESENT(defer)) temp%defer = defer

    CALL this%stored_items%store(name, temp)

  END SUBROUTINE eir_add_variable



  SUBROUTINE eir_add_function(this, name, fn, expected_parameters, errcode, &
      can_simplify, cap_bits, defer, err_handler)

    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    PROCEDURE(parser_eval_fn) :: fn
    INTEGER, INTENT(IN) :: expected_parameters
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    LOGICAL, OPTIONAL, INTENT(IN) :: can_simplify
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits
    LOGICAL, INTENT(IN), OPTIONAL :: defer
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    TYPE(eis_function_entry) :: temp

    temp%fn_ptr => fn
    temp%ptype = c_pt_function
    temp%expected_parameters = expected_parameters
    IF (PRESENT(can_simplify)) temp%can_simplify = can_simplify
    IF (PRESENT(cap_bits)) temp%cap_bits = cap_bits
    IF (PRESENT(defer)) temp%defer = defer

    CALL this%stored_items%store(name, temp)

  END SUBROUTINE eir_add_function



  SUBROUTINE eir_add_operator(this, name, fn, associativity, precedence, &
      errcode, can_simplify, unary, cap_bits, err_handler)

    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    PROCEDURE(parser_eval_fn) :: fn
    INTEGER, INTENT(IN) :: associativity, precedence
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    LOGICAL, OPTIONAL :: can_simplify
    LOGICAL, OPTIONAL :: unary
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
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
      CALL this%stored_items%store(name, temp)
    ELSE
      temp%expected_parameters = 1
      CALL this%uop_table%store(name, temp)
    END IF

  END SUBROUTINE eir_add_operator



  SUBROUTINE eir_add_stack_var(this, name, stack, errcode, defer, err_handler)
    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    TYPE(eis_stack), INTENT(IN) :: stack
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    LOGICAL, INTENT(IN), OPTIONAL :: defer
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    TYPE(eis_function_entry), TARGET :: temp
    TYPE(eis_function_entry), POINTER :: temp_ptr
    CLASS(*), POINTER :: gptr
    INTEGER(eis_i4) :: index

    gptr => this%stored_items%get(name)
    temp_ptr => NULL()
    IF (ASSOCIATED(gptr)) THEN
      SELECT TYPE(co => gptr)
        CLASS IS (eis_function_entry)
          temp_ptr => co
      END SELECT
    END IF

    IF (ASSOCIATED(temp_ptr)) THEN
      index = this%stack_variable_registry%store(stack, index = temp_ptr%value)
    ELSE
      temp_ptr => temp
      index = this%stack_variable_registry%store(stack)
    END IF

    temp_ptr%ptype = c_pt_stored_variable
    temp_ptr%value = index
    temp_ptr%cap_bits = stack%cap_bits
    temp_ptr%defer = .FALSE.
    IF (PRESENT(defer)) temp_ptr%defer = defer

    CALL this%stored_items%store(name, temp_ptr)

  END SUBROUTINE eir_add_stack_var



  SUBROUTINE eir_add_emplaced_function(this, name, def_fn, errcode, &
      expected_parameters, err_handler)
    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    PROCEDURE(parser_late_bind_fn) :: def_fn
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER, INTENT(IN), OPTIONAL :: expected_parameters
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    TYPE(eis_function_entry), TARGET :: temp
    TYPE(eis_function_entry), POINTER :: temp_ptr
    CLASS(*), POINTER :: gptr
    TYPE(late_bind_fn_holder) :: holder
    INTEGER(eis_i4) :: index

    holder%contents => def_fn

    gptr => this%stored_items%get(name)
    temp_ptr => NULL()
    IF (ASSOCIATED(gptr)) THEN
      SELECT TYPE(co => gptr)
        CLASS IS (eis_function_entry)
          temp_ptr => co
      END SELECT
    END IF

    IF (ASSOCIATED(temp_ptr)) THEN
      index = this%stack_function_registry%store(holder, index = temp_ptr%value)
    ELSE
      temp_ptr => temp
      index = this%stack_function_registry%store(holder)
    END IF

    IF (PRESENT(expected_parameters)) temp_ptr%expected_parameters &
        = expected_parameters
    temp_ptr%ptype = c_pt_emplaced_function
    temp_ptr%value = index

    CALL this%stored_items%store(name, temp)

  END SUBROUTINE eir_add_emplaced_function



  SUBROUTINE eir_fill_block(this, name, block_in, coblock_in, unary_ops, &
      cap_bits, err_handler)

    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    TYPE(eis_stack_element), INTENT(INOUT) :: block_in
    TYPE(eis_stack_co_element), INTENT(INOUT) :: coblock_in
    LOGICAL, INTENT(IN) :: unary_ops
    INTEGER(eis_bitmask), INTENT(OUT) :: cap_bits
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    CLASS(*), POINTER :: gptr
    TYPE(eis_function_entry), POINTER :: temp

    temp => NULL()
    gptr => NULL()
    cap_bits = 0_eis_bitmask
    IF (unary_ops) THEN
      gptr => this%uop_table%get(name)
    END IF
    IF (.NOT. ASSOCIATED(gptr)) gptr => this%stored_items%get(name)

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
      coblock_in%defer = temp%defer
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



  SUBROUTINE eir_copy_in(this, index, output, err_handler, insert_point)
    CLASS(eis_registry) :: this
    INTEGER(eis_i4), INTENT(IN) :: index
    TYPE(eis_stack), INTENT(INOUT) :: output
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    INTEGER, INTENT(IN), OPTIONAL :: insert_point
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
        IF (PRESENT(insert_point)) THEN
          CALL replace_element(output, temp, insert_point)
        ELSE
          CALL append_stack(output, temp)
        END IF
      END IF
    END IF

  END SUBROUTINE eir_copy_in



  SUBROUTINE eir_get_stored(this, index, fn, err_handler)
    CLASS(eis_registry) :: this
    INTEGER(eis_i4), INTENT(IN) :: index
    PROCEDURE(parser_late_bind_fn), POINTER, INTENT(OUT) :: fn
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    CLASS(*), POINTER :: gptr

    gptr => this%stack_function_registry%get(index)
    IF (ASSOCIATED(gptr)) THEN
      SELECT TYPE(co => gptr)
        CLASS IS (late_bind_fn_holder)
          fn => co%contents
        CLASS DEFAULT
          fn => NULL()
      END SELECT
    END IF

  END SUBROUTINE eir_get_stored

END MODULE eis_registry_mod
