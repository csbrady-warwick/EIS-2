MODULE eis_registry_mod

  USE eis_error_mod
  USE eis_header
  USE eis_parser_header
  USE eis_named_store_mod
  USE eis_ordered_store_mod
  USE eis_stack_mod

  IMPLICIT NONE

  INTEGER, PARAMETER :: eis_reg_index_none     = 0
  INTEGER, PARAMETER :: eis_reg_index_stack    = 1
  INTEGER, PARAMETER :: eis_reg_index_emplaced = 2

  !>@class
  !> Type holding pointer to the two kinds of functions used for emplaced
  !> variables and functions
  TYPE :: late_bind_fn_holder
    PROCEDURE(parser_late_bind_interop_fn), POINTER, NOPASS :: eis_contents &
       => NULL()
    PROCEDURE(parser_late_bind_fn), POINTER, NOPASS :: contents => NULL()
  END TYPE late_bind_fn_holder

  !>@class
  !> Type holding a string
  TYPE :: string_holder
    CHARACTER(LEN=:), ALLOCATABLE :: text
    CONTAINS
    FINAL :: sh_destructor
  END TYPE string_holder

  !>@class
  !>Type representing a namespace. Holds the list of all namespaces within
  !> a given namespace and the list of all functions, constants and variables
  !> stored in a given namespace
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

  !>@class
  !> Type holding the information needed to construct an instance of a 
  !> function, constant or variable
  TYPE :: eis_function_entry
    PROCEDURE(parser_eval_fn), POINTER, NOPASS :: fn_ptr => NULL()
    LOGICAL :: can_simplify = .TRUE.
    INTEGER :: index_type = eis_reg_index_none
    INTEGER(eis_i4) :: value = 0_eis_i4
    REAL(eis_num) :: numerical_data = 0.0_eis_num
    INTEGER :: ptype = eis_pt_null
    INTEGER :: associativity = eis_assoc_null
    INTEGER :: precedence = 0
    INTEGER :: expected_parameters = 0
    INTEGER(eis_bitmask) :: cap_bits = 0_eis_bitmask
    LOGICAL :: defer = .FALSE.
  END TYPE eis_function_entry

  !> Registry for all valid functions, constants and variables for a given
  !> parser. Stores all variables except unary operators in a namespace object
  TYPE :: eis_registry
    PRIVATE
    TYPE(eis_namespace) :: stored_items
    TYPE(named_store) :: uop_table !< Contains named unary operators
    TYPE(ordered_store) :: stack_variable_registry
    TYPE(ordered_store) :: emplaced_registry !< Contains deferred stacks

    CONTAINS

    PROCEDURE :: add_emplaced_func_holder => eir_add_emplaced_func_holder

    PROCEDURE, PUBLIC :: include_namespace => eir_include_namespace    
    PROCEDURE, PUBLIC :: add_constant => eir_add_constant
    PROCEDURE, PUBLIC :: add_variable => eir_add_variable
    PROCEDURE, PUBLIC :: add_function => eir_add_function
    PROCEDURE, PUBLIC :: add_operator => eir_add_operator
    PROCEDURE, PUBLIC :: add_stack_variable => eir_add_stack_var
    PROCEDURE, PUBLIC :: add_emplaced_function => eir_add_emplaced_function
    PROCEDURE, PUBLIC :: add_emplaced_function_c => eir_add_emplaced_function_c
    PROCEDURE, PUBLIC :: fill_block => eir_fill_block
    PROCEDURE, PUBLIC :: copy_in_stored => eir_copy_in
    PROCEDURE, PUBLIC :: get_stored_emplacement => eir_get_stored
    
  END TYPE eis_registry

  PRIVATE
  PUBLIC :: eis_registry

CONTAINS

  !>Note that the following terms will be used in this file
  !> item - A function, variable, constant, operator etc.
  !> stack - the combined set of items that makes up a parsed expression
  !> capability bits - A user specified bitmask for the 
  !> capabilities of an item. Capability bits are combined
  !> by bitwise OR and the result is stored for the stack
  !> Use for .e.g indicating that a stack can be time varying or space varying

  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Deallocates string held in string holder on destruction
  !> not needed per standard but implemented for completeness
  !> @param[inout] this
  SUBROUTINE sh_destructor(this)
    TYPE(string_holder), INTENT(INOUT) :: this
    IF (ALLOCATED(this%text)) DEALLOCATE(this%text)
  END SUBROUTINE sh_destructor


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add an item to a namespace
  !> @details
  !> This function takes an unlimited polymorphic pointer
  !> and a name to store it under. If the name contains a dot
  !> then it separates the name on the dot recursively creating
  !> new levels of namespace as it goes until it can finally
  !> store the object
  !> @param[inout] this
  !> @param[in] name
  !> @param[in] item
  RECURSIVE SUBROUTINE ern_add_item(this, name, item)
    CLASS(eis_namespace), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name !< Name to store item under
    CLASS(*), INTENT(IN) :: item !< Item to store
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


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Include a namespace in the list of namespaces to search
  !> @details
  !> Specifies that a given namespace should be tested automatically
  !> when an item is looked up by name. This has the effect of putting the
  !> item in the global namespace. The lookup order is currently
  !> 1) the global namespace
  !> 2) included namespaces in the order they were included in
  !> this behaviour is not guaranteed to be preserved in future releases.
  !> Note that namspaces are not recursively included by default
  !> @param[inout] this
  !> @param[in] namespace
  !> @param[in] only final
  RECURSIVE SUBROUTINE ern_include_namespace(this, namespace, only_final)
    CLASS(eis_namespace), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: namespace !< Namespace to include
    LOGICAL, INTENT(IN), OPTIONAL :: only_final !< Reserved for future use
    LOGICAL :: final_i
    INTEGER :: dotloc, ind
    TYPE(string_holder) :: sh

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


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get an item from a namespace
  !> @details
  !> Items are looked up using the specified name. If the name includes dots
  !> then the string is split on the dots and is used to specify a namespace to
  !> search in. If no dots are present then the global namespace and any
  !> included namespaces are considered
  !>The lookup order is currently
  !> 1) the global namespace
  !> 2) included namespaces in the order they were included in
  !> this behaviour is not guaranteed to be preserved in future releases.
  !> Note that namspaces are not recursively included by default
  !> @param[inout] this
  !> @param[in] name
  !> @return item
  RECURSIVE FUNCTION ern_get_item(this, name) RESULT(item)
    CLASS(eis_namespace), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name !< Name of item to look up
    CLASS(*), POINTER :: item !< Returned item
    INTEGER :: dotloc, ins_count, c_ins
    CLASS(*), POINTER :: gptr
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


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Tell this registry object's namespace store to include a namespace
  !> @param[inout] this
  !> @param[in] namespace
  SUBROUTINE eir_include_namespace(this, namespace)
    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: namespace !< namespace to include

    CALL this%stored_items%include_namespace(namespace)

  END SUBROUTINE eir_include_namespace


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add a \glos{constant} to the list of known names for this registry
  !> @param[inout] this
  !> @param[in] name
  !> @param[in] value
  !> @param[inout] errcode
  !> @param[in] can_simplify
  !> @param[in] cap_bits
  !> @param[in] defer
  !> @param[inout] err_handler
  SUBROUTINE eir_add_constant(this, name, value, errcode, can_simplify, &
      cap_bits, defer, err_handler)

    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name !< Name to associate with constant
    REAL(eis_num), INTENT(IN) :: value !< Value of constant
    !> Error code for store operation
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Can this constant be simplified out by the simplifier? Almost always
    !> .TRUE. for a constant. Optional, default .TRUE.
    LOGICAL, INTENT(IN), OPTIONAL :: can_simplify
    !> \glos{capbits} (capability bits} for this constant. Optional, default 0
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits
    !> Should the definition of this constant be \glos{defer}red.
    !> Optional, default .FALSE.
    LOGICAL, INTENT(IN), OPTIONAL :: defer
    !> Error handler for reporting errors. Optional, default no error handling
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler

    TYPE(eis_function_entry) :: temp
    
    temp%ptype = eis_pt_constant
    temp%numerical_data = value
    IF (PRESENT(can_simplify)) temp%can_simplify = can_simplify
    IF (PRESENT(cap_bits)) temp%cap_bits = cap_bits
    IF (PRESENT(defer)) temp%defer = defer

    CALL this%stored_items%store(name, temp)

  END SUBROUTINE eir_add_constant



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add a \glos{variable} to the list of known names for this registry
  !> @param[inout] this
  !> @param[in] name
  !> @param[in] fn
  !> @param[inout] errcode
  !> @param[in] can_simplify
  !> @param[in] cap_bits
  !> @param[in] defer
  !> @param[inout] err_handler
  SUBROUTINE eir_add_variable(this, name, fn, errcode, can_simplify, &
      cap_bits, defer, err_handler)

    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name !< Name to associate with constant
    !> Eval function to be called when evaluating
    PROCEDURE(parser_eval_fn) :: fn
    !> Error code for store operation
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Can this variable be simplified out by the simplifier?
    !> Normally only .FALSE. if a variable will change value based on
    !> host code provided host_params
    !> Optional, default .TRUE.
    LOGICAL, INTENT(IN), OPTIONAL :: can_simplify
    !> \glos{capbits} (capability bits} for this constant. Optional, default 0
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits
    !> Should the definition of this constant be \glos{defer}red.
    !> Optional, default .FALSE.
    LOGICAL, INTENT(IN), OPTIONAL :: defer
    !> Error handler for reporting errors. Optional, default no error handling
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    TYPE(eis_function_entry) :: temp

    temp%ptype = eis_pt_variable
    temp%fn_ptr => fn
    IF (PRESENT(can_simplify)) temp%can_simplify = can_simplify
    IF (PRESENT(cap_bits)) temp%cap_bits = cap_bits
    IF (PRESENT(defer)) temp%defer = defer

    CALL this%stored_items%store(name, temp)

  END SUBROUTINE eir_add_variable




  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add a \glos{function} to the list of known names for this registry
  !> @param[inout] this
  !> @param[in] name
  !> @param[in] fn
  !> @param[in] expected_parameters
  !> @param[inout] errcode
  !> @param[in] can_simplify
  !> @param[in] cap_bits
  !> @param[in] defer
  !> @param[inout] err_handler
  SUBROUTINE eir_add_function(this, name, fn, expected_parameters, errcode, &
      can_simplify, cap_bits, defer, err_handler)

    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name !< Name to associate with function
    !> Eval function to be called when evaluating
    PROCEDURE(parser_eval_fn) :: fn
    !> Expected number of parameters for this function. If <0 function will
    !> accept any number of parameters and fn will be called with the number
    !> actually provided (variadic function)
    INTEGER, INTENT(IN) :: expected_parameters
    !> Error code for store operation
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Can this function be simplified out by the simplifier?
    !> Normally only .FALSE. if a function will change behavior based on
    !> host code provided host_params
    !> Optional, default .TRUE.
    LOGICAL, OPTIONAL, INTENT(IN) :: can_simplify
    !> \glos{capbits} (capability bits} for this constant. Optional, default 0
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits
    !> Should the definition of this function be \glos{defer}red.
    !> Optional, default .FALSE.
    LOGICAL, INTENT(IN), OPTIONAL :: defer
    !> Error handler for reporting errors. Optional, default no error handling
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    TYPE(eis_function_entry) :: temp

    temp%fn_ptr => fn
    temp%ptype = eis_pt_function
    temp%expected_parameters = expected_parameters
    IF (PRESENT(can_simplify)) temp%can_simplify = can_simplify
    IF (PRESENT(cap_bits)) temp%cap_bits = cap_bits
    IF (PRESENT(defer)) temp%defer = defer

    CALL this%stored_items%store(name, temp)

  END SUBROUTINE eir_add_function



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add an \glos{operator} to the list of known names for this registry
  !> Currently this is not exposed to the host code
  !> @param[inout] this
  !> @param[in] name
  !> @param[in] fn
  !> @param[in] associativity
  !> @param[in[ precedence
  !> @param[inout] errcode
  !> @param[in] can_simplify
  !> @param[in] unary
  !> @param[in] cap_bits
  !> @param[inout] err_handler
  SUBROUTINE eir_add_operator(this, name, fn, associativity, precedence, &
      errcode, can_simplify, unary, cap_bits, err_handler)

    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name !< Name to associate with operator
    !> Eval function to be called when evaluating
    PROCEDURE(parser_eval_fn) :: fn
    INTEGER, INTENT(IN) :: associativity !<Associativity of operator
    INTEGER, INTENT(IN) :: precedence !< Precedence of operator
    INTEGER(eis_error), INTENT(INOUT) :: errcode !< Error code for store
    !> Can this function be simplified out by the simplifier?
    !> Normally only .FALSE. if a function will change behavior based on
    !> host code provided host_params
    !> Optional, default .TRUE.
    LOGICAL, OPTIONAL :: can_simplify
    !> Is this a unary or binary operator. .TRUE. is unary, .FALSE. is binary
    !> Optional, default = .FALSE. (binary operator)
    LOGICAL, OPTIONAL :: unary
    !> \glos{capbits} (capability bits} for this constant. Optional, default 0
    INTEGER(eis_bitmask), INTENT(IN), OPTIONAL :: cap_bits
    !> Error handler for reporting errors. Optional, default no error handling
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    TYPE(eis_function_entry) :: temp
    LOGICAL :: l_unary

    temp%fn_ptr => fn
    temp%ptype = eis_pt_operator
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



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add a \glos{stack_variable} to the list of known names for this registry
  !> @param[inout] this
  !> @param[in] name
  !> @param[in] stack
  !> @param[inout] errcode
  !> @param[in] defer
  !> @param[inout] err_handler
  SUBROUTINE eir_add_stack_var(this, name, stack, errcode, defer, err_handler)
    CLASS(eis_registry) :: this
    !> Name to associate with stack variable
    CHARACTER(LEN=*), INTENT(IN) :: name
    !> Existing stack to associate with name
    TYPE(eis_stack), INTENT(IN) :: stack
    INTEGER(eis_error), INTENT(INOUT) :: errcode !< Error code for store
    !> Should the definition of this function be \glos{defer}red.
    !> Optional, default .FALSE.
    LOGICAL, INTENT(IN), OPTIONAL :: defer
    !> Error handler for reporting errors. Optional, default no error handling
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    TYPE(eis_function_entry), TARGET :: temp
    TYPE(eis_function_entry), POINTER :: temp_ptr
    CLASS(*), POINTER :: gptr
    INTEGER(eis_i4) :: index

    gptr => this%stored_items%get(name)
    IF (ASSOCIATED(gptr)) THEN
      SELECT TYPE(co => gptr)
        CLASS IS (eis_function_entry)
          temp_ptr => co
        CLASS DEFAULT
          temp_ptr => NULL()
      END SELECT
    ELSE
      temp_ptr => NULL()
    END IF

    IF (ASSOCIATED(temp_ptr)) THEN
      IF (temp_ptr%index_type == eis_reg_index_stack) THEN
        index = this%stack_variable_registry%store(stack, &
            index = temp_ptr%value)
      ELSE
        temp_ptr => temp
        index = this%stack_variable_registry%store(stack)
      END IF
    ELSE
      temp_ptr => temp
      index = this%stack_variable_registry%store(stack)
    END IF

    temp_ptr%ptype = eis_pt_stored_variable
    temp_ptr%index_type = eis_reg_index_stack
    temp_ptr%value = index
    temp_ptr%cap_bits = stack%cap_bits
    temp_ptr%defer = .FALSE.
    IF (PRESENT(defer)) temp_ptr%defer = defer

    CALL this%stored_items%store(name, temp_ptr)

  END SUBROUTINE eir_add_stack_var


  !> @author C.S.Brady@warwick.ac.uk
  !> @internal
  !> @brief
  !> Inner function to deal with both Fortran style and C style
  !> emplaced functions
  !> @param[inout] this
  !> @param[in] name
  !> @param[in] holder
  !> @param[inout] errcode
  !> @param[in] expected_parameters
  !> @param[inout] err_handler
  SUBROUTINE eir_add_emplaced_func_holder(this, name, holder, errcode, &
      expected_parameters, err_handler)
    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name !< Name to store under
    TYPE(late_bind_fn_holder), INTENT(in) :: holder !< Holder type
    INTEGER(eis_error), INTENT(INOUT) :: errcode !< Error code from store
    !> Number of expected parameters. Optional, default -1
    INTEGER, INTENT(IN), OPTIONAL :: expected_parameters
    !> Optional error handler
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
      IF(temp_ptr%index_type == eis_reg_index_emplaced) &
          index = this%emplaced_registry%store(holder, index = temp_ptr%value)
    ELSE
      temp_ptr => temp
      index = this%emplaced_registry%store(holder)
    END IF

    temp_ptr%expected_parameters = -1
    IF (PRESENT(expected_parameters)) temp_ptr%expected_parameters &
        = expected_parameters
    temp_ptr%ptype = eis_pt_emplaced_function
    temp_ptr%index_type = eis_reg_index_emplaced
    temp_ptr%value = index

    CALL this%stored_items%store(name, temp)

  END SUBROUTINE eir_add_emplaced_func_holder


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Routine to add a Fortran style emplaced function or variable
  !> emplaced variables are implemented as zero parameter functions
  !> @param[inout] this
  !> @param[in] name
  !> @param[in] def_fn
  !> @param[inout] errcode
  !> @param[in] expected_parameters
  !> @param[inout] err_handler
  SUBROUTINE eir_add_emplaced_function(this, name, def_fn, errcode, &
      expected_parameters, err_handler)
    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name !< Name associated with emplacement
    PROCEDURE(parser_late_bind_fn) :: def_fn !< Definting function (Fortran)
    INTEGER(eis_error), INTENT(INOUT) :: errcode !< Error code from store
    !> Number of expected parameters for the emplacement. <0 is variadic
    !> function, 0 is used for constant. Optional, default 0
    INTEGER, INTENT(IN), OPTIONAL :: expected_parameters
    !> Error handler object. Optional, default no error reporting
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    TYPE(late_bind_fn_holder) :: holder

    holder%contents => def_fn

    CALL this%add_emplaced_func_holder(name, holder, errcode, &
        expected_parameters, err_handler)

  END SUBROUTINE eir_add_emplaced_function


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Routine to add a C style emplaced function or variable
  !> emplaced variables are implemented as zero parameter functions
  !> @param[inout] this
  !> @param[in] name
  !> @param[in] def_fn
  !> @param[inout] errcode
  !> @param[in] expected_parameters
  !> @param[inout] err_handler
  SUBROUTINE eir_add_emplaced_function_c(this, name, def_fn, errcode, &
      expected_parameters, err_handler)
    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name !< Name associated with emplacement
    PROCEDURE(parser_late_bind_interop_fn) :: def_fn !< Definting function (C)
    INTEGER(eis_error), INTENT(INOUT) :: errcode !< Error code from store
    !> Number of expected parameters for the emplacement. <0 is variadic
    !> function, 0 is used for constant. Optional, default 0
    INTEGER, INTENT(IN), OPTIONAL :: expected_parameters
    !> Error handler object. Optional, default no error reporting
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    TYPE(late_bind_fn_holder) :: holder

    holder%eis_contents => def_fn

    CALL this%add_emplaced_func_holder(name, holder, errcode, &
        expected_parameters, err_handler)

  END SUBROUTINE eir_add_emplaced_function_c


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Routine to fill a parser block used by the Shunting Yard Algorithm
  !> @param[inout] this
  !> @param[in] name
  !> @param[inout] block_in
  !> @param[inout] coblock_in
  !> @param[in] unary_ops
  !> @param[inout] err_handler
  SUBROUTINE eir_fill_block(this, name, block_in, coblock_in, unary_ops, &
      err_handler)

    CLASS(eis_registry) :: this
    !> Name extracted from the parsed string. This will be looked up
    CHARACTER(LEN=*), INTENT(IN) :: name
    !> Core block information to be returned on succesful lookup
    TYPE(eis_stack_element), INTENT(INOUT) :: block_in
    !> Extra block information to be returned on succesful lookup
    TYPE(eis_stack_co_element), INTENT(INOUT) :: coblock_in
    !> Is it possible that "name" could describe a unary operator
    LOGICAL, INTENT(IN) :: unary_ops
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    CLASS(*), POINTER :: gptr
    TYPE(eis_function_entry), POINTER :: temp

    temp => NULL()
    gptr => NULL()
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
      coblock_in%cap_bits = temp%cap_bits
      coblock_in%associativity = temp%associativity
      coblock_in%precedence = temp%precedence
      coblock_in%expected_params = temp%expected_parameters
      coblock_in%defer = temp%defer
      block_in%actual_params = temp%expected_parameters
      block_in%can_simplify = temp%can_simplify
      block_in%eval_fn => temp%fn_ptr
      block_in%numerical_data = temp%numerical_data
      block_in%value = temp%value
    ELSE
      block_in%ptype = eis_pt_bad
    END IF

  END SUBROUTINE eir_fill_block


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Routine to copy in a stack variable to a given point in an existing stack
  !> either append at the end or at the specified insertion point
  !> @param[inout] this
  !> @param[in] index
  !> @param[inout] output
  !> @param[inout] err_handler
  !> @param[in] insert_point
  SUBROUTINE eir_copy_in(this, index, output, err_handler, insert_point)
    CLASS(eis_registry) :: this
    INTEGER(eis_i4), INTENT(IN) :: index !< Index number of stack to retrieve
    TYPE(eis_stack), INTENT(INOUT) :: output !< Stack into which to insert
    !> Error handler, optional default no error handling
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    !> Insertion point. Item at the location is replaced with the content of
    !> the requested stack. Optional. Default is insert at end of stack with
    !> no replacement
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
      output%has_emplaced = output%has_emplaced .OR. temp%has_emplaced
      output%has_deferred = output%has_deferred .OR. temp%has_deferred
    END IF

  END SUBROUTINE eir_copy_in


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Routine to retrieve the stored emplacement function by index
  !> @param[inout] this
  !> @param[in] index
  !> @param[out] fn
  !> @param[out] fn_c
  !> @param[inout] err_handler
  SUBROUTINE eir_get_stored(this, index, fn, fn_c, err_handler)
    CLASS(eis_registry) :: this
    INTEGER(eis_i4), INTENT(IN) :: index !< Index of stored function
    !> Returned Fortran type emplacement function
    PROCEDURE(parser_late_bind_fn), POINTER, INTENT(OUT) :: fn
    !> Returned C type emplacement function
    PROCEDURE(parser_late_bind_interop_fn), POINTER, INTENT(OUT) :: fn_c
    !> Error handler. Optional, default no error handling
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    CLASS(*), POINTER :: gptr

    gptr => this%emplaced_registry%get(index)
    IF (ASSOCIATED(gptr)) THEN
      SELECT TYPE(co => gptr)
        CLASS IS (late_bind_fn_holder)
          fn => co%contents
          fn_c => co%eis_contents
        CLASS DEFAULT
          fn => NULL()
          fn_c => NULL()
      END SELECT
    END IF

  END SUBROUTINE eir_get_stored

END MODULE eis_registry_mod
