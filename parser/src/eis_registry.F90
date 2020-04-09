MODULE eis_registry_mod

  USE eis_error_mod
  USE eis_header
  USE eis_parser_constants
  USE eis_named_store_mod
  USE eis_ordered_store_mod
  USE eis_stack_mod

  IMPLICIT NONE

  INTEGER, PARAMETER :: eis_reg_index_none     = 0
  INTEGER, PARAMETER :: eis_reg_index_stack    = 1
  INTEGER, PARAMETER :: eis_reg_index_emplaced = 2

  TYPE, EXTENDS(eis_functor) :: parser_result_functor
    PROCEDURE(parser_result_function), POINTER, NOPASS :: result_fn
  CONTAINS
    PROCEDURE :: operate => prf_operate
  END TYPE parser_result_functor

  !>@class
  !> Type holding pointer to the two kinds of functions used for emplaced
  !> variables and functions
  TYPE :: late_bind_fn_holder
    PROCEDURE(parser_late_bind_interop_fn), POINTER, NOPASS :: clb_contents &
       => NULL()
    PROCEDURE(parser_late_bind_fn), POINTER, NOPASS :: lb_contents => NULL()
    PROCEDURE(parser_late_bind_stack_fn),POINTER, NOPASS :: lbs_contents &
       => NULL()
    PROCEDURE(parser_late_bind_stack_interop_fn),POINTER, NOPASS :: &
       clbs_contents => NULL()
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
    PROCEDURE, PUBLIC :: is_included => ern_is_included
    PROCEDURE, PUBLIC :: get => ern_get_item
    PROCEDURE, PUBLIC :: get_name_count => ern_get_name_count
    PROCEDURE, PUBLIC :: get_name => ern_get_name
    PROCEDURE :: optimise => ern_optimise
    PROCEDURE :: optimize => ern_optimise
  END TYPE eis_namespace

  !>@class
  !> Type holding the information needed to construct an instance of a 
  !> function, constant or variable
  !> @todo Update this to use derived classes rather than this approach
  TYPE :: eis_function_entry
    PROCEDURE(parser_eval_fn), POINTER, NOPASS :: fn_ptr => NULL()
    CHARACTER(LEN=:), ALLOCATABLE :: description
    LOGICAL :: is_hidden = .FALSE.
    LOGICAL :: can_simplify = .TRUE.
    INTEGER :: index_type = eis_reg_index_none
    INTEGER(eis_i4) :: value = 0_eis_i4
    REAL(eis_num) :: numerical_data = 0.0_eis_num
    INTEGER(INT32), POINTER :: i32data => NULL()
    INTEGER(INT64), POINTER :: i64data => NULL()
    REAL(REAL32), POINTER :: r32data => NULL()
    REAL(REAL64), POINTER :: r64data => NULL()
    CLASS(eis_functor), POINTER :: functor => NULL()
    INTEGER :: ptype = eis_pt_null
    INTEGER :: rtype = eis_pt_null
    INTEGER :: associativity = eis_assoc_null
    INTEGER :: precedence = 0
    INTEGER :: expected_parameters = 0
    INTEGER(eis_bitmask) :: cap_bits = 0_eis_bitmask
    LOGICAL :: defer = .FALSE.
    LOGICAL :: owns_functor = .TRUE.
    LOGICAL :: per_stack_functor = .FALSE.
    LOGICAL :: string_params = .FALSE.
  END TYPE eis_function_entry

  !> Registry for all valid functions, constants and variables for a given
  !> parser. Stores all variables except unary operators in a namespace object
  TYPE :: eis_registry
    PRIVATE
    TYPE(eis_namespace) :: stored_items
    TYPE(named_store) :: uop_table !< Contains named unary operators
    TYPE(ordered_store) :: stack_variable_registry
    TYPE(ordered_store) :: emplaced_registry !< Contains deferred stacks
    TYPE(ordered_store) :: functor_registry !< Contains functors
    TYPE(ordered_store) :: stack_function_registry !< Contains stack functions

    CONTAINS

    PROCEDURE :: add_emplaced_func_holder => eir_add_emplaced_func_holder

    PROCEDURE, PUBLIC :: include_namespace => eir_include_namespace
    PROCEDURE, PUBLIC :: is_included => eir_is_included 
    PROCEDURE, PUBLIC :: add_constant => eir_add_constant
    PROCEDURE, PUBLIC :: add_variable => eir_add_variable
    PROCEDURE, PUBLIC :: add_function => eir_add_function
    PROCEDURE, PUBLIC :: add_functor => eir_add_functor
    PROCEDURE, PUBLIC :: add_functor_pointer => eir_add_functor_pointer
    PROCEDURE, PUBLIC :: add_operator => eir_add_operator
    PROCEDURE, PUBLIC :: add_stack_variable => eir_add_stack_var
    PROCEDURE, PUBLIC :: add_stack_function => eir_add_stack_function
    PROCEDURE, PUBLIC :: get_stack_function => eir_get_stack_function
    PROCEDURE, PUBLIC :: add_emplaced_function => eir_add_emplaced_function
    PROCEDURE, PUBLIC :: add_emplaced_function_c => eir_add_emplaced_function_c
    PROCEDURE, PUBLIC :: fill_block => eir_fill_block
    PROCEDURE, PUBLIC :: copy_in_stored => eir_copy_in
    PROCEDURE, PUBLIC :: get_stored_emplacement => eir_get_stored
    PROCEDURE, PUBLIC :: optimise => eir_optimise
    PROCEDURE, PUBLIC :: optimize => eir_optimise
    PROCEDURE, PUBLIC :: get_name_count => eir_get_name_count
    PROCEDURE, PUBLIC :: get_name => eir_get_name
    PROCEDURE, PUBLIC :: result_function_to_functor_stack => eir_rf_to_fs
    
  END TYPE eis_registry

  PRIVATE
  PUBLIC :: eis_registry

CONTAINS


  FUNCTION prf_operate(this, nparams, params, host_params, status_code, &
      errcode)
    CLASS(parser_result_functor), INTENT(INOUT) :: this
    INTEGER(eis_i4), INTENT(IN) :: nparams
    REAL(eis_num), DIMENSION(nparams), INTENT(IN) :: params
    TYPE(C_PTR), INTENT(IN) :: host_params
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    REAL(eis_num) :: prf_operate
    REAL(eis_num), DIMENSION(1) :: results
    INTEGER :: result_count

    result_count = 1
    CALL this%result_fn(result_count, results, host_params, status_code, &
        errcode)
    IF (result_count /=1) THEN
      errcode = eis_err_stack_params
      prf_operate = 0.0_eis_num
      RETURN
    END IF
    prf_operate = results(1)
  END FUNCTION prf_operate


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
  !> Check if a given namespace is included
  !> @param[inout] this
  !> @param[in] namespace
  !> @return ern_is_included
  RECURSIVE FUNCTION ern_is_included(this, namespace)
    CLASS(eis_namespace), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: namespace !< Namespace to test
    LOGICAL :: ern_is_included !< Is namespace included?
    INTEGER :: dotloc, i
    CLASS(eis_namespace), POINTER :: item
    CLASS(*), POINTER :: gptr

    ern_is_included = .FALSE.

    DO i = 1, this%included_namespaces%get_size()
      gptr => this%included_namespaces%get(i)
      SELECT TYPE (gptr)
        CLASS IS (string_holder)
          IF (gptr%text == namespace) THEN
            ern_is_included = .TRUE.
            RETURN
          END IF
      END SELECT
    END DO

    RETURN

    dotloc = SCAN(namespace, '.')
    item => NULL()

    gptr => this%namespaces%get(namespace)
    IF (ASSOCIATED(gptr)) THEN
      ern_is_included = .TRUE.
      RETURN
    END IF

    IF (dotloc == 0 .OR. dotloc == LEN(namespace)) THEN
      gptr => this%namespaces%get(namespace)
      ern_is_included = ASSOCIATED(gptr)
    ELSE IF (dotloc == 1) THEN
      ern_is_included = .FALSE.
    ELSE
      gptr => this%namespaces%get(namespace(1:dotloc-1))
      IF (.NOT. ASSOCIATED(gptr)) THEN
        ern_is_included = .FALSE.
        RETURN
      END IF
      SELECT TYPE (gptr)
        CLASS IS (eis_namespace)
          ern_is_included = gptr%is_included(namespace(dotloc+1:))
      END SELECT
    END IF

  END FUNCTION ern_is_included



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
  !> Optimise the stores inside a namespace
  !> @param[inout] this
  SUBROUTINE ern_optimise(this)
    CLASS(eis_namespace), INTENT(INOUT) :: this
    CLASS(*), POINTER :: gptr
    INTEGER :: i

    CALL this%generic_store%optimise()
    DO i = 1, this%namespaces%get_name_count()
      gptr => this%namespaces%get(i)
      SELECT TYPE (gptr)
        CLASS IS (eis_namespace)
          CALL gptr%optimise()
      END SELECT
    END DO

  END SUBROUTINE ern_optimise



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get the number of names in this and all contained namespaces
  !> Optionally return the number of names in only this namespace
  !> @param[inout] this
  !> @param[out] only_me_count
  !> @return ern_get_name_count
  RECURSIVE FUNCTION ern_get_name_count(this, only_me_count)
    CLASS(eis_namespace), INTENT(INOUT) :: this
    !> Optional. If provided specifies number of names in just this namespace
    INTEGER, INTENT(OUT), OPTIONAL :: only_me_count
    !> Number of names in this namespace and all sub-namespaces
    INTEGER :: ern_get_name_count
    CLASS(*), POINTER :: gptr
    INTEGER :: i

    ern_get_name_count = this%generic_store%get_name_count()
    IF (PRESENT(only_me_count)) only_me_count = ern_get_name_count

    DO i = 1, this%namespaces%get_name_count()
      gptr => this%namespaces%get(i)
      SELECT TYPE (gptr)
        CLASS IS (eis_namespace)
          ern_get_name_count = ern_get_name_count + gptr%get_name_count()
      END SELECT
    END DO

  END FUNCTION ern_get_name_count



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get a name based on an index from 1->get_name_count
  !> @param[inout] this
  !> @param[in] index
  !> @param[inout] name
  SUBROUTINE ern_get_name(this, index, name, name_prefix)
    CLASS(eis_namespace), INTENT(INOUT) :: this
    !> Index of name to get
    INTEGER, INTENT(IN) :: index
    !> Returned name
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: name
    !> Name prefix
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: name_prefix
    CHARACTER(LEN=:), ALLOCATABLE :: n_name, pname, rname
    CLASS(*), POINTER :: gptr
    INTEGER :: i, iindex

    IF (index <= this%generic_store%get_name_count()) THEN
      CALL this%generic_store%get_name(index, n_name)
      IF (PRESENT(name_prefix)) THEN
        ALLOCATE(name, SOURCE = name_prefix // '.' // n_name)
      ELSE
        ALLOCATE(name, SOURCE = n_name)
      END IF
      DEALLOCATE(n_name)
    ELSE
      iindex = index - this%generic_store%get_name_count()

      DO i = 1, this%namespaces%get_name_count()
        gptr => this%namespaces%get(i)
        CALL this%namespaces%get_name(i, rname)
        IF (PRESENT(name_prefix)) THEN
          ALLOCATE(pname, SOURCE = name_prefix // '.' // rname)
        ELSE
          ALLOCATE(pname, SOURCE = rname)
        END IF
        SELECT TYPE (gptr)
          CLASS IS (eis_namespace)
            IF (iindex <= gptr%get_name_count()) THEN
              CALL gptr%get_name(iindex, name, pname)
              EXIT
            ELSE
              iindex = iindex - gptr%get_name_count()
            END IF
        END SELECT
        DEALLOCATE(pname)
      END DO
      IF (ALLOCATED(pname)) DEALLOCATE(pname)
      IF (ALLOCATED(rname)) DEALLOCATE(rname)
    END IF

  END SUBROUTINE ern_get_name



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
  !> Check if the registry's namespace includes cover a given namespace
  !> @param[inout] this
  !> @param[in] namespace
  !> @return eir_is_included
  FUNCTION eir_is_included(this, namespace)
    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: namespace !< namespace to test
    LOGICAL :: eir_is_included !< Is the namespace included

    IF (INDEX(namespace, '.') == 0) THEN
      eir_is_included = .TRUE.
    ELSE
      eir_is_included = this%stored_items%is_included(namespace)
    END IF

  END FUNCTION eir_is_included



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
  !> @param[in] description
  !> @param[in] hidden
  SUBROUTINE eir_add_constant(this, name, value, errcode, can_simplify, &
      cap_bits, defer, err_handler, description, hidden)

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
    !> Description of this constant
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Should this be hidden from autogenerated docs
    LOGICAL, INTENT(IN), OPTIONAL :: hidden

    TYPE(eis_function_entry) :: temp
    
    temp%ptype = eis_pt_constant
    temp%rtype = eis_pt_constant
    temp%numerical_data = value
    IF (PRESENT(can_simplify)) temp%can_simplify = can_simplify
    IF (PRESENT(cap_bits)) temp%cap_bits = cap_bits
    IF (PRESENT(defer)) temp%defer = defer
    IF (PRESENT(description)) ALLOCATE(temp%description, SOURCE = description)
    IF (PRESENT(hidden)) temp%is_hidden = hidden

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
  !> @param[inout] i32data
  !> @param[inout] i64data
  !> @param[inout] r32data
  !> @param[inout] r64data
  !> @param[in] description
  !> @param[in] hidden
  SUBROUTINE eir_add_variable(this, name, fn, errcode, can_simplify, &
      cap_bits, defer, err_handler, i32data, i64data, r32data, r64data, &
      description, hidden)

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
#ifdef F2008
    !> 32 bit integer point data
    INTEGER(INT32), TARGET, OPTIONAL :: i32data
    !> 64 bit integer point data
    INTEGER(INT64), TARGET, OPTIONAL :: i64data
    !> 32 bit real point data
    REAL(REAL32), TARGET, OPTIONAL :: r32data
    !> 64 bit real point data
    REAL(REAL64), TARGET, OPTIONAL :: r64data
#else
    !> 32 bit integer point data
    INTEGER(INT32), POINTER, OPTIONAL :: i32data
    !> 64 bit integer point data
    INTEGER(INT64), POINTER, OPTIONAL :: i64data
    !> 32 bit real point data
    REAL(REAL32), POINTER, OPTIONAL :: r32data
    !> 64 bit real point data
    REAL(REAL64), POINTER, OPTIONAL :: r64data
#endif
    !> Description of the variable
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Should this be hidden from autogenerated docs
    LOGICAL, INTENT(IN), OPTIONAL :: hidden
    TYPE(eis_function_entry) :: temp

    IF (ANY([PRESENT(i32data), PRESENT(i64data), PRESENT(r32data), &
        PRESENT(r64data)])) THEN
      temp%ptype = eis_pt_variable
      temp%rtype = eis_pt_pointer_variable
      temp%fn_ptr => NULL()

      IF (PRESENT(i32data)) temp%i32data => i32data
      IF (PRESENT(i64data)) temp%i64data => i64data
      IF (PRESENT(r32data)) temp%r32data => r32data
      IF (PRESENT(r64data)) temp%r64data => r64data
    ELSE
      temp%ptype = eis_pt_variable
      temp%rtype = eis_pt_variable
      temp%fn_ptr => fn
    END IF

    IF (PRESENT(description)) ALLOCATE(temp%description, SOURCE = description)
    IF (PRESENT(hidden)) temp%is_hidden = hidden
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
  !> @param[in] description
  !> @param[in] hidden
  !> @param[in] can_have_text_params
  SUBROUTINE eir_add_function(this, name, fn, expected_parameters, errcode, &
      can_simplify, cap_bits, defer, err_handler, description, hidden, &
      can_have_text_params)

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
    !> Description of the variable
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Should this be hidden from autogenerated docs
    LOGICAL, INTENT(IN), OPTIONAL :: hidden
    !> Can this function have text parameters
    LOGICAL, INTENT(IN), OPTIONAL :: can_have_text_params
    TYPE(eis_function_entry) :: temp

    temp%fn_ptr => fn
    temp%ptype = eis_pt_function
    temp%rtype = eis_pt_function
    temp%expected_parameters = expected_parameters
    IF (PRESENT(description)) ALLOCATE(temp%description, SOURCE = description)
    IF (PRESENT(hidden)) temp%is_hidden = hidden
    IF (PRESENT(can_simplify)) temp%can_simplify = can_simplify
    IF (PRESENT(cap_bits)) temp%cap_bits = cap_bits
    IF (PRESENT(defer)) temp%defer = defer
    IF (PRESENT(can_have_text_params)) &
        temp%string_params = can_have_text_params

    CALL this%stored_items%store(name, temp)

  END SUBROUTINE eir_add_function



  !> @brief
  !> Add a \glos{functor} to the list of known names for this registry
  !> @param[inout] this
  !> @param[in] name
  !> @param[in] fn
  !> @param[in] expected_parameters
  !> @param[inout] errcode
  !> @param[in] can_simplify
  !> @param[in] cap_bits
  !> @param[in] defer
  !> @param[in] per_stack
  !> @param[inout] err_handler
  !> @param[in] description
  !> @param[in] hidden
  !> @param[in] can_have_text_params
  SUBROUTINE eir_add_functor(this, name, functor, expected_parameters, &
      errcode, can_simplify, cap_bits, defer, per_stack, err_handler, &
      description, hidden, can_have_text_params)

    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name !< Name to associate with function
    !> Functor object to be used when the name is called
    CLASS(eis_functor), INTENT(IN) :: functor
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
    !> Is this functor one that must be replicated for every stack that uses it
    LOGICAL, INTENT(IN), OPTIONAL :: per_stack
    !> Error handler for reporting errors. Optional, default no error handling
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    !> Description of the variable
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Should this be hidden from autogenerated docs
    LOGICAL, INTENT(IN), OPTIONAL :: hidden
    !> Can this function have text parameters
    LOGICAL, INTENT(IN), OPTIONAL :: can_have_text_params
    TYPE(eis_function_entry) :: temp
    INTEGER :: index
    CLASS(*), POINTER :: ptr

    ALLOCATE(temp%functor, SOURCE = functor)
    ptr => temp%functor
    index = this%functor_registry%hold(ptr, owns = .TRUE.)
    temp%owns_functor = .TRUE.
    IF (PRESENT(per_stack)) THEN
      temp%per_stack_functor = per_stack
    ELSE
      temp%per_stack_functor = .FALSE.
    END IF
    temp%ptype = eis_pt_function
    temp%rtype = eis_pt_functor
    temp%expected_parameters = expected_parameters
    IF (PRESENT(description)) ALLOCATE(temp%description, SOURCE = description)
    IF (PRESENT(hidden)) temp%is_hidden = hidden
    IF (PRESENT(can_simplify)) temp%can_simplify = can_simplify
    IF (PRESENT(cap_bits)) temp%cap_bits = cap_bits
    IF (PRESENT(defer)) temp%defer = defer
    IF (PRESENT(can_have_text_params)) &
        temp%string_params = can_have_text_params

    CALL this%stored_items%store(name, temp)

  END SUBROUTINE eir_add_functor



  !> @brief
  !> Add a \glos{functor} to the list of known names for this registry
  !> @param[inout] this
  !> @param[in] name
  !> @param[in] fn
  !> @param[in] expected_parameters
  !> @param[inout] errcode
  !> @param[in] can_simplify
  !> @param[in] cap_bits
  !> @param[in] defer
  !> @param[inout] err_handler
  !> @param[in] description
  !> @param[in] hidden
  !> @param[in] owns
  !> @param[in] can_have_text_params
  SUBROUTINE eir_add_functor_pointer(this, name, functor, expected_parameters, &
      errcode, can_simplify, cap_bits, defer, err_handler, description, &
      hidden, owns, can_have_text_params)

    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name !< Name to associate with function
    !> Functor object to be used when the name is called
    CLASS(eis_functor), POINTER, INTENT(IN) :: functor
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
    !> Description of the variable
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Should this be hidden from autogenerated docs
    LOGICAL, INTENT(IN), OPTIONAL :: hidden
    !> Does the parser own this functor object under this name?
    LOGICAL, INTENT(IN), OPTIONAL :: owns
    !> Can this function have text parameters
    LOGICAL, INTENT(IN), OPTIONAL :: can_have_text_params
    TYPE(eis_function_entry) :: temp
    INTEGER :: index
    CLASS(*), POINTER :: ptr

    temp%functor => functor
    IF (PRESENT(owns)) THEN
      temp%owns_functor = owns
    ELSE
      temp%owns_functor = .FALSE.
    END IF
    IF (temp%owns_functor) THEN
      ptr => functor
      index = this%functor_registry%hold(ptr, owns = .TRUE.)
    END IF
    temp%ptype = eis_pt_function
    temp%rtype = eis_pt_functor
    temp%expected_parameters = expected_parameters
    IF (PRESENT(description)) ALLOCATE(temp%description, SOURCE = description)
    IF (PRESENT(hidden)) temp%is_hidden = hidden
    IF (PRESENT(can_simplify)) temp%can_simplify = can_simplify
    IF (PRESENT(cap_bits)) temp%cap_bits = cap_bits
    IF (PRESENT(defer)) temp%defer = defer
    IF (PRESENT(can_have_text_params)) &
        temp%string_params = can_have_text_params

    CALL this%stored_items%store(name, temp)

  END SUBROUTINE eir_add_functor_pointer



  !> @brief
  !> Add a stack function to the list of known names for this registry
  !> @param[inout] this
  !> @param[in] name
  !> @param[in] fn
  !> @param[in] expected_parameters
  !> @param[inout] errcode
  !> @param[inout] err_handler
  !> @param[in] description
  !> @param[in] hidden
  SUBROUTINE eir_add_stack_function(this, name, stack, expected_parameters, &
      errcode, err_handler, description, hidden)
    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name !< Name to associate with function
    TYPE(eis_stack), INTENT(IN) :: stack !< Stack defining the function
    INTEGER, INTENT(IN) :: expected_parameters !< Number of expected parameters
    INTEGER(eis_error), INTENT(INOUT) :: errcode !< Error code
    !> Error handler for reporting errors. Optional, default no error handling
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    LOGICAL, INTENT(IN), OPTIONAL :: hidden
    INTEGER :: index
    TYPE(eis_function_entry) :: temp

    index = this%stack_function_registry%store(stack)
    temp%ptype = eis_pt_function
    temp%rtype = eis_pt_stack_function
    temp%expected_parameters = expected_parameters
    IF (PRESENT(description)) ALLOCATE(temp%description, SOURCE = description)
    IF (PRESENT(hidden)) temp%is_hidden = hidden
    temp%can_simplify = .FALSE.
    temp%cap_bits = stack%cap_bits
    temp%value = index

    CALL this%stored_items%store(name, temp)

  END SUBROUTINE eir_add_stack_function



  SUBROUTINE eir_get_stack_function(this, id, stack, errcode)
    CLASS(eis_registry) :: this
    INTEGER, INTENT(IN) :: id
    TYPE(eis_stack), INTENT(INOUT) :: stack
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    CLASS(*), POINTER :: gptr
    TYPE(eis_stack), POINTER :: sptr

    errcode = eis_err_bad_value
    gptr => this%stack_function_registry%get(id)
    IF (.NOT. ASSOCIATED(gptr)) RETURN
    errcode = eis_err_none

    SELECT TYPE(gptr)
      CLASS IS (eis_stack)
        sptr => gptr
    END SELECT

    CALL deallocate_stack(stack)
    CALL initialise_stack(stack)
    stack = sptr

  END SUBROUTINE eir_get_stack_function



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
  !> @param[in] description
  !> @param[in] hidden
  !> @param[in] operator_id
  SUBROUTINE eir_add_operator(this, name, fn, associativity, precedence, &
      errcode, can_simplify, unary, cap_bits, err_handler, description, &
      hidden, operator_id)

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
    !> Description of the variable
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Should this be hidden from autogenerated docs
    LOGICAL, INTENT(IN), OPTIONAL :: hidden
    !> Override the default rtype
    INTEGER, INTENT(IN), OPTIONAL :: operator_id
    TYPE(eis_function_entry) :: temp
    LOGICAL :: l_unary

    temp%fn_ptr => fn
    temp%ptype = eis_pt_operator
    IF (PRESENT(operator_id)) THEN
      temp%rtype = operator_id
    ELSE
      temp%rtype = eis_pt_operator
    END IF
    temp%associativity = associativity
    temp%precedence = precedence
    IF (PRESENT(can_simplify)) temp%can_simplify = can_simplify
    IF (PRESENT(cap_bits)) temp%cap_bits = cap_bits
    IF (PRESENT(description)) ALLOCATE(temp%description, SOURCE = description)
    IF (PRESENT(hidden)) temp%is_hidden = hidden

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
  !> @param[in] description
  !> @param[in] hidden
  SUBROUTINE eir_add_stack_var(this, name, stack, errcode, defer, err_handler, &
      description, hidden)
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
    !> Description of the variable
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Should this be hidden from autogenerated docs
    LOGICAL, INTENT(IN), OPTIONAL :: hidden
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
    temp_ptr%rtype = eis_pt_stored_variable
    temp_ptr%index_type = eis_reg_index_stack
    temp_ptr%value = index
    temp_ptr%cap_bits = stack%cap_bits
    temp_ptr%defer = .FALSE.
    IF (PRESENT(defer)) temp_ptr%defer = defer
    IF (PRESENT(description)) ALLOCATE(temp_ptr%description, &
        SOURCE = description)
    IF (PRESENT(hidden)) temp_ptr%is_hidden = hidden

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
  !> @param[in] description
  !> @param[in] hidden
  SUBROUTINE eir_add_emplaced_func_holder(this, name, holder, errcode, &
      expected_parameters, err_handler, description, hidden)
    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name !< Name to store under
    TYPE(late_bind_fn_holder), INTENT(in) :: holder !< Holder type
    INTEGER(eis_error), INTENT(INOUT) :: errcode !< Error code from store
    !> Number of expected parameters. Optional, default -1
    INTEGER, INTENT(IN), OPTIONAL :: expected_parameters
    !> Optional error handler
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    !> Description of the variable
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Should this be hidden from autogenerated docs
    LOGICAL, INTENT(IN), OPTIONAL :: hidden
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
    IF (PRESENT(description)) ALLOCATE(temp_ptr%description, &
        SOURCE = description)
    IF (PRESENT(hidden)) temp_ptr%is_hidden = hidden
    temp_ptr%ptype = eis_pt_emplaced_function
    temp_ptr%rtype = eis_pt_emplaced_function
    temp_ptr%index_type = eis_reg_index_emplaced
    temp_ptr%value = index
    temp_ptr%can_simplify = .FALSE.

    CALL this%stored_items%store(name, temp)

  END SUBROUTINE eir_add_emplaced_func_holder



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Routine to add a Fortran style emplaced function or variable
  !> emplaced variables are implemented as zero parameter functions
  !> @param[inout] this
  !> @param[in] name
  !> @param[inout] errcode
  !> @param[in] expected_parameters
  !> @param[inout] err_handler
  !> @param[in] description
  !> @param[in] hidden
  !> @param[in] lb_fn
  !> @param[in] clb_fn
  !> @param[in] lbs_fn
  !> @param[in] clbs_fn
  SUBROUTINE eir_add_emplaced_function(this, name, errcode, &
      expected_parameters, err_handler, description, hidden, lb_fn, clb_fn, &
      lbs_fn, clbs_fn)
    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name !< Name associated with emplacement
    PROCEDURE(parser_late_bind_fn) :: def_fn !< Definting function (Fortran)
    INTEGER(eis_error), INTENT(INOUT) :: errcode !< Error code from store
    !> Number of expected parameters for the emplacement. <0 is variadic
    !> function, 0 is used for constant. Optional, default 0
    INTEGER, INTENT(IN), OPTIONAL :: expected_parameters
    !> Error handler object. Optional, default no error reporting
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    !> Description of the variable
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Should this be hidden from autogenerated docs
    LOGICAL, INTENT(IN), OPTIONAL :: hidden
    !> Defining function non-stack (Fortran)
    PROCEDURE(parser_late_bind_fn), OPTIONAL ::lb_fn
    !> Defining function non-stack (C)
    PROCEDURE(parser_late_bind_interop_fn), OPTIONAL ::clb_fn
    !> Defining function stack (Fortran)
    PROCEDURE(parser_late_bind_stack_fn), OPTIONAL ::lbs_fn
    !> Defining function stack (C)
    PROCEDURE(parser_late_bind_stack_interop_fn), OPTIONAL ::clbs_fn

    TYPE(late_bind_fn_holder) :: holder

    IF (PRESENT(lb_fn)) holder%lb_contents => lb_fn
    IF (PRESENT(clb_fn)) holder%clb_contents => clb_fn
    IF (PRESENT(lbs_fn)) holder%lbs_contents => lbs_fn
    IF (PRESENT(clbs_fn)) holder%clbs_contents => clbs_fn

    CALL this%add_emplaced_func_holder(name, holder, errcode, &
        expected_parameters, err_handler, description, hidden)

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
  !> @param[in] description
  !> @param[in] hidden
  SUBROUTINE eir_add_emplaced_function_c(this, name, def_fn, errcode, &
      expected_parameters, err_handler, description, hidden)
    CLASS(eis_registry) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name !< Name associated with emplacement
    PROCEDURE(parser_late_bind_interop_fn) :: def_fn !< Definting function (C)
    INTEGER(eis_error), INTENT(INOUT) :: errcode !< Error code from store
    !> Number of expected parameters for the emplacement. <0 is variadic
    !> function, 0 is used for constant. Optional, default 0
    INTEGER, INTENT(IN), OPTIONAL :: expected_parameters
    !> Error handler object. Optional, default no error reporting
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    !> Description of the variable
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: description
    !> Should this be hidden from autogenerated docs
    LOGICAL, INTENT(IN), OPTIONAL :: hidden
    TYPE(late_bind_fn_holder) :: holder

    holder%clb_contents => def_fn

    CALL this%add_emplaced_func_holder(name, holder, errcode, &
        expected_parameters, err_handler, description, hidden)

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
  !> @param[out] description
  !> @param[out] hidden
  SUBROUTINE eir_fill_block(this, name, block_in, coblock_in, unary_ops, &
      err_handler, description, hidden)

    CLASS(eis_registry) :: this
    !> Name extracted from the parsed string. This will be looked up
    CHARACTER(LEN=*), INTENT(IN) :: name
    !> Core block information to be returned on succesful lookup
    TYPE(eis_stack_element), INTENT(INOUT) :: block_in
    !> Extra block information to be returned on succesful lookup
    TYPE(eis_stack_co_element), INTENT(INOUT) :: coblock_in
    !> Is it possible that "name" could describe a unary operator
    LOGICAL, INTENT(IN) :: unary_ops
    !> Error handler
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    !> Description of a block if requested
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT), OPTIONAL :: description
    !> Should this be hidden from autogenerated docs
    LOGICAL, INTENT(OUT), OPTIONAL :: hidden
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
      block_in%rtype = temp%rtype
      coblock_in%cap_bits = temp%cap_bits
      coblock_in%associativity = temp%associativity
      coblock_in%precedence = temp%precedence
      coblock_in%expected_params = temp%expected_parameters
      coblock_in%defer = temp%defer
      coblock_in%can_have_string_params = temp%string_params
      block_in%actual_params = temp%expected_parameters
      block_in%can_simplify = temp%can_simplify
      block_in%eval_fn => temp%fn_ptr
      block_in%numerical_data = temp%numerical_data
      block_in%value = temp%value
      block_in%i32data => temp%i32data
      block_in%i64data => temp%i64data
      block_in%r32data => temp%r32data
      block_in%r64data => temp%r64data
      block_in%functor => temp%functor
      IF (PRESENT(description) .AND. ALLOCATED(temp%description)) THEN
        ALLOCATE(description, SOURCE = temp%description)
      END IF
      IF (PRESENT(hidden)) hidden = temp%is_hidden
    ELSE
      block_in%ptype = eis_pt_bad
      block_in%rtype = eis_pt_bad
    END IF

  END SUBROUTINE eir_fill_block


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Routine to copy in a stack variable to a given point in an existing stack
  !> either append at the end or at the specified insertion point
  !> @param[inout] this
  !> @param[in] index
  !> @param[inout] output
  !> @param[inout] errcode
  !> @param[in] insert_point
  !> @param[out] item_count
  !> @param[in] allow_multiple
  SUBROUTINE eir_copy_in(this, index, output, errcode, insert_point, &
      item_count, allow_multiple)
    CLASS(eis_registry) :: this
    INTEGER(eis_i4), INTENT(IN) :: index !< Index number of stack to retrieve
    TYPE(eis_stack), INTENT(INOUT) :: output !< Stack into which to insert
    !> Error code for this operation
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    !> Insertion point. Item at the location is replaced with the content of
    !> the requested stack. Optional. Default is insert at end of stack with
    !> no replacement
    INTEGER, INTENT(IN), OPTIONAL :: insert_point
    !> Number of items to be inserted. This is not the same as the number 
    !> of entries in the stack but is the number of items that the stack
    !> evaluates to
    INTEGER, INTENT(OUT), OPTIONAL :: item_count
    !> Should an error be returned if you are filling with a stack
    !> that unpacks to multiple items
    LOGICAL, INTENT(IN), OPTIONAL :: allow_multiple
    CLASS(*), POINTER :: gptr
    TYPE(eis_stack), POINTER :: temp
    TYPE(eis_stack), TARGET :: filled
    INTEGER(eis_error) :: err

    errcode = eis_err_none

    temp => NULL()
    gptr => this%stack_variable_registry%get(index)
    IF (ASSOCIATED(gptr)) THEN
      SELECT TYPE(co => gptr)
        CLASS IS (eis_stack)
          temp => co
      END SELECT
      IF (ASSOCIATED(temp)) THEN
        IF (ASSOCIATED(temp%eval_fn)) THEN
          CALL this%result_function_to_functor_stack(temp, err, &
              output_stack = filled)
          IF (err /= eis_err_none) RETURN
          temp => filled
        END IF
      END IF
      IF (PRESENT(allow_multiple)) THEN
        IF (.NOT. allow_multiple .AND. temp%params > 1) THEN
          errcode = eis_err_stack_params
          RETURN
        END IF
      END IF
      IF (PRESENT(item_count)) item_count = temp%params
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
  !> Routine to take a stack that has a result function bound to it
  !> and then convert it to a functor that calls the same function
  !> @param[inout] this
  !> @param[in] stack_in
  !> @param[inout] errcode
  !> @param[inout] stack_out
  !> @param[in] append
  SUBROUTINE eir_rf_to_fs(this, stack_in, errcode, output_stack, append)
    CLASS(eis_registry) :: this
    !> Stack containing
    TYPE(eis_stack), INTENT(INOUT), TARGET :: stack_in
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    TYPE(eis_stack), INTENT(OUT), OPTIONAL, TARGET :: output_stack
    LOGICAL, INTENT(IN), OPTIONAL :: append
    INTEGER :: i, dummy
    CLASS(*), POINTER :: gptr
    CLASS(parser_result_functor), POINTER :: eval_functor
    TYPE(eis_stack_element) :: iblock
    TYPE(eis_stack_co_element) :: icoblock
    TYPE(eis_stack), POINTER :: res_stack
    LOGICAL :: should_append

    should_append = .FALSE.
    IF (PRESENT(append)) should_append = append

    errcode = eis_err_none

    !Shouldn't happen but fall back gracefully
    IF (.NOT. ASSOCIATED(stack_in%eval_fn)) THEN
      IF (PRESENT(output_stack)) output_stack = stack_in
      RETURN
    END IF

    IF (PRESENT(output_stack)) THEN
      res_stack => output_stack
    ELSE
      res_stack => stack_in
    END IF

    eval_functor => NULL()
    DO i = 1, this%functor_registry%get_size()
      gptr => this%functor_registry%get(i)
      SELECT TYPE (gptr)
        TYPE IS (parser_result_functor)
          IF (ASSOCIATED(gptr%result_fn, TARGET = stack_in%eval_fn)) THEN
            eval_functor => gptr
            EXIT
          END IF
      END SELECT
    END DO

    IF (.NOT. ASSOCIATED(eval_functor)) THEN
      ALLOCATE(eval_functor)
      eval_functor%result_fn => stack_in%eval_fn
      gptr => eval_functor
      dummy = this%functor_registry%hold(gptr, owns = .TRUE.)
    END IF
    iblock%ptype = eis_pt_function
    iblock%rtype = eis_pt_functor
    iblock%functor => eval_functor
    iblock%actual_params = 0
    iblock%can_simplify = .FALSE.
    ALLOCATE(icoblock%text, SOURCE = stack_in%full_line)

    IF (.NOT. should_append) THEN
      CALL initialise_stack(res_stack)
    ELSE
      res_stack%eval_fn => NULL()
    END IF
    CALL push_to_stack(res_stack, iblock, icoblock)

  END SUBROUTINE eir_rf_to_fs



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Routine to retrieve the stored emplacement function by index
  !> @param[inout] this
  !> @param[in] index
  !> @param[out] fn
  !> @param[out] fn_c
  !> @param[out] fn_s
  !> @param[out] fn_s_v
  !> @param[inout] err_handler
  SUBROUTINE eir_get_stored(this, index, fn, fn_c, fn_s, fn_s_c, err_handler)
    CLASS(eis_registry) :: this
    INTEGER(eis_i4), INTENT(IN) :: index !< Index of stored function
    !> Returned Fortran type emplacement function
    PROCEDURE(parser_late_bind_fn), POINTER, INTENT(OUT) :: fn
    !> Returned C type emplacement function
    PROCEDURE(parser_late_bind_interop_fn), POINTER, INTENT(OUT) :: fn_c
    !> Returned Fortran type stack emplacement function
    PROCEDURE(parser_late_bind_stack_fn), POINTER, INTENT(OUT) :: fn_s
    !> Returned C type stack emplacement function
    PROCEDURE(parser_late_bind_stack_interop_fn), POINTER, INTENT(OUT) :: fn_s_c
    !> Error handler. Optional, default no error handling
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    CLASS(*), POINTER :: gptr

    gptr => this%emplaced_registry%get(index)
    IF (ASSOCIATED(gptr)) THEN
      SELECT TYPE(co => gptr)
        CLASS IS (late_bind_fn_holder)
          fn => co%lb_contents
          fn_c => co%clb_contents
          fn_s => co%lbs_contents
          fn_s_c => co%clbs_contents
        CLASS DEFAULT
          fn => NULL()
          fn_c => NULL()
          fn_s => NULL()
          fn_s_c => NULL()
      END SELECT
    END IF

  END SUBROUTINE eir_get_stored




  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Optimise the stores in this registry
  !> @param[inout] this
  SUBROUTINE eir_optimise(this)
    CLASS(eis_registry) :: this

    CALL this%uop_table%optimise()
    CALL this%stored_items%optimise()

  END SUBROUTINE eir_optimise



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get the number of names in this registry
  !> @param[inout] this
  !> @return eir_get_name_count
  FUNCTION eir_get_name_count(this)
    CLASS(eis_registry), INTENT(INOUT) :: this
    !> Number of names in this registry
    INTEGER :: eir_get_name_count

    eir_get_name_count = 0
    eir_get_name_count = eir_get_name_count + this%stored_items%get_name_count()

  END FUNCTION eir_get_name_count



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get a name specifying an index from 1 -> get_name_count
  !> @param[inout] this  
  SUBROUTINE eir_get_name(this, index, name)
    CLASS(eis_registry), INTENT(INOUT) :: this
    !> Integer specifing which name to get
    INTEGER, INTENT(IN) :: index
    !> String containing retreived name
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: name

    CALL this%stored_items%get_name(index, name)

  END SUBROUTINE eir_get_name

END MODULE eis_registry_mod
