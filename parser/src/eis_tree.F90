MODULE eis_tree_mod

  USE, INTRINSIC :: ISO_C_BINDING
  USE eis_error_mod
  USE eis_eval_stack_mod
  USE eis_header
  USE eis_stack_mod
  USE eis_utils
  USE eis_registry_mod
  IMPLICIT NONE

  !> @class
  !> Type representing a parsed string stored as an AST rather than as a stack
  TYPE :: eis_tree_item
    TYPE(eis_stack_element) :: value
    TYPE(eis_stack_co_element) :: co_value
    TYPE(eis_tree_item), DIMENSION(:), POINTER :: nodes => NULL()
    CONTAINS
    FINAL :: eit_destructor
  END TYPE eis_tree_item

  TYPE(eis_eval_stack), SAVE :: eval

  CONTAINS

  !> @brief
  !> Deallocate a tree. Pure elemental routine
  !> works on both single items and arrays
  !> @param[inout] this - Tree to deallocate
  PURE ELEMENTAL SUBROUTINE eit_destructor(this)
    TYPE(eis_tree_item), INTENT(INOUT) :: this

    IF (ASSOCIATED(this%nodes)) DEALLOCATE(this%nodes)
    CALL deallocate_stack_element(this%value)
    CALL deallocate_stack_co_element(this%co_value)

  END SUBROUTINE eit_destructor



  !> @brief
  !> Simplify a stack. Does this by converting to an AST and then recursively
  !> coverting nodes to simple constants if all nodes beneath them are marked
  !> as simplifyable
  !> @param[inout] stack - Stack to simplify. If "stack_out" is not specified
  !> then the simplified stack is stored back into "stack". If stack_out is
  !> specified then "stack" is not changed
  !> @param[in] params - Host code specified custom parameters. These are passed
  !> to each function in the stack so that they can mark themselves
  !> if they are conditionally simplifyable based on parameters. May be 
  !> C_PTR_NULL
  !> @param[inout] errcode - Error code returned by simplify operation
  !> @param[inout] err_handler - Error handler object. Optional, default no
  !> error reporting
  !> @param[out] stack_out - Stack to hold result of simplified stack. Optional,
  !> default is store simplified stack in "stack" variable
  SUBROUTINE eis_simplify_stack(stack, params, errcode, err_handler, stack_out)

    TYPE(eis_stack), INTENT(INOUT) :: stack
    TYPE(C_PTR), INTENT(IN) :: params
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    TYPE(eis_stack), INTENT(OUT), OPTIONAL, TARGET :: stack_out
    TYPE(eis_stack), POINTER :: simplified
    TYPE(eis_stack) :: intermediate
    TYPE(eis_tree_item), POINTER :: root
    INTEGER :: sp
    LOGICAL :: dummy

    IF (.NOT. stack%init) RETURN
    IF (.NOT. PRESENT(stack_out)) THEN
      ALLOCATE(simplified)
    ELSE
      simplified => stack_out
    END IF

    IF (.NOT. simplified%init) CALL initialise_stack(simplified)
    sp = stack%stack_point + 1
    DO WHILE (sp > 1)
      ALLOCATE(root)
      CALL eis_build_node(stack, sp, root)
      errcode = eis_err_none
      IF (ALLOCATED(stack%full_line)) THEN
        dummy = eis_simplify_tree(root, params, errcode, err_handler, &
            stack%filename, stack%line_number, stack%full_line)
      ELSE
        dummy = eis_simplify_tree(root, params, errcode, err_handler)
      END IF
      IF (errcode /= eis_err_none) THEN
        DEALLOCATE(root)
        IF (.NOT. PRESENT(stack_out)) DEALLOCATE(simplified)
        RETURN
      END IF
      CALL initialise_stack(intermediate)
      CALL eis_tree_to_stack(root, intermediate)
      CALL prepend_stack(simplified, intermediate)
      CALL deallocate_stack(intermediate)
      DEALLOCATE(root)
    END DO

    simplified%cap_bits = stack%cap_bits
    simplified%has_deferred = stack%has_deferred
    simplified%has_emplaced = stack%has_emplaced
    simplified%can_accept_maths_domain_errors &
        = stack%can_accept_maths_domain_errors
    IF (ALLOCATED(stack%full_line)) THEN
      ALLOCATE(simplified%full_line, SOURCE = stack%full_line)
    ELSE
      ALLOCATE(simplified%full_line, SOURCE = "")
    END IF
    simplified%params = stack%params
    IF (ALLOCATED(stack%filename)) THEN
      ALLOCATE(simplified%filename, SOURCE = stack%filename)
    ELSE
      ALLOCATE(simplified%filename, SOURCE = "")
    END IF

    simplified%line_number = stack%line_number

    IF (.NOT. PRESENT(stack_out)) THEN
      CALL deallocate_stack(stack)
      CALL copy_stack(simplified, stack)
      CALL deallocate_stack(simplified)
      DEALLOCATE(simplified)
    END IF

  END SUBROUTINE eis_simplify_stack



  !> @brief
  !> Convert a stack to an AST recursively
  !> @param[in] stack - stack that is being converted
  !> @param[inout] curindex - stack position that is being considered
  !> @param[inout] current - Pointer to the current tree node that is being 
  !>built
  RECURSIVE SUBROUTINE eis_build_node(stack, curindex, current)
    TYPE(eis_stack), INTENT(IN) :: stack
    INTEGER, INTENT(INOUT) :: curindex
    TYPE(eis_tree_item), POINTER, INTENT(INOUT) :: current
    TYPE(eis_tree_item), POINTER :: p
    INTEGER :: iparam, pcount

    curindex = curindex - 1
    current%value = stack%entries(curindex)
    IF (ALLOCATED(stack%co_entries)) &
        current%co_value = stack%co_entries(curindex)

    IF (current%value%ptype == eis_pt_function .OR. &
        current%value%ptype == eis_pt_operator .OR. &
        current%value%ptype == eis_pt_emplaced_function) THEN
      pcount = current%value%actual_params
      ALLOCATE(current%nodes(pcount))
      DO iparam = 1, pcount
        p => current%nodes(iparam)
        CALL eis_build_node(stack, curindex, p)
      END DO
    END IF
  END SUBROUTINE eis_build_node



  !> @brief
  !> Simplify a tree given the head node. Does this by recursively
  !> coverting nodes to simple constants if all nodes beneath them are marked
  !> as simplifyable
  !> @param[inout] tree - Tree to simplify. Tree is always simplified "inline"
  !> and is changed to reflect the simplification
  !> @param[in] params - Host code specified custom parameters. These are passed
  !> to each function in the tree so that they can mark themselves
  !> if they are conditionally simplifyable based on parameters. May be 
  !> C_PTR_NULL
  !> @param[inout] errcode - Error code returned by simplify operation
  !> @param[inout] err_handler - Error handler object. Optional, default no
  !> error reporting
  !> @return can_simplify - Whether simplification is possible when this node
  !> is considered
  RECURSIVE FUNCTION eis_simplify_tree(tree, params, errcode, err_handler, &
      filename, line_number, full_line) RESULT(can_simplify)
    TYPE(eis_tree_item), INTENT(INOUT) :: tree
    TYPE(C_PTR), INTENT(IN) :: params
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: filename
    INTEGER, INTENT(IN), OPTIONAL :: line_number
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: full_line
    INTEGER :: inode
    LOGICAL :: can_simplify, has_nodes, simp1
    REAL(eis_num) :: res
    TYPE(eis_eval_stack) :: eval
    INTEGER(eis_error) :: err
    INTEGER(eis_status) :: status
    CHARACTER(LEN=30) :: rstring
    TYPE(eis_tree_item), DIMENSION(:), POINTER :: tdata

    status = eis_status_none

    has_nodes = ASSOCIATED(tree%nodes)
    IF (has_nodes) has_nodes = has_nodes .AND. (SIZE(tree%nodes) > 0)
    can_simplify = tree%value%can_simplify

    IF (has_nodes) THEN
      DO inode = SIZE(tree%nodes), 1, -1
        simp1 = eis_simplify_tree(tree%nodes(inode), params, errcode, &
            err_handler, filename, line_number, full_line)
        can_simplify = can_simplify .AND. simp1
      END DO
    END IF

    IF (tree%value%ptype == eis_pt_operator) THEN
      IF (tree%value%rtype == eis_pt_op_multiply) THEN
        !Simplify multiply by zero
        IF (tree%nodes(1)%value%rtype == eis_pt_zero &
            .OR. tree%nodes(2)%value%rtype == eis_pt_zero) THEN
          tree%value%ptype = eis_pt_constant
          tree%value%rtype = eis_pt_zero
          tree%value%numerical_data = 0.0_eis_num
          tree%co_value%deriv_values = -1
          IF (ALLOCATED(tree%co_value%text)) THEN
            DEALLOCATE(tree%co_value%text)
            ALLOCATE(tree%co_value%text, SOURCE = "0")
          END IF
          DEALLOCATE(tree%nodes)
          can_simplify = .TRUE.
          RETURN
       !Simplify multiply by 1 on right
        ELSE IF (tree%nodes(1)%value%rtype == eis_pt_unity) THEN
          tdata => tree%nodes
          tree%nodes => NULL()
          CALL eis_copy_tree(tdata(2), tree)
          DEALLOCATE(tdata)
          RETURN
        !Simplify multiply by 1 of the left
        ELSE IF (tree%nodes(2)%value%rtype == eis_pt_unity) THEN
          tdata => tree%nodes
          tree%nodes => NULL()
          CALL eis_copy_tree(tdata(1), tree)
          DEALLOCATE(tdata)
          RETURN
        END IF

      ELSE IF (tree%value%rtype == eis_pt_op_divide) THEN
        !Simplify divide by 1
        IF (tree%nodes(1)%value%rtype == eis_pt_unity) THEN
          tdata => tree%nodes
          tree%nodes => NULL()
          CALL eis_copy_tree(tdata(2), tree)
          DEALLOCATE(tdata)
          RETURN
        !Simplify 0/f
        ELSE IF (tree%nodes(2)%value%rtype == eis_pt_zero) THEN
          tree%value%ptype = eis_pt_constant
          tree%value%rtype = eis_pt_zero
          tree%value%numerical_data = tree%nodes(2)%value%numerical_data
          tree%co_value%deriv_values = -1
          IF (ALLOCATED(tree%co_value%text)) THEN
            DEALLOCATE(tree%co_value%text)
            ALLOCATE(tree%co_value%text, SOURCE = "0")
          END IF
          DEALLOCATE(tree%nodes)
          can_simplify = .FALSE.
          RETURN
        !Error on divide by zero
        ELSE IF (tree%nodes(1)%value%rtype == eis_pt_zero) THEN
          errcode = eis_err_maths_domain
          RETURN
        END IF
      ELSE IF (tree%value%rtype == eis_pt_op_plus) THEN
        !Simplify add zero on right
        IF (tree%nodes(1)%value%rtype == eis_pt_zero) THEN
          tdata => tree%nodes
          tree%nodes => NULL()
          CALL eis_copy_tree(tdata(2), tree)
          DEALLOCATE(tdata)
          RETURN
        !Simplify add zero on left
        ELSE IF (tree%nodes(2)%value%rtype == eis_pt_zero) THEN
          tdata => tree%nodes
          tree%nodes => NULL()
          CALL eis_copy_tree(tdata(1), tree)
          DEALLOCATE(tdata)
          RETURN
        END IF
      ELSE IF (tree%value%rtype == eis_pt_op_minus) THEN
        !Simplify subtract zero on right
        IF ((tree%nodes(1)%value%rtype == eis_pt_zero &
            .AND. tree%nodes(2)%value%rtype == eis_pt_zero) &
            .OR. (tree%nodes(1)%value%rtype == eis_pt_unity &
            .AND. tree%nodes(2)%value%rtype == eis_pt_unity)) THEN
          tree%value%ptype = eis_pt_constant
          tree%value%rtype = eis_pt_zero
          tree%value%numerical_data = 0.0_eis_num
          tree%co_value%deriv_values = -1
          IF (ALLOCATED(tree%co_value%text)) THEN
            DEALLOCATE(tree%co_value%text)
            ALLOCATE(tree%co_value%text, SOURCE = "0")
          END IF
          DEALLOCATE(tree%nodes)
          can_simplify = .TRUE.
          RETURN
        ELSE IF (tree%nodes(1)%value%rtype == eis_pt_zero) THEN
          tdata => tree%nodes
          tree%nodes => NULL()
          CALL eis_copy_tree(tdata(2), tree)
          DEALLOCATE(tdata)
          RETURN
        END IF
      ELSE IF (tree%value%rtype == eis_pt_op_unary_minus) THEN
        IF (tree%nodes(1)%value%rtype == eis_pt_zero) THEN
          tree%value%ptype = eis_pt_constant
          tree%value%rtype = eis_pt_zero
          tree%value%numerical_data = 0.0_eis_num
          tree%co_value%deriv_values = -1
          IF (ALLOCATED(tree%co_value%text)) THEN
            DEALLOCATE(tree%co_value%text)
            ALLOCATE(tree%co_value%text, SOURCE = "0")
          END IF
          DEALLOCATE(tree%nodes)
          can_simplify = .TRUE.
          RETURN
        ELSE IF (tree%nodes(1)%value%rtype == eis_pt_unity) THEN
          !Leave -unity as -unity to allow other simplification
          can_simplify = .TRUE.
          RETURN
        END IF
      END IF
    END IF

    IF (has_nodes) THEN
      IF (can_simplify) THEN
        DO inode = SIZE(tree%nodes), 1, -1
          CALL ees_push(eval, tree%nodes(inode)%value%numerical_data, err)
       END DO
      END IF
    END IF

    IF (can_simplify .AND. tree%value%ptype /= eis_pt_constant) THEN
      err = eis_err_none
      CALL ees_eval_element(eval, tree%value, params, status, err, .TRUE.)
      CALL ees_pop_scalar(eval, res, err)
      IF (err == eis_err_none .OR. (err == eis_err_maths_domain &
          .AND. tree%co_value%can_accept_maths_domain_errors)) THEN
        can_simplify = can_simplify &
            .AND. IAND(status, eis_status_no_simplify) == 0
        IF (IAND(status, eis_status_no_simplify) == 0) THEN
          tree%value%ptype = eis_pt_constant
          tree%value%rtype = eis_pt_constant
          tree%value%numerical_data = res
          tree%co_value%deriv_values = -1
          IF (ALLOCATED(tree%co_value%text)) THEN
            DEALLOCATE(tree%co_value%text)
            WRITE(rstring,'(ES30.17)') res
            ALLOCATE(tree%co_value%text, SOURCE = TRIM(ADJUSTL(rstring)))
          END IF
          IF (has_nodes) DEALLOCATE(tree%nodes)
        ELSE
          RETURN
        END IF
      ELSE
        errcode = IOR(errcode, err)
        IF (PRESENT(err_handler)) THEN
          IF (ALLOCATED(tree%co_value%text)) THEN
            CALL err_handler%add_error(eis_err_simplifier, err, &
                tree%co_value%text, tree%co_value%charindex, &
                filename = tree%co_value%filename, &
                line_number = tree%co_value%line_number, &
                context_filename = filename, &
                context_line_number = line_number, &
                full_line = full_line, full_line_pos = &
                tree%co_value%full_line_pos)
          ELSE
            CALL err_handler%add_error(eis_err_simplifier, err)
          END IF
        END IF
      END IF
    END IF

  END FUNCTION eis_simplify_tree



  !> @brief
  !> Convert an AST back into a stack
  !> @param[in] tree - AST to covert to stack
  !> @param[inout] stack_in - stack to build the converted AST in
  RECURSIVE SUBROUTINE eis_tree_to_stack(tree, stack_in)
    TYPE(eis_tree_item), INTENT(IN) :: tree
    TYPE(eis_stack), INTENT(INOUT) :: stack_in
    INTEGER :: inode

    IF (ASSOCIATED(tree%nodes)) THEN
      DO inode = SIZE(tree%nodes), 1, -1
        CALL eis_tree_to_stack(tree%nodes(inode), stack_in)
      END DO
    END IF

    CALL push_to_stack(stack_in, tree%value, tree%co_value)

  END SUBROUTINE eis_tree_to_stack



  RECURSIVE SUBROUTINE eis_copy_tree(node_source, node_dest)

    TYPE(eis_tree_item), INTENT(IN) :: node_source
    TYPE(eis_tree_item), INTENT(OUT) :: node_dest
    TYPE(eis_tree_item), DIMENSION(:), POINTER :: dest_nodes
    INTEGER :: i

    dest_nodes => node_dest%nodes

    node_dest = node_source
    node_dest%nodes => NULL()

    IF (ASSOCIATED(node_source%nodes)) THEN
      ALLOCATE(node_dest%nodes(SIZE(node_source%nodes)))
      DO i = 1, SIZE(node_source%nodes)
        CALL eis_copy_tree(node_source%nodes(i), node_dest%nodes(i))
      END DO
    END IF

    IF (ASSOCIATED(dest_nodes)) DEALLOCATE(dest_nodes)

  END SUBROUTINE eis_copy_tree



  !> @brief
  !> Convert a stack into a string containing a dot representation
  !> of the stack
  !> @param[in] stack - Stack to visualise
  !> @param[inout] str - String to hold the dot representation
  !> @param[in] nformat - Format string for literal numbers
  SUBROUTINE eis_visualise_stack(stack, str, nformat)

    TYPE(eis_stack), INTENT(IN) :: stack
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: str
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: nformat
    TYPE(eis_tree_item), POINTER :: root
    INTEGER :: sp

    IF (.NOT. stack%init) RETURN

    sp = stack%stack_point + 1
    DO WHILE (sp > 1)
      ALLOCATE(root)
      CALL eis_build_node(stack, sp, root)
      CALL eis_tree_to_dot(root, str, nformat)
      DEALLOCATE(root)
    END DO

  END SUBROUTINE eis_visualise_stack



  SUBROUTINE eis_tree_to_dot(root, str, nformat)
    TYPE(eis_tree_item) :: root
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: str
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: nformat
    INTEGER :: root_level

    root_level = 1

    CALL eis_append_string(str, 'strict graph G {')
    CALL dot_output(root, str, root_level, nformat)
    CALL eis_append_string(str, '}')

  END SUBROUTINE eis_tree_to_dot



  !> @brief
  !> Convert a stack into a string the infix version of the stack
  !> of the stack
  !> @param[in] stack - Stack to visualise
  !> @param[inout] str - String to hold the infix representation
  !> @param[in] nformat - Format string for literal numbers
  SUBROUTINE eis_infix_stack(stack, str, nformat)

    TYPE(eis_stack), INTENT(IN) :: stack
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: str
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: nformat
    TYPE(eis_tree_item), POINTER :: root
    CHARACTER(LEN=:), ALLOCATABLE :: temp
    INTEGER :: sp

    IF (.NOT. stack%init) RETURN

    sp = stack%stack_point + 1
    DO WHILE (sp > 1)
      ALLOCATE(root)
      CALL eis_build_node(stack, sp, root)
      CALL infix_output(root, temp, nformat)
      CALL eis_prepend_string(str, temp, newline = .FALSE.)
      IF (sp > 1) CALL eis_prepend_string(str, ', ', newline = .FALSE.)
      DEALLOCATE(temp)
      DEALLOCATE(root)
    END DO

  END SUBROUTINE eis_infix_stack



  RECURSIVE SUBROUTINE dot_output(node, str, id_in, nformat)

    TYPE(eis_tree_item), INTENT(IN) :: node
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: str
    INTEGER, INTENT(INOUT) :: id_in
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: nformat
    INTEGER :: inode, my_id, ios
    CHARACTER(LEN=100) :: rstring
    CHARACTER(LEN=12) :: ds
    CHARACTER(LEN=5) ::  val1, val2
    LOGICAL :: is_string

    my_id = id_in

    is_string = (node%value%ptype == eis_pt_constant &
        .AND. node%value%value == eis_pt_character)

    SELECT CASE (node%value%ptype)
      CASE(eis_pt_function)
        ds = "box"
      CASE(eis_pt_operator)
        ds = "diamond"
      CASE DEFAULT
        IF (is_string) THEN
          ds = "doublecircle"
        ELSE
          ds = "circle"
        END IF
    END SELECT

    WRITE(val1, '(I5.5)') id_in
    IF (node%value%ptype /= eis_pt_constant .OR. .NOT. PRESENT(nformat) .OR. &
        is_string) THEN
      CALL eis_append_string(str, TRIM(val1) // '[label="' // &
          node%co_value%text // '"] [shape=' // TRIM(ds) //'];')
    ELSE
      WRITE(rstring, nformat, iostat = ios) node%value%numerical_data
      IF (ios /= 0) rstring = 'FORMAT ERROR!'
      CALL eis_append_string(str, TRIM(val1) // '[label="' // &
          TRIM(rstring) // '"] [shape=' // TRIM(ds) //'];')
    END IF

    IF (ASSOCIATED(node%nodes)) THEN
      DO inode = SIZE(node%nodes), 1, -1
        id_in = id_in + 1
        WRITE(val1, '(I5.5)') my_id
        WRITE(val2, '(I5.5)') id_in
        CALL eis_append_string(str, val1 // ' -- ' // val2 // ';')
        CALL dot_output(node%nodes(inode), str, id_in, nformat)
      END DO
    END IF

  END SUBROUTINE dot_output



  RECURSIVE SUBROUTINE infix_output(node, str, nformat)

    TYPE(eis_tree_item), INTENT(IN) :: node
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: str
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: nformat
    INTEGER :: inode
    CHARACTER(LEN=:), ALLOCATABLE :: param1, param2
    CHARACTER(LEN=1) :: bb1, ab1, bb2, ab2
    CHARACTER(LEN=100) :: rstring
    INTEGER :: ios

    bb1 = ''
    ab1 = ''
    bb2 = ''
    ab2 = ''

    SELECT CASE (node%value%ptype)
      CASE(eis_pt_function, eis_pt_emplaced_function)
        CALL eis_append_string(str, TRIM(node%co_value%text), newline = .FALSE.)
        IF (ASSOCIATED(node%nodes)) THEN
        CALL eis_append_string(str, '(', newline = .FALSE.)
          DO inode = SIZE(node%nodes), 1, -1
            CALL infix_output(node%nodes(inode), param1, nformat)
            CALL eis_append_string(str, TRIM(param1), newline = .FALSE.)
            IF (inode /= 1) CALL eis_append_string(str,',', newline = .FALSE.)
            DEALLOCATE(param1)
          END DO
          CALL eis_append_string(str,')', newline = .FALSE.)
        END IF
      CASE(eis_pt_operator)
        IF (SIZE(node%nodes) == 1) THEN
          IF (node%nodes(1)%value%ptype == eis_pt_operator) THEN
            IF (node%nodes(1)%co_value%precedence <= node%co_value%precedence) &
                THEN
              bb1 = '('
              ab1 = ')'
            END IF
          END IF
          CALL infix_output(node%nodes(1), param1, nformat)
          CALL eis_append_string(str, ' ' // TRIM(node%co_value%text) &
              // TRIM(bb1) // TRIM(param1) // TRIM(ab1), newline = .FALSE.)
        ELSE
          CALL infix_output(node%nodes(2), param1, nformat)
          CALL infix_output(node%nodes(1), param2, nformat)
          IF (node%nodes(2)%value%ptype == eis_pt_operator) THEN
            IF (node%nodes(2)%co_value%precedence <= node%co_value%precedence) &
                THEN
              bb1 = '('
              ab1 = ')'
            END IF
          END IF
          IF (node%nodes(1)%value%ptype == eis_pt_operator) THEN
            IF (node%nodes(1)%co_value%precedence <= node%co_value%precedence) &
                THEN
              bb2 = '('
              ab2 = ')'
            END IF
          END IF
          CALL eis_append_string(str,TRIM(bb1) // TRIM(param1) // TRIM(ab1) &
              // TRIM(node%co_value%text) // TRIM(bb2) // TRIM(param2) &
              // TRIM(ab2), newline = .FALSE.)
        END IF
      CASE (eis_pt_constant)
        IF (node%value%value == eis_pt_character .OR. &
            .NOT. PRESENT(nformat)) THEN
          CALL eis_append_string(str, TRIM(node%co_value%text), &
              newline = .FALSE.)
        ELSE
          WRITE(rstring, nformat, iostat = ios) node%value%numerical_data
          IF (ios /= 0) rstring = 'FORMAT ERROR!'
          CALL eis_append_string(str, TRIM(rstring), newline = .FALSE.)
        END IF
      CASE DEFAULT
        CALL eis_append_string(str, TRIM(node%co_value%text))
    END SELECT

  END SUBROUTINE infix_output

END MODULE eis_tree_mod
