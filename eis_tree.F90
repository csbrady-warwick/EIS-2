MODULE eis_tree_mod

  USE, INTRINSIC :: ISO_C_BINDING
  USE eis_constants
  USE eis_header
  USE eis_stack_mod
  USE eis_eval_stack_mod
  USE eis_error_mod
  IMPLICIT NONE

  TYPE :: eis_tree_item
    TYPE(eis_stack_element) :: value
    TYPE(eis_stack_co_element) :: co_value
    TYPE(eis_tree_item), DIMENSION(:), POINTER :: nodes => NULL()
    LOGICAL :: is_new = .FALSE.
    CONTAINS
    FINAL :: eit_destructor
  END TYPE eis_tree_item

  TYPE(eis_eval_stack), SAVE :: eval

  CONTAINS

  PURE ELEMENTAL SUBROUTINE eit_destructor(this)
    TYPE(eis_tree_item), INTENT(INOUT) :: this
    INTEGER :: inode

    IF (ASSOCIATED(this%nodes)) DEALLOCATE(this%nodes)
    CALL deallocate_stack_element(this%value)
    CALL deallocate_stack_co_element(this%co_value)

  END SUBROUTINE eit_destructor



  SUBROUTINE eis_simplify_stack(stack, params, errcode, err_handler, stack_out)

    TYPE(eis_stack), INTENT(INOUT) :: stack
    TYPE(C_PTR), INTENT(IN) :: params
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    TYPE(eis_stack), INTENT(OUT), OPTIONAL, TARGET :: stack_out
    TYPE(eis_stack), POINTER :: simplified
    TYPE(eis_tree_item), POINTER :: root
    INTEGER :: sp

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
      CALL eis_simplify_tree(root, params, errcode, err_handler)
      IF (errcode /= eis_err_none) THEN
        DEALLOCATE(root)
        RETURN
      END IF
      CALL eis_tree_to_stack(root, simplified)
      DEALLOCATE(root)
    END DO

    simplified%cap_bits = stack%cap_bits

    IF (.NOT. PRESENT(stack_out)) THEN
      CALL deallocate_stack(stack)
      CALL copy_stack(simplified, stack)
      CALL deallocate_stack(simplified)
      DEALLOCATE(simplified)
    END IF

  END SUBROUTINE eis_simplify_stack



  RECURSIVE SUBROUTINE eis_build_node(stack, curindex, current)
    TYPE(eis_stack) :: stack
    INTEGER, INTENT(INOUT) :: curindex
    TYPE(eis_tree_item), POINTER, INTENT(INOUT) :: current
    TYPE(eis_tree_item), POINTER :: p
    INTEGER :: iparam, pcount, curprime

    curindex = curindex - 1
    current%value = stack%entries(curindex)
    IF (ALLOCATED(stack%co_entries)) &
        current%co_value = stack%co_entries(curindex)

    IF (current%value%ptype == c_pt_function .OR. &
        current%value%ptype == c_pt_operator .OR. &
        current%value%ptype == c_pt_emplaced_function) THEN
      pcount = current%value%actual_params
      ALLOCATE(current%nodes(pcount))
      DO iparam = 1, pcount
        p => current%nodes(iparam)
        CALL eis_build_node(stack, curindex, p)
      END DO
    END IF
  END SUBROUTINE eis_build_node



  SUBROUTINE move_tree_item(source, dest)
    TYPE(eis_tree_item), INTENT(INOUT) :: source
    TYPE(eis_tree_item), INTENT(OUT) :: dest

    dest = source
    source%nodes => NULL()

  END SUBROUTINE move_tree_item



  RECURSIVE SUBROUTINE eis_simplify_tree(tree, params, errcode, err_handler)
    TYPE(eis_tree_item), INTENT(INOUT) :: tree
    TYPE(C_PTR), INTENT(IN) :: params
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    INTEGER :: inode
    LOGICAL :: can_simplify
    CHARACTER(LEN=:), ALLOCATABLE :: temp, temp2
    REAL(eis_num) :: res
    TYPE(eis_eval_stack) :: eval
    INTEGER(eis_error) :: err
    INTEGER(eis_status) :: status
    CHARACTER(LEN=10) :: rstring

    status = eis_status_none

    IF (ASSOCIATED(tree%nodes)) THEN
      can_simplify = .TRUE.
       
      DO inode = SIZE(tree%nodes), 1, -1
        CALL eis_simplify_tree(tree%nodes(inode), params, errcode, err_handler)
        IF (tree%nodes(inode)%value%ptype /= c_pt_constant) THEN
          !This can only happen if one of the nodes downstream returned a
          !no simplify status
          RETURN
        END IF
        can_simplify = can_simplify .AND. tree%nodes(inode)%value%can_simplify
      END DO

      IF (can_simplify) THEN
        DO inode = SIZE(tree%nodes), 1, -1
          CALL eval%push(tree%nodes(inode)%value%numerical_data, err)
        END DO
        err = eis_err_none
        CALL eval%eval_element(tree%value, params, status, err)
        CALL eval%pop(res, err)
        IF (err == eis_err_none) THEN
          IF (IAND(status, eis_status_no_simplify) == 0) THEN
            tree%value%ptype = c_pt_constant
            tree%value%numerical_data = res
            IF (ALLOCATED(tree%co_value%text)) THEN
              DEALLOCATE(tree%co_value%text)
              WRITE(rstring,'(G10.4)') res
              ALLOCATE(tree%co_value%text, SOURCE = TRIM(ADJUSTL(rstring)))
            END IF
            DEALLOCATE(tree%nodes)
          ELSE
            RETURN
          END IF
        ELSE
          errcode = IOR(errcode, err)
          IF (PRESENT(err_handler)) THEN
            IF (ALLOCATED(tree%co_value%text)) THEN
              CALL err_handler%add_error(eis_err_simplifier, err, &
                  tree%co_value%text, tree%co_value%charindex)
            ELSE
              CALL err_handler%add_error(eis_err_simplifier, err)
            END IF
          END IF
        END IF
      ELSE
        tree%value%can_simplify = .FALSE.
      END IF
    END IF

  END SUBROUTINE eis_simplify_tree



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



  SUBROUTINE eis_simple_dot(root)
    TYPE(eis_tree_item) :: root

    OPEN(UNIT=10)
    CALL eis_tree_to_dot(root, 10)
    FLUSH(10)
    CLOSE(10)

  END SUBROUTINE eis_simple_dot



  SUBROUTINE eis_visualise_stack(stack, lun)

    TYPE(eis_stack), INTENT(IN) :: stack
    INTEGER, INTENT(IN) :: lun
    TYPE(eis_tree_item), POINTER :: root
    INTEGER :: sp

    IF (.NOT. stack%init) RETURN

    sp = stack%stack_point + 1
    DO WHILE (sp > 1)
      ALLOCATE(root)
      CALL eis_build_node(stack, sp, root)
      CALL eis_tree_to_dot(root, lun)
      DEALLOCATE(root)
    END DO

  END SUBROUTINE eis_visualise_stack



  SUBROUTINE eis_tree_to_dot(root, lun)
    TYPE(eis_tree_item) :: root
    INTEGER, INTENT(IN) :: lun
    INTEGER :: root_level

    root_level = 1

    WRITE(lun,*) 'strict graph G {'
    CALL dot_output(root, root_level)
    WRITE(lun,*) '}'

  END SUBROUTINE eis_tree_to_dot



  RECURSIVE SUBROUTINE dot_output(node, id_in)

    TYPE(eis_tree_item), INTENT(IN) :: node
    INTEGER, INTENT(INOUT) :: id_in
    INTEGER :: inode, my_id
    CHARACTER(LEN=10) :: ds

    my_id = id_in

    SELECT CASE (node%value%ptype)
      CASE(c_pt_function)
        ds = "box"
      CASE(c_pt_operator)
        ds = "diamond"
      CASE DEFAULT
        ds = "circle"
    END SELECT

    WRITE(10, '(I4.4,A,A,A)') id_in, '[label="'//node%co_value%text &
        //'"] [shape=', TRIM(ds),'];'
    IF (ASSOCIATED(node%nodes)) THEN
      DO inode = SIZE(node%nodes), 1, -1
        id_in = id_in + 1
        WRITE(10,'(I4.4,A,I4.4,A)') my_id,' -- ', id_in, ';'
        CALL dot_output(node%nodes(inode), id_in)
      END DO
    END IF

  END SUBROUTINE dot_output

END MODULE eis_tree_mod
