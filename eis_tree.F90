MODULE eis_tree_mod

  USE eis_constants
  USE eis_header
  USE eis_stack_mod
  IMPLICIT NONE

  TYPE :: eis_tree_item
    TYPE(eis_stack_element) :: value
    TYPE(eis_tree_item), DIMENSION(:), POINTER :: nodes => NULL()
    LOGICAL :: is_new = .FALSE.
    CONTAINS
    FINAL :: eit_destructor
  END TYPE eis_tree_item

  CONTAINS

  PURE ELEMENTAL SUBROUTINE eit_destructor(this)
    TYPE(eis_tree_item), INTENT(INOUT) :: this
    INTEGER :: inode

    IF (ASSOCIATED(this%nodes)) DEALLOCATE(this%nodes)
    CALL deallocate_stack_element(this%value)

  END SUBROUTINE eit_destructor


  SUBROUTINE eis_simplify_stack(stack, stack_out)

    TYPE(eis_stack), INTENT(INOUT) :: stack
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
    sp = stack%stack_point
    DO WHILE (sp > 1)
      PRINT *,'Running simplify'
      ALLOCATE(root)
      CALL eis_build_node(stack, sp, root)
      CALL eis_simplify_tree(root)
      CALL eis_tree_to_dot(root)
      CALL eis_tree_to_stack(root, simplified)
      DEALLOCATE(root)
    END DO

    IF (.NOT. PRESENT(stack_out)) THEN
      CALL deallocate_stack(stack)
      CALL copy_stack(simplified, stack)
      CALL deallocate_stack(simplified)
      DEALLOCATE(simplified)
    END IF

  END SUBROUTINE eis_simplify_stack



  SUBROUTINE eis_build_tree(stack)
    TYPE(eis_stack) :: stack
    TYPE(eis_tree_item), POINTER :: top
    TYPE(eis_stack) :: stack2
    INTEGER :: sp

    sp = stack%stack_point
    ALLOCATE(top)
    CALL eis_build_node(stack, sp, top)
    CALL eis_tree_to_dot(top)

!    CALL eis_simplify_tree(top)
!    CALL eis_tree_to_dot(top)

    CALL initialise_stack(stack2)
    CALL eis_tree_to_stack(top, stack2)

    PRINT *,'Reconstructed tree is '
    CALL display_tokens_inline(stack2)
    CALL deallocate_stack(stack2)
    DEALLOCATE(top)

  END SUBROUTINE eis_build_tree



  RECURSIVE SUBROUTINE eis_build_node(stack, curindex, current)
    TYPE(eis_stack) :: stack
    INTEGER, INTENT(INOUT) :: curindex
    TYPE(eis_tree_item), POINTER, INTENT(INOUT) :: current
    TYPE(eis_tree_item), POINTER :: p
    INTEGER :: iparam, pcount

    current%value = stack%entries(curindex)

    IF (current%value%ptype == c_pt_function .OR. &
        current%value%ptype == c_pt_operator) THEN
      IF (current%value%ptype == c_pt_function) THEN 
        pcount = current%value%actual_params
      ELSE
        pcount = current%value%params
      END IF
      ALLOCATE(current%nodes(pcount))
      DO iparam = 1, pcount
        curindex = curindex - 1
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



  RECURSIVE SUBROUTINE eis_simplify_tree(tree)
    TYPE(eis_tree_item), INTENT(INOUT) :: tree
    INTEGER :: inode
    LOGICAL :: can_simplify, so1, so2
    CHARACTER(LEN=:), ALLOCATABLE :: temp, temp2
    CHARACTER(LEN=1) :: ob1, cb1, ob2, cb2

    IF (ASSOCIATED(tree%nodes)) THEN
      can_simplify = .TRUE.
      IF (tree%value%ptype == c_pt_operator .AND. SIZE(tree%nodes) == 2) THEN
        so1 = ASSOCIATED(tree%nodes(1)%nodes)
        so2 = ASSOCIATED(tree%nodes(2)%nodes)
      END IF
       
      DO inode = SIZE(tree%nodes), 1, -1
        CALL eis_simplify_tree(tree%nodes(inode))
        can_simplify = can_simplify .AND. tree%nodes(inode)%value%can_simplify
      END DO

      IF (can_simplify) THEN
        IF (tree%value%ptype == c_pt_function) THEN
          ALLOCATE(temp, SOURCE = tree%value%text // "(")
          DO inode = SIZE(tree%nodes), 1, -1
            IF (inode > 1) THEN
              ALLOCATE(temp2, SOURCE = temp // tree%nodes(inode)%value%text &
                  // ",")
            ELSE
              ALLOCATE(temp2, SOURCE = temp // tree%nodes(inode)%value%text)
            END IF
            DEALLOCATE(temp)
            CALL MOVE_ALLOC(temp2, temp)
          END DO
          ALLOCATE(temp2, SOURCE = temp // ")")
          DEALLOCATE(temp)
          DEALLOCATE(tree%value%text)
          CALL MOVE_ALLOC(temp2, tree%value%text)
        ELSE
          IF (SIZE(tree%nodes) == 1) THEN
            ALLOCATE(temp, SOURCE = "("//tree%value%text &
                // tree%nodes(1)%value%text // ")")
          ELSE
            IF (so2) THEN
              ob2="("; cb2=")"
            ELSE
              ob2=""; cb2=""
            END IF

            IF (so1) THEN
              ob1="("; cb1=")"
            ELSE
              ob1=""; cb1=""
            END IF

            ALLOCATE(temp, SOURCE = TRIM(ob2)//tree%nodes(2)%value%text&
                //TRIM(cb2) &
                // tree%value%text // TRIM(ob1) // tree%nodes(1)%value%text &
                //TRIM(cb1))
          END IF
          DEALLOCATE(tree%value%text)
          CALL MOVE_ALLOC(temp, tree%value%text)
        END IF
        tree%value%ptype = c_pt_constant
        DEALLOCATE(tree%nodes)
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

    CALL push_to_stack(stack_in, tree%value)

  END SUBROUTINE eis_tree_to_stack



  SUBROUTINE eis_tree_to_dot(root)
    TYPE(eis_tree_item) :: root
    INTEGER :: root_level

    root_level = 1

    OPEN(unit = 10, file='test.dot')
    WRITE(10,*) 'strict graph G {'
    CALL dot_output(root, root_level)
    WRITE(10,*) '}'
    CLOSE(10)

  END SUBROUTINE eis_tree_to_dot



  RECURSIVE SUBROUTINE dot_output(node, id_in)

    TYPE(eis_tree_item), INTENT(IN) :: node
    INTEGER, INTENT(INOUT) :: id_in
    INTEGER :: inode, my_id

    my_id = id_in

    WRITE(10, '(I4.4,A)') id_in, '[label="'//node%value%text//'"];'
    IF (ASSOCIATED(node%nodes)) THEN
      DO inode = SIZE(node%nodes), 1, -1
        id_in = id_in + 1
        WRITE(10,'(I4.4,A,I4.4,A)') my_id,' -- ', id_in, ';'
        CALL dot_output(node%nodes(inode), id_in)
      END DO
    END IF

  END SUBROUTINE dot_output

END MODULE eis_tree_mod
