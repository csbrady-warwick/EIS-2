MODULE eis_tree_mod

  USE eis_constants
  USE eis_header
  USE eis_stack_mod
  IMPLICIT NONE

  TYPE :: eis_tree_item
    TYPE(eis_stack_element) :: value
    TYPE(eis_tree_item), DIMENSION(:), POINTER :: nodes => NULL()
  END TYPE eis_tree_item

  CONTAINS

  SUBROUTINE eis_build_tree(stack)
    TYPE(eis_stack) :: stack
    TYPE(eis_tree_item) :: top
    TYPE(eis_stack) :: stack2
    INTEGER :: sp

    sp = stack%stack_point

    top = eis_build_node(stack, sp)
    CALL eis_tree_to_dot(top)

    CALL initialise_stack(stack2)
    CALL eis_tree_to_stack(top, stack2)

    PRINT *,'Reconstructed tree is '
    CALL display_tokens_inline(stack2)

  END SUBROUTINE eis_build_tree



  RECURSIVE FUNCTION eis_build_node(stack, curindex) RESULT(current)
    TYPE(eis_stack) :: stack
    INTEGER, INTENT(INOUT) :: curindex
    TYPE(eis_tree_item) :: current
    INTEGER :: iparam

    current%value = stack%entries(curindex)
!    PRINT *, current%value%text

    IF (stack%entries(curindex)%ptype == c_pt_function .OR. &
        stack%entries(curindex)%ptype == c_pt_operator) THEN
        ALLOCATE(current%nodes(stack%entries(curindex)%params))
        DO iparam = 1, stack%entries(curindex)%params
          curindex = curindex - 1
          current%nodes(iparam) = eis_build_node(stack, curindex)
        END DO
    END IF

  END FUNCTION eis_build_node



  RECURSIVE SUBROUTINE eis_simplify_tree(tree)
    TYPE(eis_tree_item), INTENT(INOUT) :: tree
    INTEGER :: inode
    LOGICAL :: can_simplify
    CHARACTER(LEN=:), ALLOCATABLE :: temp, temp2

    IF (.NOT. tree%value%can_simplify) RETURN

    IF (ASSOCIATED(tree%nodes)) THEN
      can_simplify = .TRUE.
      DO inode = SIZE(tree%nodes), 1, -1
        CALL eis_simpify_tree(tree%nodes(inode)
        can_simplify = can_simplify .AND. tree%nodes(inode)%value%can_simplify
      END DO
      IF (can_simplify) THEN
        ALLOCATE(temp, SOURCE = tree%value%text // "(")
        DO inode = SIZE(tree%nodes), 1, -1
        END DO
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
