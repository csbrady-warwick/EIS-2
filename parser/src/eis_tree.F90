! Copyright (c) 2019-2026, C. S. Brady and H. Ratcliffe
! SPDX-License-Identifier: BSD-3-Clause

MODULE eis_tree_mod

  USE, INTRINSIC :: ISO_C_BINDING
  USE eis_error_mod
  USE eis_eval_stack_mod
  USE eis_header
  USE eis_stack_mod
  USE eis_utils
  USE eis_registry_mod
  USE eis_core_functions_mod
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

  !> @class
  !> A term in a chain of + and - or of * and / along with the number that it
  !> counts for in the chain. The number is negative for terms that are
  !> subtracted or divided by
  TYPE :: eis_chain_term
    TYPE(eis_tree_item), POINTER :: node => NULL()
    REAL(eis_num) :: coefficient = 0.0_eis_num
  END TYPE eis_chain_term

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
  SUBROUTINE eis_simplify_stack(stack, params, errcode, err_handler, &
      stack_out, maths_fail_quickly)

    TYPE(eis_stack), INTENT(INOUT) :: stack
    TYPE(C_PTR), INTENT(IN) :: params
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    TYPE(eis_stack), INTENT(OUT), OPTIONAL, TARGET :: stack_out
    !> Report maths domain errors during simplification. Optional, default
    !> .FALSE. which replaces an expression that fails with a maths domain
    !> error with a function that raises the error when it is evaluated
    LOGICAL, INTENT(IN), OPTIONAL :: maths_fail_quickly
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
            stack%filename, stack%line_number, stack%full_line, &
            maths_fail_quickly)
      ELSE
        dummy = eis_simplify_tree(root, params, errcode, err_handler, &
            maths_fail_quickly = maths_fail_quickly)
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
    !Jumps are not nodes of the tree
    DO WHILE (stack%entries(curindex)%ptype == eis_pt_jump_conditional &
        .OR. stack%entries(curindex)%ptype == eis_pt_jump_unconditional)
      curindex = curindex - 1
    END DO
    current%value = stack%entries(curindex)
    IF (current%value%rtype == eis_pt_control_if_join) THEN
      current%value%rtype = eis_pt_control_if
      current%value%eval_fn => eis_if
    END IF
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
  !> Is a node a literal number that can be moved and combined with other
  !> literal numbers
  !> @param[in] node - Node to test
  !> @return is_constant
  FUNCTION eis_is_hoistable_constant(node) RESULT(is_constant)
    TYPE(eis_tree_item), INTENT(IN) :: node
    LOGICAL :: is_constant

    is_constant = node%value%ptype == eis_pt_constant &
        .AND. node%value%can_simplify

  END FUNCTION eis_is_hoistable_constant



  !> @brief
  !> Fill a node with a binary operator and two operands that are copies of
  !> other nodes. The operator is taken from whichever of two other nodes holds
  !> the wanted operator
  !> @param[inout] node - Node to fill. Must not have any child nodes
  !> @param[in] wanted - Operator that the node should have
  !> @param[in] pval - Value of the first node that can supply the operator
  !> @param[in] pco - Co value of the first node that can supply the operator
  !> @param[in] cval - Value of the second node that can supply the operator
  !> @param[in] cco - Co value of the second node that can supply the operator
  !> @param[in] left - Node to copy to be the left operand
  !> @param[in] right - Node to copy to be the right operand
  SUBROUTINE eis_make_op_node(node, wanted, pval, pco, cval, cco, left, right)
    TYPE(eis_tree_item), INTENT(INOUT) :: node
    INTEGER, INTENT(IN) :: wanted
    TYPE(eis_stack_element), INTENT(IN) :: pval, cval
    TYPE(eis_stack_co_element), INTENT(IN) :: pco, cco
    TYPE(eis_tree_item), INTENT(IN) :: left, right

    IF (pval%rtype == wanted) THEN
      node%value = pval
      node%co_value = pco
    ELSE
      node%value = cval
      node%co_value = cco
    END IF
    ALLOCATE(node%nodes(2))
    CALL eis_copy_tree(left, node%nodes(2))
    CALL eis_copy_tree(right, node%nodes(1))

  END SUBROUTINE eis_make_op_node



  !> @brief
  !> Reorder a node that is a binary +, -, * or / so that two constants that
  !> are in different parts of a chain of these operators end up in a single
  !> subtree. The simplifier then combines them. For example (x + 1) + 2
  !> becomes x + (1 + 2) and 2 * (3 / x) becomes (2 * 3) / x
  !> An operator that the reordering needs but that is not in the expression
  !> is taken from the global registry. All other nodes are copied unchanged.
  !> If the global registry does not hold a needed operator then the node is
  !> left alone
  !> @param[inout] tree - Node to reorder
  !> @return hoisted - Whether the node was reordered
  FUNCTION eis_hoist_constants(tree) RESULT(hoisted)
    TYPE(eis_tree_item), INTENT(INOUT) :: tree
    LOGICAL :: hoisted
    INTEGER :: oper, inver, prt, crt, top, inner, layout, side
    LOGICAL :: u_const, v_const, same, child_left
    TYPE(eis_tree_item), POINTER :: child, other, u, v, w
    TYPE(eis_tree_item), POINTER :: inner_node, lsrc, rsrc
    TYPE(eis_tree_item), DIMENSION(:), POINTER :: new_nodes, old_nodes
    TYPE(eis_stack_element) :: pval, cval, topval, ival
    TYPE(eis_stack_co_element) :: pco, cco, topco, ico
    LOGICAL :: ok

    hoisted = .FALSE.

    !oper is the commutative operator of the pair, inver is its inverse
    IF (tree%value%rtype == eis_pt_op_plus &
        .OR. tree%value%rtype == eis_pt_op_minus) THEN
      oper = eis_pt_op_plus
      inver = eis_pt_op_minus
    ELSE IF (tree%value%rtype == eis_pt_op_multiply &
        .OR. tree%value%rtype == eis_pt_op_divide) THEN
      oper = eis_pt_op_multiply
      inver = eis_pt_op_divide
    ELSE
      RETURN
    END IF
    IF (tree%value%ptype /= eis_pt_operator) RETURN
    IF (.NOT. tree%value%can_simplify) RETURN
    IF (.NOT. ASSOCIATED(tree%nodes)) RETURN
    IF (SIZE(tree%nodes) /= 2) RETURN

    !Node 2 is the left operand and node 1 is the right operand. The child
    !is an operator from the same pair on one side that has a constant on
    !one of its sides while the other operand of this node is a constant
    prt = tree%value%rtype
    DO side = 1, 2
      child => tree%nodes(side)
      other => tree%nodes(3 - side)
      IF (child%value%ptype /= eis_pt_operator) CYCLE
      IF (.NOT. child%value%can_simplify) CYCLE
      IF (.NOT. ASSOCIATED(child%nodes)) CYCLE
      IF (SIZE(child%nodes) /= 2) CYCLE
      crt = child%value%rtype
      IF (crt /= oper .AND. crt /= inver) CYCLE
      IF (.NOT. eis_is_hoistable_constant(other)) CYCLE
      u_const = eis_is_hoistable_constant(child%nodes(2))
      v_const = eis_is_hoistable_constant(child%nodes(1))
      IF (u_const .EQV. v_const) CYCLE

      !Layouts, where w is the other operand, u is the left operand of the
      !child and v is the right operand of the child
      !0 : (u C v) P w -> u C (v J w)   v and w are constants
      !1 : (u O v) P w -> v O (u P w)   u and w are constants
      !2 : (u I v) P w -> (u P w) I v   u and w are constants
      !3 : w P (u C v) -> (w P u) T v   u and w are constants
      !4 : w O (u C v) -> u O (w C v)   v and w are constants
      !5 : w I (u C v) -> (w J v) I u   v and w are constants
      !Here O is the commutative operator, I is the inverse, P is the
      !operator of this node and C is the operator of the child. J is O if P
      !and C are the same and I otherwise. T is O if P and C are the same and
      !I otherwise
      same = (prt == crt)
      child_left = (side == 2)
      IF (child_left .AND. v_const) THEN
        layout = 0
        top = crt
        IF (same) THEN
          inner = oper
        ELSE
          inner = inver
        END IF
      ELSE IF (child_left) THEN
        IF (crt == oper) THEN
          layout = 1
        ELSE
          layout = 2
        END IF
        top = crt
        inner = prt
      ELSE IF (u_const) THEN
        layout = 3
        IF (same) THEN
          top = oper
        ELSE
          top = inver
        END IF
        inner = prt
      ELSE IF (prt == oper) THEN
        layout = 4
        top = prt
        inner = crt
      ELSE
        layout = 5
        top = prt
        IF (same) THEN
          inner = oper
        ELSE
          inner = inver
        END IF
      END IF
      !Get the operators from the nodes that hold them, or from the global
      !registry if the expression does not contain them
      pval = tree%value
      pco = tree%co_value
      cval = child%value
      cco = child%co_value
      CALL eis_pick_op_template(top, pval, pco, cval, cco, cval, cco, topval, &
          topco, ok)
      IF (.NOT. ok) CYCLE
      CALL eis_pick_op_template(inner, pval, pco, cval, cco, cval, cco, ival, &
          ico, ok)
      IF (.NOT. ok) CYCLE

      u => child%nodes(2)
      v => child%nodes(1)
      w => other

      ALLOCATE(inner_node)
      SELECT CASE (layout)
        CASE (0)
          CALL eis_make_op_node(inner_node, inner, ival, ico, ival, ico, v, w)
          lsrc => u
          rsrc => inner_node
        CASE (1)
          CALL eis_make_op_node(inner_node, inner, ival, ico, ival, ico, u, w)
          lsrc => v
          rsrc => inner_node
        CASE (2)
          CALL eis_make_op_node(inner_node, inner, ival, ico, ival, ico, u, w)
          lsrc => inner_node
          rsrc => v
        CASE (3)
          CALL eis_make_op_node(inner_node, inner, ival, ico, ival, ico, w, u)
          lsrc => inner_node
          rsrc => v
        CASE (4)
          CALL eis_make_op_node(inner_node, inner, ival, ico, ival, ico, w, v)
          lsrc => u
          rsrc => inner_node
        CASE DEFAULT
          CALL eis_make_op_node(inner_node, inner, ival, ico, ival, ico, w, v)
          lsrc => inner_node
          rsrc => u
      END SELECT

      ALLOCATE(new_nodes(2))
      CALL eis_copy_tree(lsrc, new_nodes(2))
      CALL eis_copy_tree(rsrc, new_nodes(1))
      DEALLOCATE(inner_node)

      tree%value = topval
      tree%co_value = topco
      old_nodes => tree%nodes
      tree%nodes => new_nodes
      DEALLOCATE(old_nodes)
      hoisted = .TRUE.
      RETURN
    END DO

  END FUNCTION eis_hoist_constants



  !> @brief
  !> Split a node that is an operator with a constant on exactly one side into
  !> the part that is not constant (x), the constant (k) and the sign with
  !> which each contributes to the node. For + and - the sign says whether the
  !> part is added or subtracted, for * and / whether it multiplies or divides
  !> @param[in] child - Node to split
  !> @param[in] oper - Commutative operator of the pair
  !> @param[in] inver - Inverse operator of the pair
  !> @param[out] ok - Whether the node could be split
  !> @param[out] x - Pointer to the part that is not constant
  !> @param[out] k - Pointer to the constant
  !> @param[out] sx - Sign of the part that is not constant
  !> @param[out] sk - Sign of the constant
  SUBROUTINE eis_split_const_node(child, oper, inver, ok, x, k, sx, sk)
    TYPE(eis_tree_item), INTENT(IN) :: child
    INTEGER, INTENT(IN) :: oper, inver
    LOGICAL, INTENT(OUT) :: ok
    TYPE(eis_tree_item), POINTER, INTENT(INOUT) :: x, k
    INTEGER, INTENT(OUT) :: sx, sk
    LOGICAL :: u_const, v_const
    INTEGER :: inverse

    ok = .FALSE.
    IF (child%value%ptype /= eis_pt_operator) RETURN
    IF (.NOT. child%value%can_simplify) RETURN
    IF (.NOT. ASSOCIATED(child%nodes)) RETURN
    IF (SIZE(child%nodes) /= 2) RETURN
    IF (child%value%rtype /= oper .AND. child%value%rtype /= inver) RETURN
    u_const = eis_is_hoistable_constant(child%nodes(2))
    v_const = eis_is_hoistable_constant(child%nodes(1))
    IF (u_const .EQV. v_const) RETURN

    inverse = 1
    IF (child%value%rtype == inver) inverse = -1
    IF (v_const) THEN
      x => child%nodes(2)
      k => child%nodes(1)
      sx = 1
      sk = inverse
    ELSE
      x => child%nodes(1)
      k => child%nodes(2)
      sk = 1
      sx = inverse
    END IF
    ok = .TRUE.

  END SUBROUTINE eis_split_const_node



  !> @brief
  !> Pick the value and co value of whichever of three nodes holds the wanted
  !> operator
  !> @param[in] wanted - Operator that is wanted
  !> @param[in] pval - Value of the first node
  !> @param[in] pco - Co value of the first node
  !> @param[in] lval - Value of the second node
  !> @param[in] lco - Co value of the second node
  !> @param[in] rval - Value of the third node
  !> @param[in] rco - Co value of the third node
  !> @param[inout] tval - Value of the picked node
  !> @param[inout] tco - Co value of the picked node
  SUBROUTINE eis_pick_op_template(wanted, pval, pco, lval, lco, rval, rco, &
      tval, tco, found)
    INTEGER, INTENT(IN) :: wanted
    TYPE(eis_stack_element), INTENT(IN) :: pval, lval, rval
    TYPE(eis_stack_co_element), INTENT(IN) :: pco, lco, rco
    TYPE(eis_stack_element), INTENT(OUT) :: tval
    TYPE(eis_stack_co_element), INTENT(OUT) :: tco
    !> Whether the operator was found
    LOGICAL, INTENT(OUT) :: found
    CHARACTER(LEN=1) :: name

    found = .TRUE.
    IF (pval%rtype == wanted) THEN
      tval = pval
      tco = pco
    ELSE IF (lval%rtype == wanted) THEN
      tval = lval
      tco = lco
    ELSE IF (rval%rtype == wanted) THEN
      tval = rval
      tco = rco
    ELSE
      !The expression does not contain the operator so take it from the
      !global registry
      found = .FALSE.
      name = eis_operator_name(wanted)
      IF (name == ' ') RETURN
      CALL global_registry%fill_block(name, tval, tco, .FALSE.)
      IF (tval%ptype /= eis_pt_operator) RETURN
      IF (ALLOCATED(tco%text)) DEALLOCATE(tco%text)
      ALLOCATE(tco%text, SOURCE = name)
      tco%registry_type = eis_registry_global
      found = .TRUE.
    END IF

  END SUBROUTINE eis_pick_op_template



  !> @brief
  !> Name that a binary operator is registered under. A blank if the type is
  !> not one of the binary + - * /
  !> @param[in] rtype - Type of the operator
  !> @return name
  FUNCTION eis_operator_name(rtype) RESULT(name)
    INTEGER, INTENT(IN) :: rtype
    CHARACTER(LEN=1) :: name

    IF (rtype == eis_pt_op_plus) THEN
      name = '+'
    ELSE IF (rtype == eis_pt_op_minus) THEN
      name = '-'
    ELSE IF (rtype == eis_pt_op_multiply) THEN
      name = '*'
    ELSE IF (rtype == eis_pt_op_divide) THEN
      name = '/'
    ELSE
      name = ' '
    END IF

  END FUNCTION eis_operator_name



  !> @brief
  !> Reorder a node that is a binary +, -, * or / when both of its operands
  !> are operators from the same pair that each have a constant on one side.
  !> The two constants are moved into a single subtree that the simplifier
  !> then combines. For example (x + 1) + (y + 2) becomes (x + y) + (1 + 2)
  !> and (x - 1) - (y - 2) becomes (x - y) - (1 - 2)
  !> An operator that the reordering needs but that is not in the expression
  !> is taken from the global registry. All other nodes are copied unchanged.
  !> If the global registry does not hold a needed operator then the node is
  !> left alone
  !> @param[inout] tree - Node to reorder
  !> @return hoisted - Whether the node was reordered
  FUNCTION eis_hoist_constants_pair(tree) RESULT(hoisted)
    TYPE(eis_tree_item), INTENT(INOUT) :: tree
    LOGICAL :: hoisted
    INTEGER :: oper, inver, prt, option, s, kop, xop, fop
    INTEGER, DIMENSION(2) :: sx, sk
    LOGICAL :: ok, both_neg, first_positive
    TYPE(eis_tree_item), POINTER :: lc, rc, x0, x1, k0, k1, ka, kb, xa, xb
    TYPE(eis_tree_item), POINTER :: kc, xp, t1, lsrc, rsrc
    TYPE(eis_tree_item), DIMENSION(:), POINTER :: new_nodes, old_nodes
    TYPE(eis_stack_element) :: pval, lval, rval, kval, xval, fval
    TYPE(eis_stack_co_element) :: pco, lco, rco, kco, xco, fco

    hoisted = .FALSE.

    !oper is the commutative operator of the pair, inver is its inverse
    IF (tree%value%rtype == eis_pt_op_plus &
        .OR. tree%value%rtype == eis_pt_op_minus) THEN
      oper = eis_pt_op_plus
      inver = eis_pt_op_minus
    ELSE IF (tree%value%rtype == eis_pt_op_multiply &
        .OR. tree%value%rtype == eis_pt_op_divide) THEN
      oper = eis_pt_op_multiply
      inver = eis_pt_op_divide
    ELSE
      RETURN
    END IF
    IF (tree%value%ptype /= eis_pt_operator) RETURN
    IF (.NOT. tree%value%can_simplify) RETURN
    IF (.NOT. ASSOCIATED(tree%nodes)) RETURN
    IF (SIZE(tree%nodes) /= 2) RETURN

    !Node 2 is the left operand and node 1 is the right operand
    prt = tree%value%rtype
    lc => tree%nodes(2)
    rc => tree%nodes(1)
    CALL eis_split_const_node(lc, oper, inver, ok, x0, k0, sx(1), sk(1))
    IF (.NOT. ok) RETURN
    CALL eis_split_const_node(rc, oper, inver, ok, x1, k1, sx(2), sk(2))
    IF (.NOT. ok) RETURN
    !The right operand is subtracted or divided by when this node is
    IF (prt == inver) THEN
      sx(2) = -sx(2)
      sk(2) = -sk(2)
    END IF

    !Both non constant parts being subtracted can only be written without a
    !unary minus if the constant is added
    both_neg = sx(1) < 0 .AND. sx(2) < 0

    !The constants can be combined in two ways if they have different signs
    DO option = 0, 1
      xa => x0
      xb => x1
      IF (sk(1) == sk(2)) THEN
        IF (option == 1) EXIT
        kop = oper
        ka => k0
        kb => k1
        s = sk(1)
      ELSE
        first_positive = (sk(1) > 0)
        kop = inver
        IF (option == 0) THEN
          s = 1
          IF (first_positive) THEN
            ka => k0
            kb => k1
          ELSE
            ka => k1
            kb => k0
          END IF
        ELSE
          s = -1
          IF (first_positive) THEN
            ka => k1
            kb => k0
          ELSE
            ka => k0
            kb => k1
          END IF
        END IF
      END IF
      IF (both_neg .AND. s < 0) CYCLE

      IF (both_neg) THEN
        xop = inver
        fop = inver
      ELSE
        IF (sx(1) > 0 .AND. sx(2) > 0) THEN
          xop = oper
        ELSE
          xop = inver
          IF (sx(1) < 0) THEN
            xa => x1
            xb => x0
          END IF
        END IF
        IF (s > 0) THEN
          fop = oper
        ELSE
          fop = inver
        END IF
      END IF
      !Get the operators from the nodes that hold them, or from the global
      !registry if the expression does not contain them
      pval = tree%value
      pco = tree%co_value
      lval = lc%value
      lco = lc%co_value
      rval = rc%value
      rco = rc%co_value
      CALL eis_pick_op_template(kop, pval, pco, lval, lco, rval, rco, kval, &
          kco, ok)
      IF (.NOT. ok) CYCLE
      CALL eis_pick_op_template(xop, pval, pco, lval, lco, rval, rco, xval, &
          xco, ok)
      IF (.NOT. ok) CYCLE
      CALL eis_pick_op_template(fop, pval, pco, lval, lco, rval, rco, fval, &
          fco, ok)
      IF (.NOT. ok) CYCLE

      ALLOCATE(kc)
      CALL eis_make_op_node(kc, kop, kval, kco, kval, kco, ka, kb)
      IF (both_neg) THEN
        ALLOCATE(t1)
        CALL eis_make_op_node(t1, fop, fval, fco, fval, fco, kc, x0)
        lsrc => t1
        rsrc => x1
      ELSE
        ALLOCATE(xp)
        CALL eis_make_op_node(xp, xop, xval, xco, xval, xco, xa, xb)
        lsrc => xp
        rsrc => kc
      END IF

      ALLOCATE(new_nodes(2))
      CALL eis_copy_tree(lsrc, new_nodes(2))
      CALL eis_copy_tree(rsrc, new_nodes(1))
      tree%value = fval
      tree%co_value = fco
      old_nodes => tree%nodes
      tree%nodes => new_nodes
      DEALLOCATE(old_nodes)
      DEALLOCATE(kc)
      IF (both_neg) THEN
        DEALLOCATE(t1)
      ELSE
        DEALLOCATE(xp)
      END IF
      hoisted = .TRUE.
      RETURN
    END DO

  END FUNCTION eis_hoist_constants_pair



  !> @brief
  !> Can a node be merged with an identical node that appears elsewhere in an
  !> expression. Variables can because their value is fixed during an
  !> evaluation. Functions and operators can if they are marked as idempotent
  !> and all of their parameters can be merged. Nodes without text have lost
  !> the information needed to tell them apart
  !> @param[in] node - Node to test
  !> @return mergeable
  RECURSIVE FUNCTION eis_is_mergeable(node) RESULT(mergeable)
    TYPE(eis_tree_item), INTENT(IN) :: node
    LOGICAL :: mergeable
    INTEGER :: inode

    mergeable = .FALSE.
    IF (.NOT. ALLOCATED(node%co_value%text)) RETURN
    IF (LEN(node%co_value%text) == 0) RETURN

    SELECT CASE (node%value%ptype)
      CASE (eis_pt_variable)
        mergeable = .TRUE.
      CASE (eis_pt_constant)
        mergeable = node%value%can_simplify
      CASE (eis_pt_function, eis_pt_operator)
        IF (.NOT. node%co_value%is_idempotent) RETURN
        mergeable = .TRUE.
        IF (ASSOCIATED(node%nodes)) THEN
          DO inode = 1, SIZE(node%nodes)
            IF (.NOT. eis_is_mergeable(node%nodes(inode))) THEN
              mergeable = .FALSE.
              RETURN
            END IF
          END DO
        END IF
    END SELECT

  END FUNCTION eis_is_mergeable



  !> @brief
  !> Are two trees the same expression. Nodes are the same if they are the
  !> same kind of symbol from the same registry with the same name
  !> @param[in] a - First tree
  !> @param[in] b - Second tree
  !> @return equal
  RECURSIVE FUNCTION eis_trees_equal(a, b) RESULT(equal)
    TYPE(eis_tree_item), INTENT(IN) :: a, b
    LOGICAL :: equal
    INTEGER :: inode, na, nb

    equal = .FALSE.
    IF (a%value%ptype /= b%value%ptype) RETURN
    IF (a%value%rtype /= b%value%rtype) RETURN
    IF (a%value%value /= b%value%value) RETURN
    IF (a%co_value%registry_type /= b%co_value%registry_type) RETURN
    IF (.NOT. ALLOCATED(a%co_value%text)) RETURN
    IF (.NOT. ALLOCATED(b%co_value%text)) RETURN
    IF (a%co_value%text /= b%co_value%text) RETURN
    na = 0
    nb = 0
    IF (ASSOCIATED(a%nodes)) na = SIZE(a%nodes)
    IF (ASSOCIATED(b%nodes)) nb = SIZE(b%nodes)
    IF (na /= nb) RETURN
    DO inode = 1, na
      IF (.NOT. eis_trees_equal(a%nodes(inode), b%nodes(inode))) RETURN
    END DO
    equal = .TRUE.

  END FUNCTION eis_trees_equal



  !> @brief
  !> Is a node a binary operator from a pair of operators that can be part of a
  !> chain
  !> @param[in] node - Node to test
  !> @param[in] oper - Commutative operator of the pair
  !> @param[in] inver - Inverse operator of the pair
  !> @return is_chain
  FUNCTION eis_is_chain_node(node, oper, inver) RESULT(is_chain)
    TYPE(eis_tree_item), INTENT(IN) :: node
    INTEGER, INTENT(IN) :: oper, inver
    LOGICAL :: is_chain

    is_chain = .FALSE.
    IF (node%value%ptype /= eis_pt_operator) RETURN
    IF (.NOT. node%value%can_simplify) RETURN
    IF (node%value%rtype /= oper .AND. node%value%rtype /= inver) RETURN
    IF (.NOT. ASSOCIATED(node%nodes)) RETURN
    is_chain = (SIZE(node%nodes) == 2)

  END FUNCTION eis_is_chain_node



  !> @brief
  !> Count the terms in a chain of operators from a pair
  !> @param[in] node - Head of the chain
  !> @param[in] oper - Commutative operator of the pair
  !> @param[in] inver - Inverse operator of the pair
  !> @return n
  RECURSIVE FUNCTION eis_chain_count(node, oper, inver) RESULT(n)
    TYPE(eis_tree_item), INTENT(IN) :: node
    INTEGER, INTENT(IN) :: oper, inver
    INTEGER :: n

    IF (eis_is_chain_node(node, oper, inver)) THEN
      n = eis_chain_count(node%nodes(2), oper, inver) &
          + eis_chain_count(node%nodes(1), oper, inver)
    ELSE
      n = 1
    END IF

  END FUNCTION eis_chain_count



  !> @brief
  !> Fill a list of the terms in a chain of operators from a pair, from left to
  !> right
  !> @param[inout] node - Head of the chain
  !> @param[in] oper - Commutative operator of the pair
  !> @param[in] inver - Inverse operator of the pair
  !> @param[in] sign - 1 or -1 depending on whether this part of the chain is
  !> added or subtracted
  !> @param[inout] terms - List of terms
  !> @param[inout] n - Number of terms in the list
  RECURSIVE SUBROUTINE eis_chain_fill(node, oper, inver, sign, terms, n)
    TYPE(eis_tree_item), INTENT(INOUT), TARGET :: node
    INTEGER, INTENT(IN) :: oper, inver
    REAL(eis_num), INTENT(IN) :: sign
    TYPE(eis_chain_term), DIMENSION(:), INTENT(INOUT) :: terms
    INTEGER, INTENT(INOUT) :: n
    REAL(eis_num) :: right_sign

    IF (eis_is_chain_node(node, oper, inver)) THEN
      right_sign = sign
      IF (node%value%rtype == inver) right_sign = -sign
      CALL eis_chain_fill(node%nodes(2), oper, inver, sign, terms, n)
      CALL eis_chain_fill(node%nodes(1), oper, inver, right_sign, terms, n)
    ELSE
      n = n + 1
      terms(n)%node => node
      terms(n)%coefficient = sign
    END IF

  END SUBROUTINE eis_chain_fill



  !> @brief
  !> Make a constant node
  !> @param[inout] node - Node to fill
  !> @param[in] number - Value of the constant
  !> @param[in] rtype - Type of the constant
  !> @param[in] text - Text of the constant
  SUBROUTINE eis_make_constant(node, number, rtype, text)
    TYPE(eis_tree_item), INTENT(INOUT) :: node
    REAL(eis_num), INTENT(IN) :: number
    INTEGER, INTENT(IN) :: rtype
    CHARACTER(LEN=*), INTENT(IN) :: text

    node%value%ptype = eis_pt_constant
    node%value%rtype = rtype
    node%value%value = 0
    node%value%numerical_data = number
    node%value%can_simplify = .TRUE.
    IF (ALLOCATED(node%co_value%text)) DEALLOCATE(node%co_value%text)
    ALLOCATE(node%co_value%text, SOURCE = text)

  END SUBROUTINE eis_make_constant



  !> @brief
  !> Split a term of a chain of + and - into the part that is not constant and
  !> the constant if it is of the form f * constant and f can be merged
  !> @param[inout] node - Term to split
  !> @param[out] base - The part that is not constant, or the term itself if it
  !> cannot be split
  !> @param[inout] coefficient - Coefficient of the term. Multiplied by the
  !> constant if the term is split
  SUBROUTINE eis_split_coefficient(node, base, coefficient)
    TYPE(eis_tree_item), POINTER :: node, base
    REAL(eis_num), INTENT(INOUT) :: coefficient
    TYPE(eis_tree_item), POINTER :: left, right
    LOGICAL :: left_const, right_const

    base => node
    IF (node%value%ptype /= eis_pt_operator) RETURN
    IF (node%value%rtype /= eis_pt_op_multiply) RETURN
    IF (.NOT. node%value%can_simplify) RETURN
    IF (.NOT. ASSOCIATED(node%nodes)) RETURN
    IF (SIZE(node%nodes) /= 2) RETURN

    left => node%nodes(2)
    right => node%nodes(1)
    left_const = eis_is_hoistable_constant(left)
    right_const = eis_is_hoistable_constant(right)
    IF (left_const .EQV. right_const) RETURN

    IF (right_const) THEN
      IF (left%value%ptype == eis_pt_constant) RETURN
      IF (.NOT. eis_is_mergeable(left)) RETURN
      base => left
      coefficient = coefficient * right%value%numerical_data
    ELSE
      IF (right%value%ptype == eis_pt_constant) RETURN
      IF (.NOT. eis_is_mergeable(right)) RETURN
      base => right
      coefficient = coefficient * left%value%numerical_data
    END IF

  END SUBROUTINE eis_split_coefficient



  !> @brief
  !> Make the term for an item of a chain, multiplied by the magnitude of its
  !> coefficient if that is not 1
  !> @param[inout] node - Node to fill
  !> @param[in] item - Item to make the term for
  !> @param[in] times_val - Element for the * operator
  !> @param[in] times_co - Co element for the * operator
  SUBROUTINE eis_make_term(node, item, times_val, times_co)
    TYPE(eis_tree_item), INTENT(INOUT) :: node
    TYPE(eis_chain_term), INTENT(IN) :: item
    TYPE(eis_stack_element), INTENT(IN) :: times_val
    TYPE(eis_stack_co_element), INTENT(IN) :: times_co
    TYPE(eis_tree_item), POINTER :: factor, product
    REAL(eis_num) :: magnitude
    CHARACTER(LEN=30) :: rstring

    CALL eis_copy_tree(item%node, node)
    magnitude = ABS(item%coefficient)
    IF (magnitude /= 1.0_eis_num) THEN
      WRITE(rstring, '(ES30.17)') magnitude
      ALLOCATE(factor)
      CALL eis_make_constant(factor, magnitude, eis_pt_constant, &
          TRIM(ADJUSTL(rstring)))
      ALLOCATE(product)
      CALL eis_make_op_node(product, eis_pt_op_multiply, times_val, times_co, &
          times_val, times_co, node, factor)
      CALL eis_copy_tree(product, node)
      DEALLOCATE(factor)
      DEALLOCATE(product)
    END IF

  END SUBROUTINE eis_make_term



  !> @brief
  !> Collect repeated terms in a chain of + and - into a single term that is
  !> multiplied by the number of repeats. For example x + y + x becomes
  !> x * 2 + y and x - x + y becomes y. Terms are only collected if they
  !> can be merged: variables, or idempotent functions and operators that only
  !> have parameters that can be merged. Constants are left for the other
  !> simplifications. The chain is rewritten only if it ends up with fewer
  !> terms and starts with a term that is added, so that no unary minus is
  !> needed
  !> @param[inout] tree - Node to rewrite
  !> @return collected - Whether the node was rewritten
  FUNCTION eis_collect_like_terms(tree) RESULT(collected)
    TYPE(eis_tree_item), INTENT(INOUT) :: tree
    LOGICAL :: collected
    TYPE(eis_chain_term), DIMENSION(:), ALLOCATABLE :: terms, items
    INTEGER :: nterms, nfilled, nitems, i, j, head, needed
    REAL(eis_num) :: coefficient
    TYPE(eis_tree_item), POINTER :: base, result, term, newnode
    LOGICAL :: merged, ok, all_cancel, needs_times
    TYPE(eis_stack_element) :: plus_val, minus_val, times_val
    TYPE(eis_stack_co_element) :: plus_co, minus_co, times_co

    collected = .FALSE.
    IF (.NOT. eis_is_chain_node(tree, eis_pt_op_plus, eis_pt_op_minus)) RETURN

    !Flatten the chain into a list of terms, left to right, each with the sign
    !that it is added with
    nterms = eis_chain_count(tree, eis_pt_op_plus, eis_pt_op_minus)
    ALLOCATE(terms(nterms), items(nterms))
    nfilled = 0
    CALL eis_chain_fill(tree, eis_pt_op_plus, eis_pt_op_minus, 1.0_eis_num, &
        terms, nfilled)

    !Split terms of the form f * constant into f and the constant, then
    !combine identical terms
    nitems = 0
    DO i = 1, nterms
      coefficient = terms(i)%coefficient
      CALL eis_split_coefficient(terms(i)%node, base, coefficient)
      merged = .FALSE.
      IF (base%value%ptype /= eis_pt_constant) THEN
        IF (eis_is_mergeable(base)) THEN
          DO j = 1, nitems
            IF (items(j)%node%value%ptype == eis_pt_constant) CYCLE
            IF (.NOT. eis_is_mergeable(items(j)%node)) CYCLE
            IF (eis_trees_equal(items(j)%node, base)) THEN
              items(j)%coefficient = items(j)%coefficient + coefficient
              merged = .TRUE.
              EXIT
            END IF
          END DO
        END IF
      END IF
      IF (.NOT. merged) THEN
        nitems = nitems + 1
        items(nitems)%node => base
        items(nitems)%coefficient = coefficient
      END IF
    END DO
    IF (nitems >= nterms) RETURN

    !The first term must be added
    head = 0
    needed = 0
    needs_times = .FALSE.
    DO i = 1, nitems
      IF (items(i)%coefficient /= 0.0_eis_num) needed = needed + 1
      IF (head == 0 .AND. items(i)%coefficient > 0.0_eis_num) head = i
      IF (items(i)%coefficient /= 0.0_eis_num &
          .AND. ABS(items(i)%coefficient) /= 1.0_eis_num) needs_times = .TRUE.
    END DO
    all_cancel = (needed == 0)
    IF (.NOT. all_cancel .AND. head == 0) RETURN

    !Get the operators that the new expression needs
    CALL eis_pick_op_template(eis_pt_op_plus, tree%value, tree%co_value, &
        tree%value, tree%co_value, tree%value, tree%co_value, plus_val, &
        plus_co, ok)
    IF (.NOT. ok) RETURN
    CALL eis_pick_op_template(eis_pt_op_minus, tree%value, tree%co_value, &
        tree%value, tree%co_value, tree%value, tree%co_value, minus_val, &
        minus_co, ok)
    IF (.NOT. ok) RETURN
    IF (needs_times) THEN
      CALL eis_pick_op_template(eis_pt_op_multiply, tree%value, &
          tree%co_value, tree%value, tree%co_value, tree%value, &
          tree%co_value, times_val, times_co, ok)
      IF (.NOT. ok) RETURN
    END IF

    ALLOCATE(result)
    IF (all_cancel) THEN
      CALL eis_make_constant(result, 0.0_eis_num, eis_pt_zero, "0")
    ELSE
      CALL eis_make_term(result, items(head), times_val, times_co)
      DO i = 1, nitems
        IF (i == head .OR. items(i)%coefficient == 0.0_eis_num) CYCLE
        ALLOCATE(term)
        CALL eis_make_term(term, items(i), times_val, times_co)
        ALLOCATE(newnode)
        IF (items(i)%coefficient > 0.0_eis_num) THEN
          CALL eis_make_op_node(newnode, eis_pt_op_plus, plus_val, plus_co, &
              plus_val, plus_co, result, term)
        ELSE
          CALL eis_make_op_node(newnode, eis_pt_op_minus, minus_val, &
              minus_co, minus_val, minus_co, result, term)
        END IF
        DEALLOCATE(result)
        DEALLOCATE(term)
        result => newnode
      END DO
    END IF
    CALL eis_copy_tree(result, tree)
    DEALLOCATE(result)
    collected = .TRUE.

  END FUNCTION eis_collect_like_terms



  !> @brief
  !> Cancel factors that appear both multiplied and divided by in a chain of *
  !> and /. For example x * y / x becomes y and x * x / x becomes x. Factors
  !> are only cancelled if they can be merged: variables, or idempotent
  !> functions and operators that only have parameters that can be merged.
  !> Factors that are repeated but do not cancel are left alone. If only
  !> divisors remain then the chain starts with 1
  !> Note that cancelling a factor that is zero removes the maths domain error
  !> that dividing by it would have caused
  !> @param[inout] tree - Node to rewrite
  !> @return cancelled - Whether the node was rewritten
  FUNCTION eis_cancel_factors(tree) RESULT(cancelled)
    TYPE(eis_tree_item), INTENT(INOUT) :: tree
    LOGICAL :: cancelled
    TYPE(eis_chain_term), DIMENSION(:), ALLOCATABLE :: terms
    LOGICAL, DIMENSION(:), ALLOCATABLE :: removed
    INTEGER :: nterms, nfilled, i, j, head, remaining
    TYPE(eis_tree_item), POINTER :: result, term, newnode
    LOGICAL :: ok
    TYPE(eis_stack_element) :: times_val, divide_val
    TYPE(eis_stack_co_element) :: times_co, divide_co

    cancelled = .FALSE.
    IF (.NOT. eis_is_chain_node(tree, eis_pt_op_multiply, eis_pt_op_divide)) &
        RETURN

    !Flatten the chain into a list of factors, left to right, each with
    !whether it is multiplied by or divided by
    nterms = eis_chain_count(tree, eis_pt_op_multiply, eis_pt_op_divide)
    ALLOCATE(terms(nterms), removed(nterms))
    nfilled = 0
    CALL eis_chain_fill(tree, eis_pt_op_multiply, eis_pt_op_divide, &
        1.0_eis_num, terms, nfilled)

    !Cancel each factor with a later identical factor that is the opposite way
    !round
    removed = .FALSE.
    DO i = 1, nterms
      IF (removed(i)) CYCLE
      IF (terms(i)%node%value%ptype == eis_pt_constant) CYCLE
      IF (.NOT. eis_is_mergeable(terms(i)%node)) CYCLE
      DO j = i + 1, nterms
        IF (removed(j)) CYCLE
        IF (terms(j)%coefficient /= -terms(i)%coefficient) CYCLE
        IF (.NOT. eis_is_mergeable(terms(j)%node)) CYCLE
        IF (eis_trees_equal(terms(i)%node, terms(j)%node)) THEN
          removed(i) = .TRUE.
          removed(j) = .TRUE.
          cancelled = .TRUE.
          EXIT
        END IF
      END DO
    END DO
    IF (.NOT. cancelled) RETURN
    cancelled = .FALSE.

    !Find the factors that remain and what the chain starts with
    head = 0
    remaining = 0
    DO i = 1, nterms
      IF (removed(i)) CYCLE
      remaining = remaining + 1
      IF (head == 0 .AND. terms(i)%coefficient > 0.0_eis_num) head = i
    END DO

    !Get the operators that the new expression needs
    CALL eis_pick_op_template(eis_pt_op_multiply, tree%value, tree%co_value, &
        tree%value, tree%co_value, tree%value, tree%co_value, times_val, &
        times_co, ok)
    IF (.NOT. ok) RETURN
    CALL eis_pick_op_template(eis_pt_op_divide, tree%value, tree%co_value, &
        tree%value, tree%co_value, tree%value, tree%co_value, divide_val, &
        divide_co, ok)
    IF (.NOT. ok) RETURN

    ALLOCATE(result)
    IF (remaining == 0 .OR. head == 0) THEN
      CALL eis_make_constant(result, 1.0_eis_num, eis_pt_unity, "1")
    ELSE
      CALL eis_copy_tree(terms(head)%node, result)
    END IF
    DO i = 1, nterms
      IF (removed(i) .OR. i == head) CYCLE
      ALLOCATE(term)
      CALL eis_copy_tree(terms(i)%node, term)
      ALLOCATE(newnode)
      IF (terms(i)%coefficient > 0.0_eis_num) THEN
        CALL eis_make_op_node(newnode, eis_pt_op_multiply, times_val, &
            times_co, times_val, times_co, result, term)
      ELSE
        CALL eis_make_op_node(newnode, eis_pt_op_divide, divide_val, &
            divide_co, divide_val, divide_co, result, term)
      END IF
      DEALLOCATE(result)
      DEALLOCATE(term)
      result => newnode
    END DO
    CALL eis_copy_tree(result, tree)
    DEALLOCATE(result)
    cancelled = .TRUE.

  END FUNCTION eis_cancel_factors



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
      filename, line_number, full_line, maths_fail_quickly) &
      RESULT(can_simplify)
    TYPE(eis_tree_item), INTENT(INOUT) :: tree
    TYPE(C_PTR), INTENT(IN) :: params
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: filename
    INTEGER, INTENT(IN), OPTIONAL :: line_number
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: full_line
    !> Report maths domain errors during simplification. Optional, default
    !> .FALSE. which replaces an expression that fails with a maths domain
    !> error with a function that raises the error when it is evaluated
    LOGICAL, INTENT(IN), OPTIONAL :: maths_fail_quickly
    INTEGER :: inode
    LOGICAL :: can_simplify, has_nodes, simp1, fail_quickly
    REAL(eis_num) :: res
    TYPE(eis_eval_stack) :: eval
    INTEGER(eis_error) :: err
    INTEGER(eis_status) :: status
    CHARACTER(LEN=30) :: rstring
    TYPE(eis_tree_item), DIMENSION(:), POINTER :: tdata

    status = eis_status_none

    fail_quickly = .FALSE.
    IF (PRESENT(maths_fail_quickly)) fail_quickly = maths_fail_quickly

    has_nodes = ASSOCIATED(tree%nodes)
    IF (has_nodes) has_nodes = has_nodes .AND. (SIZE(tree%nodes) > 0)
    can_simplify = tree%value%can_simplify

    IF (has_nodes) THEN
      DO inode = SIZE(tree%nodes), 1, -1
        simp1 = eis_simplify_tree(tree%nodes(inode), params, errcode, &
            err_handler, filename, line_number, full_line, maths_fail_quickly)
        can_simplify = can_simplify .AND. simp1
      END DO
    END IF

#ifndef EIS_NO_ADVANCED_SIMPLIFY
    !Move constants together so that they can be simplified and collect or
    !cancel repeated terms. The rewritten tree is then simplified again
    IF (tree%value%ptype == eis_pt_operator &
        .AND. errcode == eis_err_none) THEN
      IF (eis_hoist_constants(tree)) THEN
        can_simplify = eis_simplify_tree(tree, params, errcode, err_handler, &
            filename, line_number, full_line, maths_fail_quickly)
        RETURN
      END IF
      IF (eis_hoist_constants_pair(tree)) THEN
        can_simplify = eis_simplify_tree(tree, params, errcode, err_handler, &
            filename, line_number, full_line, maths_fail_quickly)
        RETURN
      END IF
      IF (eis_collect_like_terms(tree)) THEN
        can_simplify = eis_simplify_tree(tree, params, errcode, err_handler, &
            filename, line_number, full_line, maths_fail_quickly)
        RETURN
      END IF
      IF (eis_cancel_factors(tree)) THEN
        can_simplify = eis_simplify_tree(tree, params, errcode, err_handler, &
            filename, line_number, full_line, maths_fail_quickly)
        RETURN
      END IF
    END IF
#endif

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
          IF (fail_quickly) THEN
            errcode = eis_err_maths_domain
            RETURN
          END IF
          tree%value%ptype = eis_pt_function
          tree%value%rtype = eis_pt_function
          tree%value%eval_fn => eis_maths_domain_error
          IF (ALLOCATED(tree%co_value%text)) DEALLOCATE(tree%co_value%text)
          ALLOCATE(tree%co_value%text, SOURCE = 'MATHS DOMAIN ERROR')
          tree%value%actual_params = 0
          tree%value%can_simplify = .FALSE.
          DEALLOCATE(tree%nodes)
          can_simplify = .FALSE.
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
          can_simplify =.FALSE.
          RETURN
        ELSE IF (tree%nodes(1)%value%ptype == eis_pt_operator &
            .AND. tree%nodes(1)%value%rtype == eis_pt_op_unary_minus) THEN
          !Simplify -(-f)
          tdata => tree%nodes
          tree%nodes => NULL()
          CALL eis_copy_tree(tdata(1)%nodes(1), tree)
          DEALLOCATE(tdata)
          can_simplify = tree%value%ptype == eis_pt_constant &
              .AND. tree%value%can_simplify
          RETURN
        END IF
      ELSE IF (tree%value%rtype == eis_pt_op_unary_plus) THEN
        !Simplify +f
        tdata => tree%nodes
        tree%nodes => NULL()
        CALL eis_copy_tree(tdata(1), tree)
        DEALLOCATE(tdata)
        can_simplify = tree%value%ptype == eis_pt_constant &
            .AND. tree%value%can_simplify
        RETURN
      ELSE IF (tree%value%rtype == eis_pt_op_power) THEN
        IF (tree%nodes(1)%value%rtype == eis_pt_zero &
            .OR. tree%nodes(2)%value%rtype == eis_pt_unity) THEN
          !Simplify f^0 and 1^f
          tree%value%ptype = eis_pt_constant
          tree%value%rtype = eis_pt_unity
          tree%value%numerical_data = 1.0_eis_num
          tree%co_value%deriv_values = -1
          IF (ALLOCATED(tree%co_value%text)) THEN
            DEALLOCATE(tree%co_value%text)
            ALLOCATE(tree%co_value%text, SOURCE = "1")
          END IF
          DEALLOCATE(tree%nodes)
          can_simplify = .TRUE.
          RETURN
        ELSE IF (tree%nodes(1)%value%rtype == eis_pt_unity) THEN
          !Simplify f^1
          tdata => tree%nodes
          tree%nodes => NULL()
          CALL eis_copy_tree(tdata(2), tree)
          DEALLOCATE(tdata)
          can_simplify = tree%value%ptype == eis_pt_constant &
              .AND. tree%value%can_simplify
          RETURN
        END IF
      END IF
    END IF

    !Simplify an if with a constant condition. The nodes are held in the
    !reverse of the order of the parameters so the condition is last, the
    !first result is next and the second result is first
    IF (tree%value%rtype == eis_pt_control_if .AND. has_nodes) THEN
      IF (SIZE(tree%nodes) == 3) THEN
        IF (eis_is_hoistable_constant(tree%nodes(3))) THEN
          tdata => tree%nodes
          tree%nodes => NULL()
          IF (tdata(3)%value%numerical_data /= 0.0_eis_num) THEN
            CALL eis_copy_tree(tdata(2), tree)
          ELSE
            CALL eis_copy_tree(tdata(1), tree)
          END IF
          DEALLOCATE(tdata)
          can_simplify = tree%value%ptype == eis_pt_constant &
              .AND. tree%value%can_simplify
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
          !A result that is exactly 0 or 1 is flagged as such so that the
          !simplifications for 0 and 1 apply to it
          IF (res == 0.0_eis_num) THEN
            tree%value%rtype = eis_pt_zero
          ELSE IF (res == 1.0_eis_num) THEN
            tree%value%rtype = eis_pt_unity
          END IF
          tree%value%numerical_data = res
          tree%co_value%deriv_values = -1
          IF (ALLOCATED(tree%co_value%text)) THEN
            DEALLOCATE(tree%co_value%text)
            IF (res == 0.0_eis_num) THEN
              ALLOCATE(tree%co_value%text, SOURCE = "0")
            ELSE IF (res == 1.0_eis_num) THEN
              ALLOCATE(tree%co_value%text, SOURCE = "1")
            ELSE
              WRITE(rstring,'(ES30.17)') res
              ALLOCATE(tree%co_value%text, SOURCE = TRIM(ADJUSTL(rstring)))
            END IF
          END IF
          IF (has_nodes) DEALLOCATE(tree%nodes)
        ELSE
          RETURN
        END IF
      ELSE IF (.NOT. fail_quickly .AND. err == eis_err_maths_domain) THEN
        tree%value%ptype = eis_pt_function
        tree%value%rtype = eis_pt_function
        tree%value%eval_fn => eis_maths_domain_error
        IF (ALLOCATED(tree%co_value%text)) DEALLOCATE(tree%co_value%text)
        ALLOCATE(tree%co_value%text, SOURCE = 'MATHS DOMAIN ERROR')
        tree%value%actual_params = 0
        tree%value%can_simplify = .FALSE.
        IF (has_nodes) DEALLOCATE(tree%nodes)
        can_simplify = .FALSE.
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
    INTEGER :: inode, jc, ju
    TYPE(eis_stack_element) :: jump_cond, jump_uncond, join
    TYPE(eis_stack_co_element) :: jump_cond_co, jump_uncond_co
    LOGICAL :: is_if

    is_if = .FALSE.
    IF (tree%value%rtype == eis_pt_control_if .AND. ASSOCIATED(tree%nodes)) &
        is_if = (SIZE(tree%nodes) == 3)

    IF (is_if) THEN
      !Condition, jump over the first branch if false, first branch, jump over
      !the second branch, second branch, join. The parameters are held in
      !the tree in the reverse of the order that they are in the stack
      jump_cond%ptype = eis_pt_jump_conditional
      jump_cond%rtype = eis_pt_jump_conditional
      jump_cond%actual_params = 0
      jump_cond%can_simplify = .FALSE.
      jump_cond_co = tree%co_value
      IF (ALLOCATED(jump_cond_co%text)) DEALLOCATE(jump_cond_co%text)
      ALLOCATE(jump_cond_co%text, SOURCE = 'jump_conditional')
      jump_cond_co%expected_params = 0
      jump_cond_co%cap_bits = 0_eis_bitmask
      jump_cond_co%defer = .FALSE.

      jump_uncond%ptype = eis_pt_jump_unconditional
      jump_uncond%rtype = eis_pt_jump_unconditional
      jump_uncond%actual_params = 0
      jump_uncond%can_simplify = .FALSE.
      jump_uncond_co = tree%co_value
      IF (ALLOCATED(jump_uncond_co%text)) DEALLOCATE(jump_uncond_co%text)
      ALLOCATE(jump_uncond_co%text, SOURCE = 'jump_unconditional')
      jump_uncond_co%expected_params = 0
      jump_uncond_co%cap_bits = 0_eis_bitmask
      jump_uncond_co%defer = .FALSE.

      join = tree%value
      join%rtype = eis_pt_control_if_join
      join%eval_fn => NULL()
      join%functor => NULL()

      CALL eis_tree_to_stack(tree%nodes(3), stack_in)
      CALL push_to_stack(stack_in, jump_cond, jump_cond_co)
      jc = stack_in%stack_point
      CALL eis_tree_to_stack(tree%nodes(2), stack_in)
      CALL push_to_stack(stack_in, jump_uncond, jump_uncond_co)
      ju = stack_in%stack_point
      CALL eis_tree_to_stack(tree%nodes(1), stack_in)
      stack_in%entries(jc)%value = ju - jc
      stack_in%entries(ju)%value = stack_in%stack_point - ju
      CALL push_to_stack(stack_in, join, tree%co_value)
      RETURN
    END IF

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
            IF (node%nodes(2)%co_value%precedence < node%co_value%precedence &
                .OR. (node%nodes(2)%co_value%precedence &
                == node%co_value%precedence &
                .AND. node%co_value%associativity == eis_assoc_ra)) THEN
              bb1 = '('
              ab1 = ')'
            END IF
          END IF
          IF (node%nodes(1)%value%ptype == eis_pt_operator) THEN
            IF (node%nodes(1)%co_value%precedence < node%co_value%precedence &
                .OR. (node%nodes(1)%co_value%precedence &
                == node%co_value%precedence &
                .AND. node%co_value%associativity == eis_assoc_la)) THEN
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
