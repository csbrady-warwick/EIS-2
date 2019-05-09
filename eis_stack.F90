MODULE eis_stack_mod

  USE eis_header
  IMPLICIT NONE

  CONTAINS

  SUBROUTINE initialise_stack(stack)

    TYPE(eis_stack), INTENT(INOUT) :: stack

    stack%stack_point = 0
    stack%stack_size = 1
    stack%cap_bits = 0
    ALLOCATE(stack%entries(stack%stack_size))
    ALLOCATE(stack%co_entries(stack%stack_size))
    CALL initialise_stack_element(stack%entries)
    CALL initialise_stack_co_element(stack%co_entries)
    stack%init = .TRUE.

  END SUBROUTINE initialise_stack



  SUBROUTINE deallocate_stack(stack)

    TYPE(eis_stack), INTENT(INOUT) :: stack

    stack%stack_point = 0
    stack%stack_size = 0
    IF (stack%init) THEN
      CALL deallocate_stack_element(stack%entries)
      IF (ALLOCATED(stack%co_entries)) &
          CALL deallocate_stack_co_element(stack%co_entries)
      DEALLOCATE(stack%entries)
      IF (ALLOCATED(stack%co_entries)) DEALLOCATE(stack%co_entries)
    END IF
    stack%init = .FALSE.

  END SUBROUTINE deallocate_stack



  PURE ELEMENTAL SUBROUTINE deallocate_stack_element(element)
    TYPE(eis_stack_element), INTENT(INOUT) :: element

  END SUBROUTINE deallocate_stack_element



  PURE ELEMENTAL SUBROUTINE deallocate_stack_co_element(coelement)
    TYPE(eis_stack_co_element), INTENT(INOUT) :: coelement

    IF (ALLOCATED(coelement%text)) DEALLOCATE(coelement%text)
  END SUBROUTINE deallocate_stack_co_element




  SUBROUTINE copy_stack(stack, copy)

    TYPE(eis_stack), INTENT(IN) :: stack
    TYPE(eis_stack), INTENT(OUT) :: copy

    copy = stack

  END SUBROUTINE copy_stack




  SUBROUTINE grow_stack(stack, new_elements)
    TYPE(eis_stack), INTENT(INOUT) :: stack
    INTEGER, INTENT(IN) :: new_elements
    TYPE(eis_stack_element), ALLOCATABLE :: new_buffer(:)
    TYPE(eis_stack_co_element), ALLOCATABLE :: new_cobuffer(:)
    INTEGER :: i

    IF (.NOT. stack%init) RETURN
    IF (new_elements < stack%stack_point) RETURN

    ALLOCATE(new_buffer(new_elements))
    new_buffer(1:stack%stack_point) = stack%entries(1:stack%stack_point)
    DEALLOCATE(stack%entries)
    CALL MOVE_ALLOC(new_buffer, stack%entries)
    stack%stack_size = new_elements
    CALL initialise_stack_element(stacK%entries(stack%stack_point+1:&
        stack%stack_size))
    IF (ALLOCATED(stack%co_entries)) THEN
      ALLOCATE(new_cobuffer(new_elements))
      new_cobuffer(1:stack%stack_point) = stack%co_entries(1:stack%stack_point)
      DEALLOCATE(stack%co_entries)
      CALL MOVE_ALLOC(new_cobuffer, stack%co_entries)
      CALL initialise_stack_co_element(stacK%co_entries(stack%stack_point+1:&
          stack%stack_size))
    END IF

  END SUBROUTINE grow_stack



  SUBROUTINE minify_stack(stack)
    TYPE(eis_stack), INTENT(INOUT) :: stack
    INTEGER :: istack

    !Manually deleting strings unnecessary per standard
    !but there are compilers in the wild that fail
    DO istack = 1, stack%stack_point
      IF (ALLOCATED(stack%co_entries(istack)%text)) &
          DEALLOCATE(stack%co_entries(istack)%text)
    END DO

    DEALLOCATE(stack%co_entries)

    CALL grow_stack(stack, stack%stack_point)

  END SUBROUTINE minify_stack



  SUBROUTINE append_stack(stack, append)

    TYPE(eis_stack), INTENT(INOUT) :: stack, append
    INTEGER :: i, n, old_size, old_stack_point

    old_stack_point = stack%stack_point

    IF (stack%stack_point + append%stack_point > stack%stack_size) THEN
      CALL grow_stack(stack, 2 * (stack%stack_point + append%stack_point))
    END IF

    stack%stack_point = old_stack_point + append%stack_point
    n = old_stack_point + 1
    DO i = 1,append%stack_point
      stack%entries(n) = append%entries(i)
      n = n + 1
    END DO

    IF (ALLOCATED(stack%co_entries)) THEN
      n = old_stack_point + 1
      DO i = 1,append%stack_point
        stack%co_entries(n) = append%co_entries(i)
        n = n + 1
      END DO
    END IF
    stack%cap_bits = IOR(stack%cap_bits, append%cap_bits)

  END SUBROUTINE append_stack



  SUBROUTINE push_to_stack(stack, value, co_value)

    TYPE(eis_stack_element), INTENT(IN) :: value
    TYPE(eis_stack_co_element), INTENT(IN), OPTIONAL :: co_value
    TYPE(eis_stack), INTENT(INOUT) :: stack
    INTEGER :: i, old_size

    IF (stack%stack_point + 1 > stack%stack_size) THEN
      CALL grow_stack(stack, 2 * stack%stack_size)
    END IF

    stack%stack_point = stack%stack_point + 1
    stack%entries(stack%stack_point) = value
    IF (PRESENT(co_value) .AND. ALLOCATED(stack%co_entries)) THEN
        stack%co_entries(stack%stack_point) = co_value
    END IF

  END SUBROUTINE push_to_stack



  SUBROUTINE pop_to_stack(stack1, stack2)

    TYPE(eis_stack), INTENT(INOUT) :: stack1, stack2

    IF (ALLOCATED(stack1%co_entries)) THEN
      CALL push_to_stack(stack2, stack1%entries(stack1%stack_point), &
          stack1%co_entries(stack1%stack_point))
    ELSE
      CALL push_to_stack(stack2, stack1%entries(stack1%stack_point))
    END IF
    stack1%stack_point = stack1%stack_point - 1

  END SUBROUTINE pop_to_stack



  SUBROUTINE pop_to_null(stack)

    TYPE(eis_stack), INTENT(INOUT) :: stack

    stack%stack_point = stack%stack_point - 1

  END SUBROUTINE pop_to_null



  SUBROUTINE pop_from_stack(stack, value, covalue)

    TYPE(eis_stack), INTENT(INOUT) :: stack
    TYPE(eis_stack_element), INTENT(OUT) :: value
    TYPE(eis_stack_co_element), INTENT(OUT), OPTIONAL :: covalue

    value = stack%entries(stack%stack_point)
    IF (PRESENT(covalue) .AND. ALLOCATED(stack%co_entries)) THEN
      covalue = stack%co_entries(stack%stack_point)
    END IF
    stack%stack_point = stack%stack_point - 1

  END SUBROUTINE pop_from_stack



  SUBROUTINE stack_snoop(stack, value, offset, covalue)

    TYPE(eis_stack), INTENT(INOUT) :: stack
    TYPE(eis_stack_element), INTENT(OUT) :: value
    INTEGER, INTENT(IN) :: offset
    TYPE(eis_stack_co_element), INTENT(OUT), OPTIONAL :: covalue

    value = stack%entries(stack%stack_point-offset)
    IF (PRESENT(covalue) .AND. ALLOCATED(stack%co_entries)) THEN
      covalue = stack%co_entries(stack%stack_point)
    END IF

  END SUBROUTINE stack_snoop



  PURE ELEMENTAL SUBROUTINE initialise_stack_element(element)

    TYPE(eis_stack_element), INTENT(INOUT) :: element

    element%ptype = 0
    element%value = 0
    element%numerical_data = 0

  END SUBROUTINE initialise_stack_element



  PURE ELEMENTAL SUBROUTINE initialise_stack_co_element(element)

    TYPE(eis_stack_co_element), INTENT(INOUT) :: element

  END SUBROUTINE initialise_stack_co_element



  SUBROUTINE display_tokens_inline(token_list)
    TYPE(eis_stack), INTENT(IN) :: token_list
    INTEGER :: i

    IF (.NOT. ALLOCATED(token_list%co_entries)) RETURN

    DO i = 1, token_list%stack_point
      IF (.NOT. ALLOCATED(token_list%co_entries(i)%text)) CYCLE
      WRITE(*,'(A)', ADVANCE='NO') TRIM(token_list%co_entries(i)%text) // " "
    END DO
    WRITE(*,*) ""
  END SUBROUTINE display_tokens_inline

END MODULE eis_stack_mod
