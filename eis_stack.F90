MODULE eis_stack_mod

  USE eis_header
  IMPLICIT NONE

  CONTAINS

  SUBROUTINE initialise_stack(stack)

    TYPE(eis_stack), INTENT(INOUT) :: stack

    stack%stack_point = 0

    stack%stack_size = 1
    ALLOCATE(stack%entries(stack%stack_size))
    CALL initialise_stack_element(stack%entries(1))
    stack%init = .TRUE.

  END SUBROUTINE initialise_stack



  SUBROUTINE deallocate_stack(stack)

    TYPE(eis_stack), INTENT(INOUT) :: stack

    stack%stack_point = 0
    stack%stack_size = 0
    IF (stack%init) THEN
      CALL deallocate_stack_element(stack%entries)
      DEALLOCATE(stack%entries)
    END IF
    stack%init = .FALSE.

  END SUBROUTINE deallocate_stack



  PURE ELEMENTAL SUBROUTINE deallocate_stack_element(element)
    TYPE(eis_stack_element), INTENT(INOUT) :: element

    IF (ALLOCATED(element%text)) DEALLOCATE(element%text)
  END SUBROUTINE deallocate_stack_element



  SUBROUTINE copy_stack(stack, copy)

    TYPE(eis_stack), INTENT(IN) :: stack
    TYPE(eis_stack), INTENT(OUT) :: copy

    copy = stack

  END SUBROUTINE copy_stack




  SUBROUTINE grow_stack(stack, new_elements)
    TYPE(eis_stack), INTENT(INOUT) :: stack
    INTEGER, INTENT(IN) :: new_elements
    TYPE(eis_stack_element), ALLOCATABLE :: new_buffer(:)
    INTEGER :: i

    IF (.NOT. stack%init) RETURN
    IF (new_elements < stack%stack_point) RETURN

    ALLOCATE(new_buffer(new_elements))
    new_buffer(1:stack%stack_point) = stack%entries(1:stack%stack_point)
    DEALLOCATE(stack%entries)
    CALL MOVE_ALLOC(new_buffer, stack%entries)
    stack%stack_size = new_elements
    DO i = stack%stack_point+1,stack%stack_size
      CALL initialise_stack_element(stack%entries(i))
    END DO

  END SUBROUTINE grow_stack



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

  END SUBROUTINE append_stack



  SUBROUTINE push_to_stack(stack, value)

    TYPE(eis_stack_element), INTENT(IN) :: value
    TYPE(eis_stack), INTENT(INOUT) :: stack
    INTEGER :: i, old_size

    IF (stack%stack_point + 1 > stack%stack_size) THEN
      CALL grow_stack(stack, 2 * stack%stack_size)
    END IF

    stack%stack_point = stack%stack_point + 1
    stack%entries(stack%stack_point) = value

  END SUBROUTINE push_to_stack



  SUBROUTINE pop_to_stack(stack1, stack2)

    TYPE(eis_stack), INTENT(INOUT) :: stack1, stack2

    CALL push_to_stack(stack2, stack1%entries(stack1%stack_point))
    stack1%stack_point = stack1%stack_point - 1

  END SUBROUTINE pop_to_stack



  SUBROUTINE pop_to_null(stack)

    TYPE(eis_stack), INTENT(INOUT) :: stack

    stack%stack_point = stack%stack_point - 1

  END SUBROUTINE pop_to_null



  SUBROUTINE pop_from_stack(stack, value)

    TYPE(eis_stack_element), INTENT(OUT) :: value
    TYPE(eis_stack), INTENT(INOUT) :: stack

    value = stack%entries(stack%stack_point)
    stack%stack_point = stack%stack_point - 1

  END SUBROUTINE pop_from_stack



  SUBROUTINE stack_snoop(stack, value, offset)

    TYPE(eis_stack), INTENT(INOUT) :: stack
    TYPE(eis_stack_element), INTENT(OUT) :: value
    INTEGER, INTENT(IN) :: offset

    IF (stack%stack_point-offset <= 0) THEN
      PRINT *, 'Unable to snoop stack', stack%stack_point
      STOP
    END IF

    value = stack%entries(stack%stack_point-offset)

  END SUBROUTINE stack_snoop



  SUBROUTINE initialise_stack_element(element)

    TYPE(eis_stack_element), INTENT(INOUT) :: element

    element%ptype = 0
    element%value = 0
    element%numerical_data = 0

  END SUBROUTINE initialise_stack_element



  SUBROUTINE display_tokens(token_list)

    TYPE(eis_stack), INTENT(IN) :: token_list
    INTEGER :: i

      DO i = 1, token_list%stack_point
        PRINT *, 'Type', token_list%entries(i)%ptype
        PRINT *, 'Data', token_list%entries(i)%value
        PRINT *, 'NumData', token_list%entries(i)%numerical_data
        PRINT *, 'Text :', TRIM(token_list%entries(i)%text)
        PRINT *, '---------------'
      END DO

  END SUBROUTINE display_tokens



  SUBROUTINE display_tokens_inline(token_list)
    TYPE(eis_stack), INTENT(IN) :: token_list
    INTEGER :: i

    DO i = 1, token_list%stack_point
      WRITE(*,'(A)', ADVANCE='NO') TRIM(token_list%entries(i)%text) // " "
    END DO
    WRITE(*,*) NEW_LINE('A')
  END SUBROUTINE display_tokens_inline

END MODULE eis_stack_mod
