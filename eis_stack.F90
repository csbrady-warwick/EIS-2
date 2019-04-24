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
    IF (stack%init) DEALLOCATE(stack%entries)
    stack%init = .FALSE.

  END SUBROUTINE deallocate_stack



  SUBROUTINE copy_stack(stack, copy)

    TYPE(eis_stack), INTENT(IN) :: stack
    TYPE(eis_stack), INTENT(OUT) :: copy

    copy = stack
    ALLOCATE(copy%entries(copy%stack_size))
    copy%entries(1:copy%stack_point) = stack%entries(1:copy%stack_point)

  END SUBROUTINE copy_stack



  SUBROUTINE append_stack(stack, append)

    TYPE(eis_stack), INTENT(INOUT) :: stack, append
    TYPE(eis_stack_element), POINTER :: old_buffer(:)
    INTEGER :: i, n, old_size, old_stack_point

    old_stack_point = stack%stack_point
    stack%stack_point = old_stack_point + append%stack_point

    IF (stack%stack_point > stack%stack_size) THEN
      old_size = stack%stack_size
      stack%stack_size = 2 * stack%stack_point
      old_buffer => stack%entries
      ALLOCATE(stack%entries(stack%stack_size))
      stack%entries(1:old_size) = old_buffer(1:old_size)
      DO i = old_stack_point+1,stack%stack_size
        CALL initialise_stack_element(stack%entries(i))
      END DO
      DEALLOCATE(old_buffer)
    END IF

    n = old_stack_point + 1
    DO i = 1,append%stack_point
      stack%entries(n) = append%entries(i)
      n = n + 1
    END DO

    CALL deallocate_stack(append)

  END SUBROUTINE append_stack



  SUBROUTINE push_to_stack(stack, value)

    TYPE(eis_stack_element), INTENT(IN) :: value
    TYPE(eis_stack), INTENT(INOUT) :: stack
    TYPE(eis_stack_element), POINTER :: old_buffer(:)
    INTEGER :: i, old_size

    stack%stack_point = stack%stack_point + 1

    IF (stack%stack_point > stack%stack_size) THEN
      old_size = stack%stack_size
      stack%stack_size = 2 * stack%stack_size
      old_buffer => stack%entries
      ALLOCATE(stack%entries(stack%stack_size))
      stack%entries(1:old_size) = old_buffer(1:old_size)
      DO i = old_size+1,stack%stack_size
        CALL initialise_stack_element(stack%entries(i))
      END DO
      DEALLOCATE(old_buffer)
    END IF

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



  SUBROUTINE set_stack_zero(stack, n_zeros)

    TYPE(eis_stack), INTENT(INOUT) :: stack
    INTEGER, INTENT(IN), OPTIONAL :: n_zeros
    INTEGER :: zmax, iz, errcode

    zmax = 1
    IF (PRESENT(n_zeros)) zmax = n_zeros

    CALL deallocate_stack(stack)
    CALL initialise_stack(stack)
    DO iz = 1, zmax
      CALL tokenize('0', stack, errcode)
    END DO

  END SUBROUTINE set_stack_zero

END MODULE eis_stack_mod
