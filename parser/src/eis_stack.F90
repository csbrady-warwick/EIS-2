MODULE eis_stack_mod

  USE eis_header
  USE eis_parser_header
  USE eis_utils
  IMPLICIT NONE

  CONTAINS

  !> @brief
  !> Initialise a stack object
  !> @param[in] stack - Stack to initialise
  SUBROUTINE initialise_stack(stack)

    TYPE(eis_stack), INTENT(INOUT) :: stack

    CALL deallocate_stack(stack)
    stack%stack_size = 1
    stack%cap_bits = 0
    ALLOCATE(stack%entries(stack%stack_size))
    ALLOCATE(stack%co_entries(stack%stack_size))
    CALL initialise_stack_element(stack%entries)
    CALL initialise_stack_co_element(stack%co_entries)
    stack%init = .TRUE.

  END SUBROUTINE initialise_stack


  !> @brief
  !> Deallocate a stack object
  !> @param[in] stack - Stack to deallocate
  SUBROUTINE deallocate_stack(stack)

    TYPE(eis_stack), INTENT(INOUT) :: stack

    stack%stack_point = 0
    stack%stack_size = 0
    NULLIFY(stack%eval_fn)
    IF (stack%init) THEN
      CALL deallocate_stack_element(stack%entries)
      IF (ALLOCATED(stack%co_entries)) &
          CALL deallocate_stack_co_element(stack%co_entries)
      DEALLOCATE(stack%entries)
      IF (ALLOCATED(stack%co_entries)) DEALLOCATE(stack%co_entries)
    END IF
    stack%init = .FALSE.

  END SUBROUTINE deallocate_stack


  !> @brief
  !> Function to deallocate a stack element. Null operator at present
  !> @param[inout] element - Stack element to deallocate
  PURE ELEMENTAL SUBROUTINE deallocate_stack_element(element)
    TYPE(eis_stack_element), INTENT(INOUT) :: element

  END SUBROUTINE deallocate_stack_element


  !> @brief
  !> Function to deallocate a stack coelement.
  !> @param[inout] coelement - Stack coelement to deallocate
  PURE ELEMENTAL SUBROUTINE deallocate_stack_co_element(coelement)
    TYPE(eis_stack_co_element), INTENT(INOUT) :: coelement

    IF (ALLOCATED(coelement%text)) DEALLOCATE(coelement%text)
  END SUBROUTINE deallocate_stack_co_element



  !> @brief
  !> Function to copy a stack. If possible should just be
  !> simple equality so that the user can just do simple equality
  !> EIS core should always use this function just in case this
  !> isn't possible
  !> @param[in] stack - Source stack
  !> @param[in] copy - Destination stack
  SUBROUTINE copy_stack(stack, copy)

    TYPE(eis_stack), INTENT(IN) :: stack
    TYPE(eis_stack), INTENT(OUT) :: copy

    copy = stack

  END SUBROUTINE copy_stack




  !> @brief
  !> Reallocate a stack to have a new size. Cannot be shrunk to a size
  !> below the number of elements in the stack
  !> @param[inout] stack - Stack to grow or shrink
  !> @param[in] new_elements - Number of elements to set the stack size to
  SUBROUTINE grow_stack(stack, new_elements)
    TYPE(eis_stack), INTENT(INOUT) :: stack
    INTEGER, INTENT(IN) :: new_elements
    TYPE(eis_stack_element), ALLOCATABLE :: new_buffer(:)
    TYPE(eis_stack_co_element), ALLOCATABLE :: new_cobuffer(:)

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



  !> @brief
  !> Reduce a stack to be as small as possible while still functioning
  !> @param[inout] stack - Stack to minify
  SUBROUTINE minify_stack(stack)
    TYPE(eis_stack), INTENT(INOUT) :: stack
    INTEGER :: istack

    IF (stack%has_deferred) RETURN

    !Manually deleting strings unnecessary per standard
    !but there are compilers in the wild that fail
    DO istack = 1, stack%stack_point
      IF (ALLOCATED(stack%co_entries(istack)%text)) &
          DEALLOCATE(stack%co_entries(istack)%text)
    END DO

    DEALLOCATE(stack%co_entries)
    DEALLOCATE(stack%full_line)

    CALL grow_stack(stack, stack%stack_point)

  END SUBROUTINE minify_stack



  !> @brief
  !> Prepend one stack to the start of another stack.
  !> @param[inout] stack - Stack to prepend to
  !> @param[in] prepend - Stack to prepend to "stack"
  SUBROUTINE prepend_stack(stack, prepend)

    TYPE(eis_stack), INTENT(INOUT) :: stack, prepend
    INTEGER :: i, n, old_stack_point

    old_stack_point = stack%stack_point

    IF (stack%stack_point + prepend%stack_point > stack%stack_size) THEN
      CALL grow_stack(stack, 2 * (stack%stack_point + prepend%stack_point))
    END IF

    stack%stack_point = old_stack_point + prepend%stack_point
    stack%entries(prepend%stack_point+1:prepend%stack_point + old_stack_point) &
        = stack%entries(1:old_stack_point)
    stack%entries(1:prepend%stack_point) &
        = prepend%entries(1:prepend%stack_point)

    IF (ALLOCATED(stack%co_entries)) THEN
    stack%co_entries(prepend%stack_point+1:prepend%stack_point &
        + old_stack_point) = stack%co_entries(1:old_stack_point)
    stack%co_entries(1:prepend%stack_point) &
        = prepend%co_entries(1:prepend%stack_point)
    END IF
    stack%cap_bits = IOR(stack%cap_bits, prepend%cap_bits)

  END SUBROUTINE prepend_stack



  !> @brief
  !> Append one stack to the end of another stack.
  !> @param[inout] stack - Stack to append to
  !> @param[in] append - Stack to append to "stack"
  SUBROUTINE append_stack(stack, append)

    TYPE(eis_stack), INTENT(INOUT) :: stack, append
    INTEGER :: i, n, old_stack_point

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


  !> @brief
  !> Replace a single element of a stack with all of the elements of another
  !> stack. The specified element is removed and replaced.
  !> @param[inout] dest - Stack to replace the item in
  !> @param[in] src - Stack to copy into "dest" at the specified point
  !> @param[in] insert_point - Point in "dest" to replace with "src"
  SUBROUTINE replace_element(dest, src, insert_point)

    TYPE(eis_stack), INTENT(INOUT) :: dest, src
    INTEGER, INTENT(IN) :: insert_point
    INTEGER :: i, n, old_stack_point

    IF (.NOT. dest%init .OR. .NOT. src%init) RETURN


    IF (insert_point > dest%stack_point) THEN
      CALL append_stack(dest, src)
      RETURN
    END IF

    old_stack_point = dest%stack_point

    IF (dest%stack_point + src%stack_point > dest%stack_size) THEN
      CALL grow_stack(dest, 2 * (dest%stack_point + src%stack_point))
    END IF

    dest%entries(insert_point + src%stack_point + 1:&
        dest%stack_point + src%stack_point) &
        = dest%entries(insert_point + 1:dest%stack_point)
    IF (ALLOCATED(dest%co_entries) .AND. ALLOCATED(src%co_entries)) THEN
      dest%co_entries(insert_point + src%stack_point + 1:&
          dest%stack_point + src%stack_point)&
          = dest%co_entries(insert_point + 1:dest%stack_point)
    END IF

    dest%stack_point = old_stack_point + src%stack_point
    n = insert_point
    DO i = 1,src%stack_point
      dest%entries(n) = src%entries(i)
      n = n + 1
    END DO

    IF (ALLOCATED(dest%co_entries) .AND. ALLOCATED(src%co_entries)) THEN
      n = insert_point
      DO i = 1,src%stack_point
        dest%co_entries(n) = src%co_entries(i)
        n = n + 1
      END DO
    END IF
    dest%cap_bits = IOR(dest%cap_bits, src%cap_bits)

  END SUBROUTINE replace_element


  !> @brief
  !> Push a single value onto the stack. Optionally also a covalue
  !> @param[inout] dest - Stack to replace the item in
  !> @param[in] stack - Stack to push the value on to
  !> @param[in] value - Value to push on
  !> @param[in] co_value - Covalue to push on. Optional, default no co_value
  !> If stack cannot store co_values this is ignored if present
  SUBROUTINE push_to_stack(stack, value, co_value)

    TYPE(eis_stack_element), INTENT(IN) :: value
    TYPE(eis_stack_co_element), INTENT(IN), OPTIONAL :: co_value
    TYPE(eis_stack), INTENT(INOUT) :: stack

    IF (stack%stack_point + 1 > stack%stack_size) THEN
      CALL grow_stack(stack, 2 * stack%stack_size)
    END IF

    stack%stack_point = stack%stack_point + 1
    stack%entries(stack%stack_point) = value
    IF (PRESENT(co_value) .AND. ALLOCATED(stack%co_entries)) THEN
        stack%co_entries(stack%stack_point) = co_value
    END IF

  END SUBROUTINE push_to_stack



  !> @brief
  !> Pop a value off a stack and push it onto another stack.
  !> If covalues are enabled in stack1 then they are also copied
  !> @param[inout] stack1 - Stack to remove the value from
  !> @param[inout] stack2 - Stack to add value to
  !> @param[out] stack_empty - Is the stack empty when trying to pop
  SUBROUTINE pop_to_stack(stack1, stack2, stack_empty)

    TYPE(eis_stack), INTENT(INOUT) :: stack1, stack2
    LOGICAL, OPTIONAL :: stack_empty

    IF (PRESENT(stack_empty)) stack_empty = .TRUE.
    IF (stack1%stack_point == 0) RETURN
    IF (PRESENT(stack_empty)) stack_empty = .FALSE.

    IF (ALLOCATED(stack1%co_entries)) THEN
      CALL push_to_stack(stack2, stack1%entries(stack1%stack_point), &
          stack1%co_entries(stack1%stack_point))
    ELSE
      CALL push_to_stack(stack2, stack1%entries(stack1%stack_point))
    END IF
    stack1%stack_point = stack1%stack_point - 1

  END SUBROUTINE pop_to_stack



  !> @brief
  !> Pop a value off a stack and ignore it
  !> @param[inout] stack - Stack to remove the value from
  SUBROUTINE pop_to_null(stack)

    TYPE(eis_stack), INTENT(INOUT) :: stack

    stack%stack_point = stack%stack_point - 1

  END SUBROUTINE pop_to_null



  !> @brief
  !> Pop a value off a stack and return it. If present and requested
  !> covalues are also returned
  !> @param[inout] stack - Stack to remove the value from
  !> @param[out] value - Value at the top of the stack
  !> @param[out] covalue - Covalue at the top of the stack
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



  !> @brief
  !> Check the value at a distance "offset" from the top of the stack and 
  !> return value and covalue
  !> @param[inout] stack - Stack to return the value from
  !> @param[out] value - Value at the top of the stack
  !> @param[in] offset - Offset from the top of the stack to snoop at.
  !> 0 returns the item at the top of the stack
  !> @param[out] covalue - Covalue at the top of the stack
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



  !> @brief
  !> Routine to initialise a stack element. Pure elemental routine
  !> works on single elements and arrays
  !> @param[inout] element - Element to initialise
  PURE ELEMENTAL SUBROUTINE initialise_stack_element(element)

    TYPE(eis_stack_element), INTENT(INOUT) :: element

    element%ptype = 0
    element%value = 0
    element%numerical_data = 0

  END SUBROUTINE initialise_stack_element



  !> @brief
  !> Routine to initialise a stack coelement. Pure elemental routine
  !> works on single cielements and arrays
  !> @param[inout] element - Coelement to initialise
  PURE ELEMENTAL SUBROUTINE initialise_stack_co_element(element)

    TYPE(eis_stack_co_element), INTENT(INOUT) :: element

  END SUBROUTINE initialise_stack_co_element


  !> @brief
  !> Routine to return a textual version of the stack. Shows the stack in RPN
  !> form
  !> @param[in] token_list - Stack to visualise
  !> @param[inout] str - Allocatable string. Will be allocated exactly the 
  !> right size to hold the token stream
  SUBROUTINE get_tokens(token_list, str)
    TYPE(eis_stack), INTENT(IN) :: token_list
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: str
    INTEGER :: i

    IF (ALLOCATED(str)) DEALLOCATE(str)
    IF (.NOT. ALLOCATED(token_list%co_entries)) RETURN

    DO i = 1, token_list%stack_point
      IF (ALLOCATED(token_list%co_entries(i)%text)) THEN
        CALL eis_append_string(str, token_list%co_entries(i)%text // " ", &
            newline = .FALSE.)
      ELSE
        CALL eis_append_string(str,"{bad_token}", newline = .FALSE.)
      END IF
    END DO
   
  END SUBROUTINE get_tokens


  !> @brief
  !> Routine to automatically print the result of get_tokens to stdout
  !> @param[in] token_list - Stack to visualise
  SUBROUTINE display_tokens_inline(token_list)
    TYPE(eis_stack), INTENT(IN) :: token_list
    CHARACTER(LEN=:), ALLOCATABLE :: str

    IF (.NOT. ALLOCATED(token_list%co_entries)) RETURN

    CALL get_tokens(token_list, str)
    IF (ALLOCATED(str)) THEN
      WRITE(*,*) str
      DEALLOCATE(str)
    END IF
    WRITE(*,*) ""
  END SUBROUTINE display_tokens_inline

END MODULE eis_stack_mod
