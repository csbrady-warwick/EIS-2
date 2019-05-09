MODULE eis_eval_stack_mod

  USE, INTRINSIC :: ISO_C_BINDING
  USE eis_constants
  USE eis_header
  USE eis_error_mod

  IMPLICIT NONE

  TYPE eis_eval_stack
    REAL(eis_num), DIMENSION(:), ALLOCATABLE :: entries
    REAL(eis_num), DIMENSION(:), ALLOCATABLE :: fn_call_vals
    INTEGER :: stack_point = 0
    CONTAINS
    PROCEDURE, PRIVATE :: resize => ees_resize
    PROCEDURE :: eval_element => ees_eval_element
    PROCEDURE :: evaluate => ees_evaluate
    PROCEDURE :: push => ees_push
    PROCEDURE, PRIVATE :: pop_scalar => ees_pop_scalar
    PROCEDURE, PRIVATE :: pop_vector => ees_pop_vector
    GENERIC :: pop => pop_scalar, pop_vector
    PROCEDURE, PRIVATE :: trim_first => ees_trim_first
  END TYPE eis_eval_stack

  CONTAINS

  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Resize the backing store 
  !> @param[inout] this
  !> @param[in] n_elements
  SUBROUTINE ees_resize(this, n_elements, errcode)
    CLASS(eis_eval_stack), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: n_elements
    INTEGER(eis_i8), INTENT(INOUT) :: errcode
    REAL(eis_num), DIMENSION(:), ALLOCATABLE :: temp
    INTEGER :: newcount

    newcount = MAX(n_elements, this%stack_point, 1)

    ALLOCATE(temp(1:newcount))
    IF (ALLOCATED(this%entries)) THEN
      temp(1:this%stack_point) = this%entries(1:this%stack_point)
      DEALLOCATE(this%entries)
    END IF
    CALL MOVE_ALLOC(temp, this%entries)
  END SUBROUTINE ees_resize



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Evaluate a single stack element
  !> @param[inout] this
  !> @param[in] n_elements
  SUBROUTINE ees_eval_element(this, element, user_params, status_code, errcode)
    CLASS(eis_eval_stack), INTENT(INOUT) :: this
    TYPE(eis_stack_element), INTENT(IN) :: element
    TYPE(C_PTR), INTENT(IN) :: user_params
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER :: iel

    IF (ASSOCIATED(element%eval_fn)) THEN
      IF (.NOT. ALLOCATED(this%fn_call_vals)) THEN
        ALLOCATE(this%fn_call_vals(element%actual_params))
      ELSE IF (element%actual_params > SIZE(this%fn_call_vals)) THEN
        DEALLOCATE(this%fn_call_vals)
        ALLOCATE(this%fn_call_vals(element%actual_params))
      END IF
      CALL this%pop(element%actual_params, this%fn_call_vals, errcode)
      CALL this%push(element%eval_fn(element%actual_params, &
          this%fn_call_vals, user_params, status_code, errcode), errcode)
    END IF

  END SUBROUTINE ees_eval_element



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Push a value onto a stack
  !> @param[inout] this
  !> @param [in] value
  !> @param [inout] errcode
  SUBROUTINE ees_push(this, value, errcode)
    CLASS(eis_eval_stack), INTENT(INOUT) :: this
    REAL(eis_num), INTENT(IN) :: value
    INTEGER(eis_i8), INTENT(INOUT) :: errcode

    IF (.NOT. ALLOCATED(this%entries)) CALL this%resize(1, errcode)
    IF (this%stack_point == SIZE(this%entries)) &
        CALL this%resize(this%stack_point * 2, errcode)

    this%stack_point = this%stack_point + 1
    this%entries(this%stack_point) = value

  END SUBROUTINE ees_push



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get the value from the top of the stack
  !> @param[inout] this
  !> @param[out] value
  !> @param[inout] errcode
  SUBROUTINE ees_pop_scalar(this, value, errcode)
    CLASS(eis_eval_stack), INTENT(INOUT) :: this
    REAL(eis_num), INTENT(OUT) :: value
    INTEGER(eis_i8), INTENT(INOUT) :: errcode

    IF (this%stack_point > 0) THEN
      value = this%entries(this%stack_point)
      this%stack_point = this%stack_point - 1
    ELSE
      !>@TODO error code
    END IF

  END SUBROUTINE ees_pop_scalar



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get the value from the top of the stack
  !> @param[inout] this
  !> @param[in] value_count
  !> @param[out] values
  !> @param[inout] errcode
  SUBROUTINE ees_pop_vector(this, value_count, values, errcode)
    CLASS(eis_eval_stack), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: value_count
    REAL(eis_num), DIMENSION(:), INTENT(OUT) :: values
    INTEGER(eis_i8), INTENT(INOUT) :: errcode

    IF (this%stack_point >= value_count) THEN
      values(1:value_count) = this%entries(this%stack_point - value_count + 1 &
          :this%stack_point)
      this%stack_point = this%stack_point - value_count
    ELSE
      !>@TODO error code
    END IF

  END SUBROUTINE ees_pop_vector



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Pull the bottom element of the stack and push the stack down
  !> @param[inout] this
  !> @param[out] value
  !> @param[inout] errcode
  SUBROUTINE ees_trim_first(this, value, errcode)
    CLASS(eis_eval_stack), INTENT(INOUT) :: this
    REAL(eis_num), INTENT(OUT) :: value
    INTEGER(eis_i8), INTENT(INOUT) :: errcode

    IF (this%stack_point > 0) THEN
      value = this%entries(1)
      this%entries(1:this%stack_point-1) = this%entries(2:this%stack_point)
      this%stack_point = this%stack_point - 1
    ELSE
      !>@TODO error code
    END IF

  END SUBROUTINE ees_trim_first



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Evaluate a stack to a set of results
  !> @param[inout] this
  !> @param[in] stack
  !> @param[in] result_vals
  FUNCTION ees_evaluate(this, stack, result_vals, user_params, errcode, &
      err_handler, is_no_op) RESULT(result_count)
    CLASS(eis_eval_stack), INTENT(INOUT) :: this
    TYPE(eis_stack), INTENT(IN) :: stack
    REAL(eis_num), DIMENSION(:), ALLOCATABLE :: result_vals
    TYPE(C_PTR), INTENT(IN) :: user_params
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    LOGICAL, INTENT(OUT), OPTIONAL :: is_no_op
    INTEGER(eis_error) :: err
    INTEGER(eis_status) :: stat_in, status
    INTEGER :: result_count
    INTEGER :: istack
    REAL(eis_num) :: where_condition

    status = eis_status_none

    DO istack = 1, stack%stack_point
      IF (stack%entries(istack)%ptype == c_pt_constant) THEN
        CALL this%push(stack%entries(istack)%numerical_data, errcode)
      ELSE
        err = errcode
        stat_in = status
        CALL this%eval_element(stack%entries(istack), user_params, stat_in, &
            err)
        status = IOR(status, stat_in)
        IF (err /= eis_err_none .AND. PRESENT(err_handler)) THEN
          IF (ALLOCATED(stack%co_entries)) THEN
            CALL err_handler%add_error(eis_err_evaluator, err, &
                stack%co_entries(istack)%text, &
                stack%co_entries(istack)%charindex)
          ELSE
            CALL err_handler%add_error(eis_err_evaluator, err)
          END IF
          errcode = IOR(errcode, err)
        END IF
      END IF
    END DO

    IF (stack%where_stack) THEN
      where_condition = this%entries(1)
      CALL this%trim_first(where_condition, errcode)
      IF (PRESENT(is_no_op)) THEN
        is_no_op = ABS(where_condition) < eis_tiny
      ELSE
        errcode = IOR(errcode, eis_err_where)
        CALL err_handler%add_error(eis_err_evaluator, errcode, 'where', 1)
      END IF
    ELSE
      IF (PRESENT(is_no_op)) THEN
        is_no_op = .FALSE.
      END IF
    END IF

    IF (.NOT. ALLOCATED(result_vals)) THEN
      ALLOCATE(result_vals(1:this%stack_point))
    ELSE IF (SIZE(result_vals) < this%stack_point) THEN
      DEALLOCATE(result_vals)
      ALLOCATE(result_vals(1:this%stack_point))
    END IF

    result_count = this%stack_point
    CALL this%pop(this%stack_point, result_vals, errcode)

  END FUNCTION ees_evaluate

END MODULE eis_eval_stack_mod
