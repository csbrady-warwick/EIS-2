MODULE eis_eval_stack_mod

  USE, INTRINSIC :: ISO_C_BINDING
  USE eis_constants
  USE eis_error_mod
  USE eis_header
  USE eis_parser_header

  IMPLICIT NONE

  TYPE eis_eval_stack
    REAL(eis_num), DIMENSION(:), ALLOCATABLE :: entries
    REAL(eis_num), DIMENSION(:), ALLOCATABLE :: fn_call_vals
    INTEGER :: stack_point = 0
  END TYPE eis_eval_stack

  CONTAINS

  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Resize the backing store. Will not become smaller than
  !> the part of the stack that is in use
  !> @param[inout] this
  !> @param[in] n_elements
  PURE SUBROUTINE ees_resize(this, n_elements, errcode)
    TYPE(eis_eval_stack), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: n_elements !< Number of elements to set the size to
    INTEGER(eis_i8), INTENT(INOUT) :: errcode !< Error code
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
  !> @param[in] user_params
  !> @param[inout] status_code
  !> @param[inout] errcode
  SUBROUTINE ees_eval_element(this, element, user_params, status_code, errcode)
    TYPE(eis_eval_stack), INTENT(INOUT) :: this
    TYPE(eis_stack_element), INTENT(IN) :: element !< Element to be evaluated
    TYPE(C_PTR), INTENT(IN) :: user_params !< Host code supplied parameters
    INTEGER(eis_status), INTENT(INOUT) :: status_code !< Status code information
    INTEGER(eis_error), INTENT(INOUT) :: errcode !< Error code information

    IF (ASSOCIATED(element%eval_fn)) THEN
      IF (.NOT. ALLOCATED(this%fn_call_vals)) THEN
        ALLOCATE(this%fn_call_vals(element%actual_params))
      ELSE IF (element%actual_params > SIZE(this%fn_call_vals)) THEN
        DEALLOCATE(this%fn_call_vals)
        ALLOCATE(this%fn_call_vals(element%actual_params))
      END IF
      CALL ees_pop_vector(this, element%actual_params, this%fn_call_vals, &
          errcode)
      CALL ees_push(this, element%eval_fn(element%actual_params, &
          this%fn_call_vals, user_params, status_code, errcode), errcode)
    END IF

  END SUBROUTINE ees_eval_element



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Push a value onto a stack
  !> @param[inout] this
  !> @param [in] value
  !> @param [inout] errcode
  PURE SUBROUTINE ees_push(this, value, errcode)
    TYPE(eis_eval_stack), INTENT(INOUT) :: this
    REAL(eis_num), INTENT(IN) :: value !< Value to push on the stack
    INTEGER(eis_i8), INTENT(INOUT) :: errcode !< Error code information

    IF (.NOT. ALLOCATED(this%entries)) CALL ees_resize(this, 100, errcode)
    IF (this%stack_point == SIZE(this%entries)) &
        CALL ees_resize(this, this%stack_point * 2, errcode)

    this%stack_point = this%stack_point + 1
    this%entries(this%stack_point) = value

  END SUBROUTINE ees_push



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get the value from the top of the stack
  !> @param[inout] this
  !> @param[out] value
  !> @param[inout] errcode
  PURE SUBROUTINE ees_pop_scalar(this, value, errcode)
    TYPE(eis_eval_stack), INTENT(INOUT) :: this
    REAL(eis_num), INTENT(OUT) :: value !< Value to pop off the stack
    INTEGER(eis_i8), INTENT(INOUT) :: errcode !< Error code information

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
  PURE SUBROUTINE ees_pop_vector(this, value_count, values, errcode)
    TYPE(eis_eval_stack), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: value_count !< Number of values to pop off the stack
    !> Array to hold the popped values
    REAL(eis_num), DIMENSION(:), INTENT(OUT) :: values
    INTEGER(eis_i8), INTENT(INOUT) :: errcode !< Error code information

    IF (value_count <= 0) RETURN

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
    TYPE(eis_eval_stack), INTENT(INOUT) :: this
    !> Value removed from the bottom of the stack
    REAL(eis_num), INTENT(OUT) :: value
    INTEGER(eis_i8), INTENT(INOUT) :: errcode !< Error code information

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
  !> Evaluate a stack to a set of results. Output array will must be allocatable
  !> and if it is too small to hold the results then it will be reallocated to
  !> be large enough to hold them
  !> @param[inout] this
  !> @param[in] stack
  !> @param[inout] result_vals
  !> @param[in] user_params
  !> @param[inout] errcode
  !> @param[inout] err_handler
  !> @param[out] is_no_op
  FUNCTION ees_evaluate(this, stack, result_vals, user_params, errcode, &
      err_handler, is_no_op) RESULT(result_count)
    TYPE(eis_eval_stack), INTENT(INOUT) :: this
    TYPE(eis_stack), INTENT(IN) :: stack !< Stack to evaluate
    !> Array holding the results of the evaluation
    REAL(eis_num), DIMENSION(:), INTENT(INOUT), ALLOCATABLE :: result_vals
    TYPE(C_PTR), INTENT(IN) :: user_params !< Host code user supplied parameters
    INTEGER(eis_error), INTENT(INOUT) :: errcode !< Error code
    !< Error handler instance. Option, default no error reporting
    TYPE(eis_error_handler), INTENT(INOUT), OPTIONAL :: err_handler
    !> Logical value for if expression is flagged as being a null operation.
    !> Optional, default is not to return the null operation status
    LOGICAL, INTENT(OUT), OPTIONAL :: is_no_op
    INTEGER(eis_error) :: err
    INTEGER(eis_status) :: stat_in, status
    INTEGER :: result_count
    INTEGER :: istack
    REAL(eis_num) :: where_condition

    status = eis_status_none

    DO istack = 1, stack%stack_point
      IF (stack%entries(istack)%ptype == eis_pt_constant) THEN
        CALL ees_push(this, stack%entries(istack)%numerical_data, errcode)
      ELSE
        err = eis_err_none
        stat_in = status
        CALL ees_eval_element(this, stack%entries(istack), user_params, &
            stat_in, err)
        status = IOR(status, stat_in)
      END IF
      IF (err /= eis_err_none .AND. PRESENT(err_handler)) THEN
        IF (ALLOCATED(stack%co_entries)) THEN
          CALL err_handler%add_error(eis_err_evaluator, err, &
              stack%co_entries(istack)%text, &
              stack%co_entries(istack)%charindex)
        ELSE
          CALL err_handler%add_error(eis_err_evaluator, err)
        END IF
      END IF
      errcode = IOR(errcode, err)
    END DO

    IF (stack%where_stack) THEN
      where_condition = this%entries(1)
      CALL ees_trim_first(this, where_condition, errcode)
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
    CALL ees_pop_vector(this, this%stack_point, result_vals, errcode)

  END FUNCTION ees_evaluate




  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Evaluate a stack to a set of results in the fastest way possible
  !> @param[inout] this
  !> @param[in] stack
  !> @param[inout] result_vals
  !> @param[in] user_params
  !> @param[inout] errcode
  !> @param[inout] err_handler
  !> @param[out] is_no_op
  FUNCTION ees_evaluate_fast(this, stack, result_vals, user_params, errcode&
      ) RESULT(result_count)
    TYPE(eis_eval_stack), INTENT(INOUT) :: this
    TYPE(eis_stack), INTENT(IN) :: stack !< Stack to evaluate
    !> Array holding the results of the evaluation
    REAL(eis_num), DIMENSION(:), INTENT(INOUT) :: result_vals
    TYPE(C_PTR), INTENT(IN) :: user_params !< Host code user supplied parameters
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER(eis_error) :: err
    INTEGER(eis_status) :: stat_in, status
    INTEGER :: result_count
    INTEGER :: istack

    status = eis_status_none
    errcode = eis_err_none

    this%stack_point = 0
    DO istack = 1, stack%stack_point
      IF (this%stack_point == SIZE(this%entries)) &
          CALL ees_resize(this, this%stack_point * 2, err)
      IF (stack%entries(istack)%ptype == eis_pt_constant) THEN
        err = eis_err_none
        CALL ees_push(this, stack%entries(istack)%numerical_data, err)
      ELSE
        err = eis_err_none
        stat_in = status
        CALL ees_eval_element(this, stack%entries(istack), user_params, &
            stat_in, err)
        status = IOR(status, stat_in)
      END IF
      errcode = IOR(errcode, err)
      IF (errcode /= eis_err_none) THEN
        result_count = 0
        RETURN
      END IF
    END DO

    result_count = this%stack_point
    CALL ees_pop_vector(this, MIN(result_count, SIZE(result_vals)), &
        result_vals, err)

  END FUNCTION ees_evaluate_fast



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Evaluate a stack to a set of results in the fastest way possible
  !> @param[inout] this
  !> @param[in] stack
  !> @param[in] user_params
  !> @param[in] iter_fn
  !> @param[in] store_fn
  SUBROUTINE ees_evaluate_iter(this, stack, user_params, iter_fn, store_fn)
    TYPE(eis_eval_stack), INTENT(INOUT) :: this
    TYPE(eis_stack), INTENT(IN) :: stack !< Stack to evaluate
    TYPE(C_PTR), INTENT(IN) :: user_params !< Host code user supplied parameters
    !> Function to advance user_params to next iteration and return whether or
    !> not to keep iterating
    PROCEDURE(parser_param_update_fn) :: iter_fn
    !> Function to store the results of the evaluation
    PROCEDURE(parser_store_data_fn) :: store_fn
    INTEGER(eis_error) :: err, errcode
    INTEGER(eis_status) :: stat_in, status
    INTEGER :: result_count
    INTEGER :: istack
    LOGICAL :: run
    REAL(eis_num), DIMENSION(:), ALLOCATABLE :: result_vals

    status = eis_status_none
    ALLOCATE(result_vals(1))

    this%stack_point = 0
    run = .TRUE.
    DO WHILE(run)
      DO istack = 1, stack%stack_point
        IF (this%stack_point == SIZE(this%entries)) &
            CALL ees_resize(this, this%stack_point * 2, errcode)
        IF (stack%entries(istack)%ptype == eis_pt_constant) THEN
          CALL ees_push(this, stack%entries(istack)%numerical_data, errcode)
        ELSE
          err = eis_err_none
          stat_in = status
          CALL ees_eval_element(this, stack%entries(istack), user_params, &
              stat_in, err)
          status = IOR(status, stat_in)
        END IF
        errcode = IOR(errcode, err)
      END DO

      result_count = this%stack_point
      IF (SIZE(result_vals) < result_count) ALLOCATE(result_vals(result_count))
      CALL ees_pop_vector(this, result_count, result_vals, errcode)
      CALL store_fn(result_count, result_vals, errcode)
      run = iter_fn(user_params) /= 0
    END DO

  END SUBROUTINE ees_evaluate_iter

END MODULE eis_eval_stack_mod
