MODULE eis_function_registry_mod

  USE eis_header
  USE eis_named_store_mod

  IMPLICIT NONE

  TYPE :: function_entry
    PROCEDURE(parser_eval_fn), POINTER, NOPASS :: fn_ptr
  END TYPE function_entry

  TYPE(named_store), SAVE :: global_fn_table
  PRIVATE
  PUBLIC :: add_eis_function, get_eis_function

CONTAINS

  SUBROUTINE add_eis_function(name, fn)

    CHARACTER(LEN=*), INTENT(IN) :: name
    PROCEDURE(parser_eval_fn) :: fn
    TYPE(function_entry) :: temp

    temp%fn_ptr => fn

    CALL global_fn_table%store(name, temp)

  END SUBROUTINE add_eis_function



  FUNCTION get_eis_function(name) RESULT (gdf)

    CHARACTER(LEN=*), INTENT(IN) :: name
    CLASS(*), POINTER :: gptr
    TYPE(function_entry), POINTER :: temp
    PROCEDURE(parser_eval_fn), POINTER :: gdf

    temp => NULL()
    gptr => global_fn_table%get(name)
    IF (ASSOCIATED(gptr)) THEN
      SELECT TYPE(co => gptr)
        CLASS IS (function_entry)
          temp => co
      END SELECT
    END IF

    IF (ASSOCIATED(temp)) THEN
      gdf => temp%fn_ptr
    ELSE
      gdf => NULL()
    END IF

  END FUNCTION get_eis_function

END MODULE eis_function_registry_mod
