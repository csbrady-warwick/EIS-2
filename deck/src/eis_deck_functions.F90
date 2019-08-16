MODULE eis_deck_function_mod

  USE, INTRINSIC :: ISO_C_BINDING
  USE eis_constants
  USE eis_parser_header

  INTERFACE
    SUBROUTINE block_callback(block_text, parents, parent_kind, errcode)
      IMPORT eis_error
      CHARACTER(LEN=*), INTENT(IN) :: block_text
      INTEGER, DIMENSION(:), INTENT(IN) :: parents
      INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE block_callback

    SUBROUTINE key_text_callback(key_text, parents, parent_kind, errcode)
      IMPORT eis_error
      CHARACTER(LEN=*), INTENT(IN) :: key_text
      INTEGER, DIMENSION(:), INTENT(IN) :: parents
      INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE key_text_callback

    SUBROUTINE key_value_callback(key_text, value_text, parents, parent_kind, &
        errcode)
      IMPORT eis_error
      CHARACTER(LEN=*), INTENT(IN) :: key_text, value_text
      INTEGER, DIMENSION(:), INTENT(IN) :: parents
      INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE key_value_callback

    SUBROUTINE key_stack_callback(key_text, value_stack, parents, parent_kind, &
        errcode)
      IMPORT eis_error, eis_stack
      CHARACTER(LEN=*), INTENT(IN) :: key_text
      TYPE(eis_stack), INTENT(INOUT) :: value_stack
      INTEGER, DIMENSION(:), INTENT(IN) :: parents
      INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE key_stack_callback
  END INTERFACE

  INTERFACE
    SUBROUTINE block_callback_c(block_text, nparents, parents, parent_kind, &
        errcode) BIND(C)
      IMPORT eis_error, C_PTR
      TYPE(C_PTR), VALUE, INTENT(IN) :: block_text
      INTEGER, VALUE, INTENT(IN) :: nparents
      INTEGER, DIMENSION(nparents), INTENT(IN) :: parents
      INTEGER, DIMENSION(nparents), INTENT(IN) :: parent_kind
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE block_callback_c

    SUBROUTINE key_text_callback_c(key_text, nparents, parents, parent_kind, &
        errcode) BIND(C)
      IMPORT eis_error, C_PTR
      TYPE(C_PTR), VALUE, INTENT(IN) :: key_text
      INTEGER, VALUE, INTENT(IN) :: nparents
      INTEGER, DIMENSION(nparents), INTENT(IN) :: parents
      INTEGER, DIMENSION(nparents), INTENT(IN) :: parent_kind
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE key_text_callback_c

    SUBROUTINE key_value_callback_c(key_text, value_text, nparents, parents, &
        parent_kind, errcode) BIND(C)
      IMPORT eis_error, C_PTR
      TYPE(C_PTR), VALUE, INTENT(IN) :: key_text, value_text
      INTEGER, VALUE, INTENT(IN) :: nparents
      INTEGER, DIMENSION(nparents), INTENT(IN) :: parents
      INTEGER, DIMENSION(nparents), INTENT(IN) :: parent_kind
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE key_value_callback_c

    SUBROUTINE key_stack_callback_c(key_text, value_stack, nparents, parents, &
        parent_kind, errcode) BIND(C)
      IMPORT eis_error, C_PTR
      TYPE(C_PTR), VALUE, INTENT(IN) :: key_text
      INTEGER, VALUE, INTENT(IN) :: value_stack
      INTEGER, VALUE, INTENT(IN) :: nparents
      INTEGER, DIMENSION(nparents), INTENT(IN) :: parents
      INTEGER, DIMENSION(nparents), INTENT(IN) :: parent_kind
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE key_stack_callback_c
  END INTERFACE

END MODULE eis_deck_function_mod
