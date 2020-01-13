MODULE eis_deck_function_mod

  USE, INTRINSIC :: ISO_C_BINDING
  USE eis_header
  USE eis_parser_header
  USE eis_parser_mod

  IMPLICIT NONE

  ABSTRACT INTERFACE

    SUBROUTINE block_generic_callback(block_text, pass_number, parent_kind, &
        status_code, host_state, errcode)
      IMPORT eis_error, eis_status, eis_bitmask
      CHARACTER(LEN=*), INTENT(IN) :: block_text
      INTEGER, INTENT(IN) :: pass_number
      INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE block_generic_callback

    SUBROUTINE block_callback(block_text, pass_number, parents, parent_kind, &
        status_code, host_state, errcode)
      IMPORT eis_error, eis_status, eis_bitmask
      CHARACTER(LEN=*), INTENT(IN) :: block_text
      INTEGER, INTENT(IN) :: pass_number
      INTEGER, DIMENSION(:), INTENT(IN) :: parents
      INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE block_callback

    SUBROUTINE block_remap_callback(block_text, pass_number, remap_text, &
        status_code, host_state, errcode)
      IMPORT eis_error, eis_status, eis_bitmask
      CHARACTER(LEN=*), INTENT(IN) :: block_text
      INTEGER, INTENT(IN) :: pass_number
      CHARACTER(LEN=*), INTENT(INOUT) :: remap_text
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE block_remap_callback

    FUNCTION should_key_trigger_callback(key_text, pass_number, parents, &
        parent_kind, status_code, host_state, errcode)
      IMPORT eis_error, eis_status, eis_bitmask
      CHARACTER(LEN=*), INTENT(IN) :: key_text
      INTEGER, INTENT(IN) :: pass_number
      INTEGER, DIMENSION(:), INTENT(IN) :: parents
      INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
      INTEGER(eis_error), INTENT(INOUT) :: errcode
      LOGICAL :: should_key_trigger_callback
    END FUNCTION should_key_trigger_callback

    SUBROUTINE key_text_callback(key_text, pass_number, parents, parent_kind, &
        status_code, host_state, errcode)
      IMPORT eis_error, eis_status, eis_bitmask
      CHARACTER(LEN=*), INTENT(IN) :: key_text
      INTEGER, INTENT(IN) :: pass_number
      INTEGER, DIMENSION(:), INTENT(IN) :: parents
      INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE key_text_callback

    SUBROUTINE key_value_callback(key_text, value_text, pass_number, parents, &
        parent_kind, status_code, host_state, errcode)
      IMPORT eis_error, eis_status, eis_bitmask
      CHARACTER(LEN=*), INTENT(IN) :: key_text, value_text
      INTEGER, INTENT(IN) :: pass_number
      INTEGER, DIMENSION(:), INTENT(IN) :: parents
      INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE key_value_callback

    SUBROUTINE key_numeric_value_callback(key_text, values, pass_number, &
        cap_bits, parser, parents, parent_kind, status_code, host_state, &
        errcode)
      IMPORT eis_error, eis_status, eis_num, eis_parser, eis_bitmask
      CHARACTER(LEN=*), INTENT(IN) :: key_text
      REAL(eis_num), DIMENSION(:), INTENT(IN) :: values
      INTEGER, INTENT(IN) :: pass_number
      INTEGER(eis_bitmask), INTENT(IN) :: cap_bits
      TYPE(eis_parser), INTENT(INOUT) :: parser
      INTEGER, DIMENSION(:), INTENT(IN) :: parents
      INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE key_numeric_value_callback

    SUBROUTINE key_stack_callback(key_text, value_stack, pass_number, parser, &
        parents, parent_kind, status_code, host_state, errcode)
      IMPORT eis_error, eis_stack, eis_status, eis_parser, eis_bitmask
      CHARACTER(LEN=*), INTENT(IN) :: key_text
      TYPE(eis_stack), INTENT(INOUT) :: value_stack
      INTEGER, INTENT(IN) :: pass_number
      TYPE(eis_parser), INTENT(INOUT) :: parser
      INTEGER, DIMENSION(:), INTENT(IN) :: parents
      INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE key_stack_callback

    SUBROUTINE event_callback(event_text, pass_number, parents, parent_kind, &
        status_code, host_state, errcode)
      IMPORT eis_error, eis_stack, eis_status, eis_parser, eis_bitmask
      CHARACTER(LEN=*), INTENT(IN) :: event_text
      INTEGER, INTENT(IN) :: pass_number
      INTEGER, DIMENSION(:), INTENT(IN) :: parents
      INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE event_callback

    SUBROUTINE generic_event_callback(event_text, pass_number, parent_kind, &
        status_code, host_state, errcode)
      IMPORT eis_error, eis_stack, eis_status, eis_parser, eis_bitmask
      CHARACTER(LEN=*), INTENT(IN) :: event_text
      INTEGER, INTENT(IN) :: pass_number
      INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE generic_event_callback
    
  END INTERFACE

  INTERFACE
    SUBROUTINE block_generic_callback_c(block_text, pass_number, nparents, &
        parent_kind, status_code, host_state, errcode)
      IMPORT eis_error_c, eis_status_c, eis_bitmask_c, C_INT, C_PTR
      TYPE(C_PTR), VALUE, INTENT(IN) :: block_text
      INTEGER(C_INT), INTENT(IN) :: pass_number
      INTEGER(C_INT), VALUE, INTENT(IN) :: nparents
      INTEGER(C_INT), DIMENSION(nparents), INTENT(IN) :: parent_kind
      INTEGER(eis_status_c), INTENT(INOUT) :: status_code
      INTEGER(eis_bitmask_c), INTENT(INOUT) :: host_state
      INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    END SUBROUTINE block_generic_callback_c

    SUBROUTINE block_callback_c(block_text, pass_number, nparents, parents, &
        parent_kind, status_code, host_state, errcode) BIND(C)
      IMPORT eis_error_c, eis_status_c, eis_bitmask_c, C_INT, C_PTR
      TYPE(C_PTR), VALUE, INTENT(IN) :: block_text
      INTEGER(C_INT), INTENT(IN) :: pass_number
      INTEGER(C_INT), VALUE, INTENT(IN) :: nparents
      INTEGER(C_INT), DIMENSION(nparents), INTENT(IN) :: parents
      INTEGER(C_INT), DIMENSION(nparents), INTENT(IN) :: parent_kind
      INTEGER(eis_status_c), INTENT(INOUT) :: status_code
      INTEGER(eis_bitmask_c), INTENT(INOUT) :: host_state
      INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    END SUBROUTINE block_callback_c

    SUBROUTINE block_remap_callback_c(block_text, pass_number, len_remap_text, &
        remap_text, status_code, host_state, errcode) BIND(C)
      IMPORT eis_error_c, eis_status_c, eis_bitmask_c, C_INT, C_PTR
      TYPE(C_PTR), VALUE, INTENT(IN) :: block_text
      INTEGER(C_INT), INTENT(IN) :: pass_number
      INTEGER(C_INT), VALUE, INTENT(IN) :: len_remap_text
      TYPE(C_PTR), VALUE, INTENT(IN) :: remap_text
      INTEGER(eis_status_c), INTENT(INOUT) :: status_code
      INTEGER(eis_bitmask_c), INTENT(INOUT) :: host_state
      INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    END SUBROUTINE block_remap_callback_c

    FUNCTION should_key_trigger_callback_c(key_text, pass_number, nparents, &
        parents, parent_kind, status_code, host_state, errcode) BIND(C)
      IMPORT eis_error_c, eis_status_c, eis_bitmask_c, C_INT, C_PTR
      TYPE(C_PTR), VALUE, INTENT(IN) :: key_text
      INTEGER(C_INT), INTENT(IN) :: pass_number
      INTEGER(C_INT), VALUE, INTENT(IN) :: nparents
      INTEGER(C_INT), DIMENSION(nparents), INTENT(IN) :: parents
      INTEGER(C_INT), DIMENSION(nparents), INTENT(IN) :: parent_kind
      INTEGER(eis_status_c), INTENT(INOUT) :: status_code
      INTEGER(eis_bitmask_c), INTENT(INOUT) :: host_state
      INTEGER(eis_error_c), INTENT(INOUT) :: errcode
      INTEGER(C_INT) :: should_key_trigger_callback_c
    END FUNCTION should_key_trigger_callback_c

    SUBROUTINE key_text_callback_c(key_text, pass_number, nparents, parents, &
        parent_kind, status_code, host_state, errcode) BIND(C)
      IMPORT eis_error_c, eis_status_c, eis_bitmask_c, C_INT, C_PTR
      TYPE(C_PTR), VALUE, INTENT(IN) :: key_text
      INTEGER(C_INT), INTENT(IN) :: pass_number
      INTEGER(C_INT), VALUE, INTENT(IN) :: nparents
      INTEGER(C_INT), DIMENSION(nparents), INTENT(IN) :: parents
      INTEGER(C_INT), DIMENSION(nparents), INTENT(IN) :: parent_kind
      INTEGER(eis_status_c), INTENT(INOUT) :: status_code
      INTEGER(eis_bitmask_c), INTENT(INOUT) :: host_state
      INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    END SUBROUTINE key_text_callback_c

    SUBROUTINE key_value_callback_c(key_text, value_text, pass_number, &
        nparents, parents, parent_kind, status_code, host_state, errcode) &
        BIND(C)
      IMPORT eis_error_c, eis_status_c, eis_bitmask_c, C_INT, C_PTR
      TYPE(C_PTR), VALUE, INTENT(IN) :: key_text, value_text
      INTEGER(C_INT), INTENT(IN) :: pass_number
      INTEGER(C_INT), VALUE, INTENT(IN) :: nparents
      INTEGER(C_INT), DIMENSION(nparents), INTENT(IN) :: parents
      INTEGER(C_INT), DIMENSION(nparents), INTENT(IN) :: parent_kind
      INTEGER(eis_status_c), INTENT(INOUT) :: status_code
      INTEGER(eis_bitmask_c), INTENT(INOUT) :: host_state
      INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    END SUBROUTINE key_value_callback_c

    SUBROUTINE key_numeric_value_callback_c(key_text, nvalues, values, &
        pass_number, cap_bits, parser, nparents, parents, parent_kind, &
        status_code, host_state, errcode) BIND(C)
      IMPORT eis_error_c, eis_status_c, eis_num_c, eis_bitmask_c, C_INT, C_PTR
      TYPE(C_PTR), VALUE, INTENT(IN) :: key_text
      INTEGER(C_INT), INTENT(IN) :: pass_number
      INTEGER(eis_bitmask_c), VALUE, INTENT(IN) :: cap_bits
      INTEGER(C_INT), VALUE, INTENT(IN) :: nvalues
      REAL(eis_num_c), DIMENSION(nvalues), INTENT(IN) :: values
      INTEGER(C_INT), VALUE, INTENT(IN) :: parser
      INTEGER(C_INT), VALUE, INTENT(IN) :: nparents
      INTEGER(C_INT), DIMENSION(nparents), INTENT(IN) :: parents
      INTEGER(C_INT), DIMENSION(nparents), INTENT(IN) :: parent_kind
      INTEGER(eis_status_c), INTENT(INOUT) :: status_code
      INTEGER(eis_bitmask_c), INTENT(INOUT) :: host_state
      INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    END SUBROUTINE key_numeric_value_callback_c

    SUBROUTINE key_stack_callback_c(key_text, value_stack, pass_number, &
        parser, nparents, parents, parent_kind, status_code, host_state, &
        errcode) BIND(C)
      IMPORT eis_error_c, eis_status_c, eis_bitmask_c, C_INT, C_PTR
      TYPE(C_PTR), VALUE, INTENT(IN) :: key_text
      INTEGER(C_INT), INTENT(IN) :: pass_number
      INTEGER(C_INT), VALUE, INTENT(IN) :: value_stack
      INTEGER(C_INT), VALUE, INTENT(IN) :: parser
      INTEGER(C_INT), VALUE, INTENT(IN) :: nparents
      INTEGER(C_INT), DIMENSION(nparents), INTENT(IN) :: parents
      INTEGER(C_INT), DIMENSION(nparents), INTENT(IN) :: parent_kind
      INTEGER(eis_status_c), INTENT(INOUT) :: status_code
      INTEGER(eis_bitmask_c), INTENT(INOUT) :: host_state
      INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    END SUBROUTINE key_stack_callback_c

    SUBROUTINE event_callback_c(event_text, pass_number, &
        nparents, parents, parent_kind, status_code, host_state, errcode) &
        BIND(C)
      IMPORT eis_error_c, eis_status_c, eis_bitmask_c, C_INT, C_PTR
      TYPE(C_PTR), VALUE, INTENT(IN) :: event_text
      INTEGER(C_INT), INTENT(IN) :: pass_number
      INTEGER(C_INT), VALUE, INTENT(IN) :: nparents
      INTEGER(C_INT), DIMENSION(nparents), INTENT(IN) :: parents
      INTEGER(C_INT), DIMENSION(nparents), INTENT(IN) :: parent_kind
      INTEGER(eis_status_c), INTENT(INOUT) :: status_code
      INTEGER(eis_bitmask_c), INTENT(INOUT) :: host_state
      INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    END SUBROUTINE event_callback_c

    SUBROUTINE generic_event_callback_c(event_text, pass_number, &
        nparents, parent_kind, status_code, host_state, errcode) &
        BIND(C)
      IMPORT eis_error_c, eis_status_c, eis_bitmask_c, C_INT, C_PTR
      TYPE(C_PTR), VALUE, INTENT(IN) :: event_text
      INTEGER(C_INT), INTENT(IN) :: pass_number
      INTEGER(C_INT), VALUE, INTENT(IN) :: nparents
      INTEGER(C_INT), DIMENSION(nparents), INTENT(IN) :: parent_kind
      INTEGER(eis_status_c), INTENT(INOUT) :: status_code
      INTEGER(eis_bitmask_c), INTENT(INOUT) :: host_state
      INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    END SUBROUTINE generic_event_callback_c
  END INTERFACE

END MODULE eis_deck_function_mod
