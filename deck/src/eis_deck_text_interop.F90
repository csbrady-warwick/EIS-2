! Copyright (c) 2019-2026, C. S. Brady and H. Ratcliffe
! SPDX-License-Identifier: BSD-3-Clause

#include "interop_version.inc"
#if (EIS_C_INTEROP_VERSION <= 2)
MODULE eis_deck_text_interop

  USE, INTRINSIC :: ISO_C_BINDING
  USE eis_header
  USE eis_utils
  USE eis_string_store_mod, ONLY: filename_processor_proto_c, &
      file_text_processor_proto_c
  USE eis_parser_mod
  USE eis_deck_definition_mod
  USE eis_deck_definition_interop
  USE eis_deck_from_text_mod
  IMPLICIT NONE

  PRIVATE :: add_interop_deck_parser

  !> Holder for a text deck parser that is referenced from C by an integer ID
  TYPE :: deck_parser_holder
    TYPE(eis_text_deck_parser), POINTER :: contents => NULL()
    INTEGER :: refcount = 1
    LOGICAL :: in_use = .FALSE.
  END TYPE deck_parser_holder

  TYPE(deck_parser_holder), DIMENSION(:), ALLOCATABLE :: interop_deck_parsers

  !> Parameters for eis_deck_create_parser
  TYPE, BIND(C) :: eis_deck_create_parser_params
    INTEGER(C_INT) :: parser_id
    TYPE(C_PTR) :: host_params
    INTEGER(C_INT) :: unknown_block_is_fatal
    INTEGER(C_INT) :: unknown_key_is_fatal
    INTEGER(C_INT) :: bad_key_is_fatal
    INTEGER(C_INT) :: non_value_line_is_blank
    INTEGER(C_INT) :: end_pass_on_error
    INTEGER(C_INT) :: finalize_on_error
  END TYPE eis_deck_create_parser_params

  !> Parameters for eis_deck_parser_inc_ref
  TYPE, BIND(C) :: eis_deck_parser_inc_ref_params
    INTEGER(C_INT) :: dummy
  END TYPE eis_deck_parser_inc_ref_params

  !> Parameters for eis_deck_parser_dec_ref
  TYPE, BIND(C) :: eis_deck_parser_dec_ref_params
    INTEGER(C_INT) :: dummy
  END TYPE eis_deck_parser_dec_ref_params

  !> Parameters for eis_deck_parse_file
  TYPE, BIND(C) :: eis_deck_parse_file_params
    INTEGER(C_INT) :: pass_number
    INTEGER(C_INT) :: max_passes
    INTEGER(C_INT) :: max_level
    INTEGER(C_INT) :: allow_root_keys
    INTEGER(C_INT) :: allow_empty_blocks
    TYPE(C_PTR) :: state
    INTEGER(C_INT) :: initialise_all_blocks
    INTEGER(C_INT) :: unknown_block_is_fatal
    INTEGER(C_INT) :: unknown_key_is_fatal
    INTEGER(C_INT) :: non_value_line_is_blank
    TYPE(C_FUNPTR) :: filename_processor
    TYPE(C_FUNPTR) :: file_text_processor
    INTEGER(C_INT) :: end_pass_on_error
    INTEGER(C_INT) :: finalize_on_error
    INTEGER(C_INT) :: bad_key_is_fatal
  END TYPE eis_deck_parse_file_params

  !> Parameters for eis_deck_parse_string
  TYPE, BIND(C) :: eis_deck_parse_string_params
    INTEGER(C_INT) :: pass_number
    INTEGER(C_INT) :: max_passes
    INTEGER(C_INT) :: max_level
    INTEGER(C_INT) :: allow_root_keys
    INTEGER(C_INT) :: allow_empty_blocks
    TYPE(C_PTR) :: state
    INTEGER(C_INT) :: initialise_all_blocks
    INTEGER(C_INT) :: unknown_block_is_fatal
    INTEGER(C_INT) :: unknown_key_is_fatal
    INTEGER(C_INT) :: non_value_line_is_blank
    TYPE(C_PTR) :: filename
    INTEGER(C_INT) :: end_pass_on_error
    INTEGER(C_INT) :: finalize_on_error
    INTEGER(C_INT) :: bad_key_is_fatal
  END TYPE eis_deck_parse_string_params

  !> Parameters for eis_deck_reparse
  TYPE, BIND(C) :: eis_deck_reparse_params
    INTEGER(C_INT) :: pass_number
    INTEGER(C_INT) :: max_passes
    INTEGER(C_INT) :: max_level
    INTEGER(C_INT) :: allow_root_keys
    INTEGER(C_INT) :: allow_empty_blocks
    TYPE(C_PTR) :: state
    INTEGER(C_INT) :: initialise_all_blocks
    INTEGER(C_INT) :: unknown_block_is_fatal
    INTEGER(C_INT) :: unknown_key_is_fatal
    INTEGER(C_INT) :: non_value_line_is_blank
    INTEGER(C_INT) :: end_pass_on_error
    INTEGER(C_INT) :: finalize_on_error
    INTEGER(C_INT) :: bad_key_is_fatal
  END TYPE eis_deck_reparse_params

  !> Parameters for eis_deck_parse_serialised
  TYPE, BIND(C) :: eis_deck_parse_serialised_params
    INTEGER(C_INT) :: pass_number
    INTEGER(C_INT) :: max_passes
    INTEGER(C_INT) :: max_level
    INTEGER(C_INT) :: allow_root_keys
    INTEGER(C_INT) :: allow_empty_blocks
    TYPE(C_PTR) :: state
    INTEGER(C_INT) :: initialise_all_blocks
    INTEGER(C_INT) :: unknown_block_is_fatal
    INTEGER(C_INT) :: unknown_key_is_fatal
    INTEGER(C_INT) :: non_value_line_is_blank
    INTEGER(C_INT) :: end_pass_on_error
    INTEGER(C_INT) :: finalize_on_error
    INTEGER(C_INT) :: bad_key_is_fatal
  END TYPE eis_deck_parse_serialised_params

  !> Parameters for eis_deck_get_serialised
  TYPE, BIND(C) :: eis_deck_get_serialised_params
    TYPE(C_FUNPTR) :: filename_processor
    TYPE(C_FUNPTR) :: file_text_processor
  END TYPE eis_deck_get_serialised_params

  !> Parameters for eis_deck_initialise
  TYPE, BIND(C) :: eis_deck_initialise_params
    TYPE(C_PTR) :: state
    INTEGER(C_INT) :: pass_number
  END TYPE eis_deck_initialise_params

  !> Parameters for eis_deck_finalise
  TYPE, BIND(C) :: eis_deck_finalise_params
    TYPE(C_PTR) :: state
    INTEGER(C_INT) :: pass_number
    INTEGER(C_INT) :: last_pass
  END TYPE eis_deck_finalise_params

  !> Parameters for eis_deck_get_error_count
  TYPE, BIND(C) :: eis_deck_get_error_count_params
    INTEGER(C_INT) :: dummy
  END TYPE eis_deck_get_error_count_params

  !> Parameters for eis_deck_get_error_report
  TYPE, BIND(C) :: eis_deck_get_error_report_params
    INTEGER(C_INT) :: dummy
  END TYPE eis_deck_get_error_report_params

  !> Parameters for eis_deck_flush_errors
  TYPE, BIND(C) :: eis_deck_flush_errors_params
    INTEGER(C_INT) :: dummy
  END TYPE eis_deck_flush_errors_params

  !> Parameters for eis_deck_get_parsed_block_name
  TYPE, BIND(C) :: eis_deck_get_parsed_block_name_params
    INTEGER(C_INT) :: dummy
  END TYPE eis_deck_get_parsed_block_name_params

  !> Parameters for eis_deck_get_parsed_block_parents
  TYPE, BIND(C) :: eis_deck_get_parsed_block_parents_params
    INTEGER(C_INT) :: dummy
  END TYPE eis_deck_get_parsed_block_parents_params

  !> Parameters for eis_deck_get_block_structure
  TYPE, BIND(C) :: eis_deck_get_block_structure_params
    INTEGER(C_INT) :: include_keys
    INTEGER(C_INT) :: max_depth
    INTEGER(C_INT) :: max_key_depth
  END TYPE eis_deck_get_block_structure_params

  !> Parameters for eis_deck_release_strings
  TYPE, BIND(C) :: eis_deck_release_strings_params
    INTEGER(C_INT) :: dummy
  END TYPE eis_deck_release_strings_params

  CONTAINS

  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Store a text deck parser so that it can be referenced from C
  !> @param[in] deck - Parser to store
  !> @return deck_id - ID of the stored parser
  FUNCTION add_interop_deck_parser(deck) RESULT(deck_id)

    TYPE(eis_text_deck_parser), POINTER, INTENT(IN) :: deck
    INTEGER :: deck_id
    TYPE(deck_parser_holder), DIMENSION(:), ALLOCATABLE :: temp
    INTEGER :: i, old_size

    IF (.NOT. ALLOCATED(interop_deck_parsers)) ALLOCATE(interop_deck_parsers(10))

    DO i = 1, SIZE(interop_deck_parsers)
      IF (.NOT. interop_deck_parsers(i)%in_use) THEN
        interop_deck_parsers(i)%contents => deck
        interop_deck_parsers(i)%refcount = 1
        interop_deck_parsers(i)%in_use = .TRUE.
        deck_id = i
        RETURN
      END IF
    END DO

    old_size = SIZE(interop_deck_parsers)
    ALLOCATE(temp(old_size * 2))
    temp(1:old_size) = interop_deck_parsers
    CALL MOVE_ALLOC(temp, interop_deck_parsers)
    interop_deck_parsers(old_size + 1)%contents => deck
    interop_deck_parsers(old_size + 1)%refcount = 1
    interop_deck_parsers(old_size + 1)%in_use = .TRUE.
    deck_id = old_size + 1

  END FUNCTION add_interop_deck_parser



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get a pointer to a text deck parser that is referenced from C
  !> @param[in] deck_id - ID of the parser
  !> @return deck - Pointer to the parser. NULL if the ID is invalid
  FUNCTION eis_deck_get_interop_parser(deck_id) RESULT(deck)

    INTEGER, INTENT(IN) :: deck_id
    TYPE(eis_text_deck_parser), POINTER :: deck

    deck => NULL()
    IF (.NOT. ALLOCATED(interop_deck_parsers)) RETURN
    IF (deck_id < 1 .OR. deck_id > SIZE(interop_deck_parsers)) RETURN
    IF (.NOT. interop_deck_parsers(deck_id)%in_use) RETURN
    deck => interop_deck_parsers(deck_id)%contents

  END FUNCTION eis_deck_get_interop_parser



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_deck_create_parser to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_deck_create_parser_params_init(params) BIND(C)

    TYPE(eis_deck_create_parser_params), INTENT(OUT) :: params

    params%parser_id = -1
    params%host_params = C_NULL_PTR
    params%unknown_block_is_fatal = 1
    params%unknown_key_is_fatal = 1
    params%bad_key_is_fatal = 1
    params%non_value_line_is_blank = 0
    params%end_pass_on_error = 1
    params%finalize_on_error = 1

  END SUBROUTINE eis_deck_create_parser_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_deck_parser_inc_ref to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_deck_parser_inc_ref_params_init(params) BIND(C)

    TYPE(eis_deck_parser_inc_ref_params), INTENT(OUT) :: params

    params%dummy = 0

  END SUBROUTINE eis_deck_parser_inc_ref_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_deck_parser_dec_ref to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_deck_parser_dec_ref_params_init(params) BIND(C)

    TYPE(eis_deck_parser_dec_ref_params), INTENT(OUT) :: params

    params%dummy = 0

  END SUBROUTINE eis_deck_parser_dec_ref_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for eis_deck_parse_file to
  !> their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_deck_parse_file_params_init(params) BIND(C)

    TYPE(eis_deck_parse_file_params), INTENT(OUT) :: params

    params%pass_number = 1
    params%max_passes = 1
    params%max_level = -1
    params%allow_root_keys = -1
    params%allow_empty_blocks = -1
    params%state = C_NULL_PTR
    params%initialise_all_blocks = 0
    params%unknown_block_is_fatal = -1
    params%unknown_key_is_fatal = -1
    params%non_value_line_is_blank = -1
    params%filename_processor = C_NULL_FUNPTR
    params%file_text_processor = C_NULL_FUNPTR
    params%end_pass_on_error = -1
    params%finalize_on_error = -1
    params%bad_key_is_fatal = -1

  END SUBROUTINE eis_deck_parse_file_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for eis_deck_parse_string
  !> to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_deck_parse_string_params_init(params) BIND(C)

    TYPE(eis_deck_parse_string_params), INTENT(OUT) :: params

    params%pass_number = 1
    params%max_passes = 1
    params%max_level = -1
    params%allow_root_keys = -1
    params%allow_empty_blocks = -1
    params%state = C_NULL_PTR
    params%initialise_all_blocks = 0
    params%unknown_block_is_fatal = -1
    params%unknown_key_is_fatal = -1
    params%non_value_line_is_blank = -1
    params%filename = C_NULL_PTR
    params%end_pass_on_error = -1
    params%finalize_on_error = -1
    params%bad_key_is_fatal = -1

  END SUBROUTINE eis_deck_parse_string_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for eis_deck_reparse to
  !> their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_deck_reparse_params_init(params) BIND(C)

    TYPE(eis_deck_reparse_params), INTENT(OUT) :: params

    params%pass_number = 1
    params%max_passes = 1
    params%max_level = -1
    params%allow_root_keys = -1
    params%allow_empty_blocks = -1
    params%state = C_NULL_PTR
    params%initialise_all_blocks = 0
    params%unknown_block_is_fatal = -1
    params%unknown_key_is_fatal = -1
    params%non_value_line_is_blank = -1
    params%end_pass_on_error = -1
    params%finalize_on_error = -1
    params%bad_key_is_fatal = -1

  END SUBROUTINE eis_deck_reparse_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_deck_parse_serialised to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_deck_parse_serialised_params_init(params) BIND(C)

    TYPE(eis_deck_parse_serialised_params), INTENT(OUT) :: params

    params%pass_number = 1
    params%max_passes = 1
    params%max_level = -1
    params%allow_root_keys = -1
    params%allow_empty_blocks = -1
    params%state = C_NULL_PTR
    params%initialise_all_blocks = 0
    params%unknown_block_is_fatal = -1
    params%unknown_key_is_fatal = -1
    params%non_value_line_is_blank = -1
    params%end_pass_on_error = -1
    params%finalize_on_error = -1
    params%bad_key_is_fatal = -1

  END SUBROUTINE eis_deck_parse_serialised_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_deck_get_serialised to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_deck_get_serialised_params_init(params) BIND(C)

    TYPE(eis_deck_get_serialised_params), INTENT(OUT) :: params

    params%filename_processor = C_NULL_FUNPTR
    params%file_text_processor = C_NULL_FUNPTR

  END SUBROUTINE eis_deck_get_serialised_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for eis_deck_initialise to
  !> their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_deck_initialise_params_init(params) BIND(C)

    TYPE(eis_deck_initialise_params), INTENT(OUT) :: params

    params%state = C_NULL_PTR
    params%pass_number = 1

  END SUBROUTINE eis_deck_initialise_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for eis_deck_finalise to
  !> their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_deck_finalise_params_init(params) BIND(C)

    TYPE(eis_deck_finalise_params), INTENT(OUT) :: params

    params%state = C_NULL_PTR
    params%pass_number = 1
    params%last_pass = 1

  END SUBROUTINE eis_deck_finalise_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_deck_get_error_count to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_deck_get_error_count_params_init(params) BIND(C)

    TYPE(eis_deck_get_error_count_params), INTENT(OUT) :: params

    params%dummy = 0

  END SUBROUTINE eis_deck_get_error_count_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_deck_get_error_report to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_deck_get_error_report_params_init(params) BIND(C)

    TYPE(eis_deck_get_error_report_params), INTENT(OUT) :: params

    params%dummy = 0

  END SUBROUTINE eis_deck_get_error_report_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_deck_flush_errors to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_deck_flush_errors_params_init(params) BIND(C)

    TYPE(eis_deck_flush_errors_params), INTENT(OUT) :: params

    params%dummy = 0

  END SUBROUTINE eis_deck_flush_errors_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_deck_get_parsed_block_name to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_deck_get_parsed_block_name_params_init(params) BIND(C)

    TYPE(eis_deck_get_parsed_block_name_params), INTENT(OUT) :: params

    params%dummy = 0

  END SUBROUTINE eis_deck_get_parsed_block_name_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_deck_get_parsed_block_parents to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_deck_get_parsed_block_parents_params_init(params) BIND(C)

    TYPE(eis_deck_get_parsed_block_parents_params), INTENT(OUT) :: params

    params%dummy = 0

  END SUBROUTINE eis_deck_get_parsed_block_parents_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_deck_get_block_structure to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_deck_get_block_structure_params_init(params) BIND(C)

    TYPE(eis_deck_get_block_structure_params), INTENT(OUT) :: params

    params%include_keys = 0
    params%max_depth = -1
    params%max_key_depth = -1

  END SUBROUTINE eis_deck_get_block_structure_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to set the parameters for
  !> eis_deck_release_strings to their default values
  !> @param[out] params - Parameters to set
  SUBROUTINE eis_deck_release_strings_params_init(params) BIND(C)

    TYPE(eis_deck_release_strings_params), INTENT(OUT) :: params

    params%dummy = 0

  END SUBROUTINE eis_deck_release_strings_params_init



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to create a text deck parser and return a unique
  !> ID that is used in future calls
  !> @param[inout] errcode - Error code
  !> @param[in] params - Pointer to an eis_deck_create_parser_params structure.
  !> NULL gives the default parameters. If parser_id is positive then it is the
  !> ID of the maths parser to use, otherwise the deck parser creates its own
  !> @return deck_id - Unique ID of the created parser. -1 on failure
  FUNCTION eis_deck_create_parser(errcode, params) BIND(C) RESULT(deck_id)

    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    INTEGER(C_INT) :: deck_id
    TYPE(eis_text_deck_parser), POINTER :: deck
    CLASS(eis_parser), POINTER :: parser
    INTEGER(eis_error) :: errcode_f
    TYPE(eis_deck_create_parser_params), POINTER :: p
    TYPE(eis_deck_create_parser_params), TARGET :: defaults

    errcode = INT(eis_err_none, eis_error_c)
    deck_id = -1

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_deck_create_parser_params_init(defaults)
      p => defaults
    END IF

    parser => NULL()
    IF (p%parser_id > 0) THEN
      parser => eis_get_interop_parser(INT(p%parser_id))
      IF (.NOT. ASSOCIATED(parser)) THEN
        errcode = IOR(errcode, INT(eis_err_bad_parser, eis_error_c))
        RETURN
      END IF
    END IF

    ALLOCATE(deck)
    errcode_f = eis_err_none
    IF (ASSOCIATED(parser)) THEN
      CALL deck%init(errcode_f, parser = parser, &
          unknown_block_is_fatal = (p%unknown_block_is_fatal /= 0), &
          unknown_key_is_fatal = (p%unknown_key_is_fatal /= 0), &
          bad_key_is_fatal = (p%bad_key_is_fatal /= 0), &
          non_value_line_is_blank = (p%non_value_line_is_blank /= 0), &
          end_pass_on_error = (p%end_pass_on_error /= 0), &
          finalize_on_error = (p%finalize_on_error /= 0), &
          host_params = p%host_params)
    ELSE
      CALL deck%init(errcode_f, &
          unknown_block_is_fatal = (p%unknown_block_is_fatal /= 0), &
          unknown_key_is_fatal = (p%unknown_key_is_fatal /= 0), &
          bad_key_is_fatal = (p%bad_key_is_fatal /= 0), &
          non_value_line_is_blank = (p%non_value_line_is_blank /= 0), &
          end_pass_on_error = (p%end_pass_on_error /= 0), &
          finalize_on_error = (p%finalize_on_error /= 0), &
          host_params = p%host_params)
    END IF
    errcode = IOR(errcode, INT(errcode_f, eis_error_c))
    deck_id = add_interop_deck_parser(deck)

  END FUNCTION eis_deck_create_parser



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Increment the reference count for a text deck parser
  !> @param[in] deck_id - ID of the parser
  !> @param[in] params - Pointer to an eis_deck_parser_inc_ref_params
  !> structure. NULL gives the default parameters. Currently unused
  SUBROUTINE eis_deck_parser_inc_ref(deck_id, params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: deck_id
    TYPE(C_PTR), VALUE, INTENT(IN) :: params

    IF (C_ASSOCIATED(params)) CONTINUE

    IF (ASSOCIATED(eis_deck_get_interop_parser(deck_id))) THEN
      interop_deck_parsers(deck_id)%refcount &
          = interop_deck_parsers(deck_id)%refcount + 1
    END IF

  END SUBROUTINE eis_deck_parser_inc_ref



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Decrement the reference count for a text deck parser. When the reference
  !> count reaches zero the parser is deallocated
  !> @param[in] deck_id - ID of the parser
  !> @param[in] params - Pointer to an eis_deck_parser_dec_ref_params
  !> structure. NULL gives the default parameters. Currently unused
  SUBROUTINE eis_deck_parser_dec_ref(deck_id, params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: deck_id
    TYPE(C_PTR), VALUE, INTENT(IN) :: params

    IF (C_ASSOCIATED(params)) CONTINUE

    IF (.NOT. ASSOCIATED(eis_deck_get_interop_parser(deck_id))) RETURN

    interop_deck_parsers(deck_id)%refcount = MAX(&
        interop_deck_parsers(deck_id)%refcount - 1, 0)
    IF (interop_deck_parsers(deck_id)%refcount == 0) THEN
      DEALLOCATE(interop_deck_parsers(deck_id)%contents)
      interop_deck_parsers(deck_id)%contents => NULL()
      interop_deck_parsers(deck_id)%in_use = .FALSE.
    END IF

  END SUBROUTINE eis_deck_parser_dec_ref



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to load a deck file and parse it against a deck
  !> definition
  !> @param[in] deck_id - ID of the text deck parser
  !> @param[in] definition_id - ID of the deck definition
  !> @param[in] filename - Name of the file to load
  !> @param[inout] errcode - Error code
  !> @param[in] params - Pointer to an eis_deck_parse_file_params structure.
  !> NULL gives the default parameters
  SUBROUTINE eis_deck_parse_file(deck_id, definition_id, filename, errcode, &
      params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: deck_id
    INTEGER(C_INT), VALUE, INTENT(IN) :: definition_id
    TYPE(C_PTR), VALUE, INTENT(IN) :: filename
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    TYPE(eis_text_deck_parser), POINTER :: deck
    TYPE(eis_deck_definition), POINTER :: definition
    CHARACTER(LEN=:), ALLOCATABLE :: filename_f
    INTEGER(eis_error) :: errcode_f
    INTEGER, POINTER :: pmax_level
    LOGICAL, TARGET :: root_keys, empty_blocks, block_fatal, key_fatal
    LOGICAL, TARGET :: non_value, pass_end, final_error, bad_fatal
    LOGICAL, POINTER :: proot_keys, pempty_blocks, pblock_fatal, pkey_fatal
    LOGICAL, POINTER :: pnon_value, ppass_end, pfinal_error, pbad_fatal
    INTEGER(eis_bitmask), TARGET :: state_f
    INTEGER(eis_bitmask), POINTER :: pstate
    INTEGER(eis_bitmask_c), POINTER :: cstate
    PROCEDURE(filename_processor_proto_c), POINTER :: c_fp
    PROCEDURE(file_text_processor_proto_c), POINTER :: c_ftp
    TYPE(eis_deck_parse_file_params), POINTER :: p
    TYPE(eis_deck_parse_file_params), TARGET :: defaults

    errcode = INT(eis_err_none, eis_error_c)

    deck => eis_deck_get_interop_parser(deck_id)
    definition => eis_deck_get_interop_definition(definition_id)
    IF (.NOT. ASSOCIATED(deck) .OR. .NOT. ASSOCIATED(definition) &
        .OR. .NOT. C_ASSOCIATED(filename)) THEN
      errcode = IOR(errcode, INT(eis_err_bad_value, eis_error_c))
      RETURN
    END IF

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_deck_parse_file_params_init(defaults)
      p => defaults
    END IF

    pmax_level => NULL()
    IF (p%max_level >= 0) pmax_level => p%max_level
    proot_keys => NULL()
    IF (p%allow_root_keys >= 0) THEN
      root_keys = (p%allow_root_keys /= 0)
      proot_keys => root_keys
    END IF
    pempty_blocks => NULL()
    IF (p%allow_empty_blocks >= 0) THEN
      empty_blocks = (p%allow_empty_blocks /= 0)
      pempty_blocks => empty_blocks
    END IF
    pblock_fatal => NULL()
    IF (p%unknown_block_is_fatal >= 0) THEN
      block_fatal = (p%unknown_block_is_fatal /= 0)
      pblock_fatal => block_fatal
    END IF
    pkey_fatal => NULL()
    IF (p%unknown_key_is_fatal >= 0) THEN
      key_fatal = (p%unknown_key_is_fatal /= 0)
      pkey_fatal => key_fatal
    END IF
    pnon_value => NULL()
    IF (p%non_value_line_is_blank >= 0) THEN
      non_value = (p%non_value_line_is_blank /= 0)
      pnon_value => non_value
    END IF
    ppass_end => NULL()
    IF (p%end_pass_on_error >= 0) THEN
      pass_end = (p%end_pass_on_error /= 0)
      ppass_end => pass_end
    END IF
    pfinal_error => NULL()
    IF (p%finalize_on_error >= 0) THEN
      final_error = (p%finalize_on_error /= 0)
      pfinal_error => final_error
    END IF
    pbad_fatal => NULL()
    IF (p%bad_key_is_fatal >= 0) THEN
      bad_fatal = (p%bad_key_is_fatal /= 0)
      pbad_fatal => bad_fatal
    END IF

    pstate => NULL()
    cstate => NULL()
    IF (C_ASSOCIATED(p%state)) THEN
      CALL C_F_POINTER(p%state, cstate)
      state_f = INT(cstate, eis_bitmask)
      pstate => state_f
    END IF

    c_fp => NULL()
    c_ftp => NULL()
    IF (C_ASSOCIATED(p%filename_processor)) &
        CALL C_F_PROCPOINTER(p%filename_processor, c_fp)
    IF (C_ASSOCIATED(p%file_text_processor)) &
        CALL C_F_PROCPOINTER(p%file_text_processor, c_ftp)

    CALL eis_c_f_string(filename, filename_f)
    errcode_f = eis_err_none
    CALL deck%parse_deck_file(filename_f, definition, errcode_f, &
        pass_number = INT(p%pass_number), max_passes = INT(p%max_passes), &
        max_level = pmax_level, allow_root_keys = proot_keys, &
        allow_empty_blocks = pempty_blocks, state = pstate, &
        initialise_all_blocks = (p%initialise_all_blocks /= 0), &
        unknown_block_is_fatal = pblock_fatal, &
        unknown_key_is_fatal = pkey_fatal, &
        non_value_line_is_blank = pnon_value, end_pass_on_error = ppass_end, &
        finalize_on_error = pfinal_error, bad_key_is_fatal = pbad_fatal, &
        c_filename_processor = c_fp, c_file_text_processor = c_ftp)
    errcode = IOR(errcode, INT(errcode_f, eis_error_c))
    IF (ASSOCIATED(pstate)) cstate = INT(state_f, eis_bitmask_c)

  END SUBROUTINE eis_deck_parse_file



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to parse the text of a deck held in a string
  !> against a deck definition
  !> @param[in] deck_id - ID of the text deck parser
  !> @param[in] definition_id - ID of the deck definition
  !> @param[in] text - Text of the deck
  !> @param[inout] errcode - Error code
  !> @param[in] params - Pointer to an eis_deck_parse_string_params structure.
  !> NULL gives the default parameters
  SUBROUTINE eis_deck_parse_string(deck_id, definition_id, text, errcode, &
      params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: deck_id
    INTEGER(C_INT), VALUE, INTENT(IN) :: definition_id
    TYPE(C_PTR), VALUE, INTENT(IN) :: text
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    TYPE(eis_text_deck_parser), POINTER :: deck
    TYPE(eis_deck_definition), POINTER :: definition
    CHARACTER(LEN=:), ALLOCATABLE :: text_f
    CHARACTER(LEN=:), ALLOCATABLE, TARGET :: filename_f
    CHARACTER(LEN=:), POINTER :: pfilename
    INTEGER(eis_error) :: errcode_f
    INTEGER, POINTER :: pmax_level
    LOGICAL, TARGET :: root_keys, empty_blocks, block_fatal, key_fatal
    LOGICAL, TARGET :: non_value, pass_end, final_error, bad_fatal
    LOGICAL, POINTER :: proot_keys, pempty_blocks, pblock_fatal, pkey_fatal
    LOGICAL, POINTER :: pnon_value, ppass_end, pfinal_error, pbad_fatal
    INTEGER(eis_bitmask), TARGET :: state_f
    INTEGER(eis_bitmask), POINTER :: pstate
    INTEGER(eis_bitmask_c), POINTER :: cstate
    TYPE(eis_deck_parse_string_params), POINTER :: p
    TYPE(eis_deck_parse_string_params), TARGET :: defaults

    errcode = INT(eis_err_none, eis_error_c)

    deck => eis_deck_get_interop_parser(deck_id)
    definition => eis_deck_get_interop_definition(definition_id)
    IF (.NOT. ASSOCIATED(deck) .OR. .NOT. ASSOCIATED(definition) &
        .OR. .NOT. C_ASSOCIATED(text)) THEN
      errcode = IOR(errcode, INT(eis_err_bad_value, eis_error_c))
      RETURN
    END IF

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_deck_parse_string_params_init(defaults)
      p => defaults
    END IF

    pmax_level => NULL()
    IF (p%max_level >= 0) pmax_level => p%max_level
    proot_keys => NULL()
    IF (p%allow_root_keys >= 0) THEN
      root_keys = (p%allow_root_keys /= 0)
      proot_keys => root_keys
    END IF
    pempty_blocks => NULL()
    IF (p%allow_empty_blocks >= 0) THEN
      empty_blocks = (p%allow_empty_blocks /= 0)
      pempty_blocks => empty_blocks
    END IF
    pblock_fatal => NULL()
    IF (p%unknown_block_is_fatal >= 0) THEN
      block_fatal = (p%unknown_block_is_fatal /= 0)
      pblock_fatal => block_fatal
    END IF
    pkey_fatal => NULL()
    IF (p%unknown_key_is_fatal >= 0) THEN
      key_fatal = (p%unknown_key_is_fatal /= 0)
      pkey_fatal => key_fatal
    END IF
    pnon_value => NULL()
    IF (p%non_value_line_is_blank >= 0) THEN
      non_value = (p%non_value_line_is_blank /= 0)
      pnon_value => non_value
    END IF
    ppass_end => NULL()
    IF (p%end_pass_on_error >= 0) THEN
      pass_end = (p%end_pass_on_error /= 0)
      ppass_end => pass_end
    END IF
    pfinal_error => NULL()
    IF (p%finalize_on_error >= 0) THEN
      final_error = (p%finalize_on_error /= 0)
      pfinal_error => final_error
    END IF
    pbad_fatal => NULL()
    IF (p%bad_key_is_fatal >= 0) THEN
      bad_fatal = (p%bad_key_is_fatal /= 0)
      pbad_fatal => bad_fatal
    END IF

    pstate => NULL()
    cstate => NULL()
    IF (C_ASSOCIATED(p%state)) THEN
      CALL C_F_POINTER(p%state, cstate)
      state_f = INT(cstate, eis_bitmask)
      pstate => state_f
    END IF

    pfilename => NULL()
    IF (C_ASSOCIATED(p%filename)) THEN
      CALL eis_c_f_string(p%filename, filename_f)
      pfilename => filename_f
    END IF

    CALL eis_c_f_string(text, text_f)
    errcode_f = eis_err_none
    CALL deck%parse_deck_string(text_f, definition, errcode_f, &
        pass_number = INT(p%pass_number), max_passes = INT(p%max_passes), &
        max_level = pmax_level, allow_root_keys = proot_keys, &
        allow_empty_blocks = pempty_blocks, state = pstate, &
        initialise_all_blocks = (p%initialise_all_blocks /= 0), &
        unknown_block_is_fatal = pblock_fatal, &
        unknown_key_is_fatal = pkey_fatal, &
        non_value_line_is_blank = pnon_value, filename = pfilename, &
        end_pass_on_error = ppass_end, finalize_on_error = pfinal_error, &
        bad_key_is_fatal = pbad_fatal)
    errcode = IOR(errcode, INT(errcode_f, eis_error_c))
    IF (ASSOCIATED(pstate)) cstate = INT(state_f, eis_bitmask_c)

  END SUBROUTINE eis_deck_parse_string



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to parse the deck that is already held in a text
  !> deck parser again against a deck definition. Used for multiple passes
  !> @param[in] deck_id - ID of the text deck parser
  !> @param[in] definition_id - ID of the deck definition
  !> @param[inout] errcode - Error code
  !> @param[in] params - Pointer to an eis_deck_reparse_params structure. NULL
  !> gives the default parameters
  SUBROUTINE eis_deck_reparse(deck_id, definition_id, errcode, params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: deck_id
    INTEGER(C_INT), VALUE, INTENT(IN) :: definition_id
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    TYPE(eis_text_deck_parser), POINTER :: deck
    TYPE(eis_deck_definition), POINTER :: definition
    INTEGER(eis_error) :: errcode_f
    INTEGER, POINTER :: pmax_level
    LOGICAL, TARGET :: root_keys, empty_blocks, block_fatal, key_fatal
    LOGICAL, TARGET :: non_value, pass_end, final_error, bad_fatal
    LOGICAL, POINTER :: proot_keys, pempty_blocks, pblock_fatal, pkey_fatal
    LOGICAL, POINTER :: pnon_value, ppass_end, pfinal_error, pbad_fatal
    INTEGER(eis_bitmask), TARGET :: state_f
    INTEGER(eis_bitmask), POINTER :: pstate
    INTEGER(eis_bitmask_c), POINTER :: cstate
    TYPE(eis_deck_reparse_params), POINTER :: p
    TYPE(eis_deck_reparse_params), TARGET :: defaults

    errcode = INT(eis_err_none, eis_error_c)

    deck => eis_deck_get_interop_parser(deck_id)
    definition => eis_deck_get_interop_definition(definition_id)
    IF (.NOT. ASSOCIATED(deck) .OR. .NOT. ASSOCIATED(definition)) THEN
      errcode = IOR(errcode, INT(eis_err_bad_value, eis_error_c))
      RETURN
    END IF

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_deck_reparse_params_init(defaults)
      p => defaults
    END IF

    pmax_level => NULL()
    IF (p%max_level >= 0) pmax_level => p%max_level
    proot_keys => NULL()
    IF (p%allow_root_keys >= 0) THEN
      root_keys = (p%allow_root_keys /= 0)
      proot_keys => root_keys
    END IF
    pempty_blocks => NULL()
    IF (p%allow_empty_blocks >= 0) THEN
      empty_blocks = (p%allow_empty_blocks /= 0)
      pempty_blocks => empty_blocks
    END IF
    pblock_fatal => NULL()
    IF (p%unknown_block_is_fatal >= 0) THEN
      block_fatal = (p%unknown_block_is_fatal /= 0)
      pblock_fatal => block_fatal
    END IF
    pkey_fatal => NULL()
    IF (p%unknown_key_is_fatal >= 0) THEN
      key_fatal = (p%unknown_key_is_fatal /= 0)
      pkey_fatal => key_fatal
    END IF
    pnon_value => NULL()
    IF (p%non_value_line_is_blank >= 0) THEN
      non_value = (p%non_value_line_is_blank /= 0)
      pnon_value => non_value
    END IF
    ppass_end => NULL()
    IF (p%end_pass_on_error >= 0) THEN
      pass_end = (p%end_pass_on_error /= 0)
      ppass_end => pass_end
    END IF
    pfinal_error => NULL()
    IF (p%finalize_on_error >= 0) THEN
      final_error = (p%finalize_on_error /= 0)
      pfinal_error => final_error
    END IF
    pbad_fatal => NULL()
    IF (p%bad_key_is_fatal >= 0) THEN
      bad_fatal = (p%bad_key_is_fatal /= 0)
      pbad_fatal => bad_fatal
    END IF

    pstate => NULL()
    cstate => NULL()
    IF (C_ASSOCIATED(p%state)) THEN
      CALL C_F_POINTER(p%state, cstate)
      state_f = INT(cstate, eis_bitmask)
      pstate => state_f
    END IF

    errcode_f = eis_err_none
    CALL deck%reparse_deck(definition, errcode_f, &
        pass_number = INT(p%pass_number), max_passes = INT(p%max_passes), &
        max_level = pmax_level, allow_root_keys = proot_keys, &
        allow_empty_blocks = pempty_blocks, state = pstate, &
        initialise_all_blocks = (p%initialise_all_blocks /= 0), &
        unknown_block_is_fatal = pblock_fatal, &
        unknown_key_is_fatal = pkey_fatal, bad_key_is_fatal = pbad_fatal, &
        non_value_line_is_blank = pnon_value, end_pass_on_error = ppass_end, &
        finalize_on_error = pfinal_error)
    errcode = IOR(errcode, INT(errcode_f, eis_error_c))
    IF (ASSOCIATED(pstate)) cstate = INT(state_f, eis_bitmask_c)

  END SUBROUTINE eis_deck_reparse



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to parse a serialised deck against a deck
  !> definition. The serialised text contains null characters so it is given
  !> as a pointer and a length
  !> @param[in] deck_id - ID of the text deck parser
  !> @param[in] definition_id - ID of the deck definition
  !> @param[in] serial_text - Serialised deck from eis_deck_get_serialised
  !> @param[in] text_length - Length of the serialised deck in characters
  !> @param[inout] errcode - Error code
  !> @param[in] params - Pointer to an eis_deck_parse_serialised_params
  !> structure. NULL gives the default parameters
  SUBROUTINE eis_deck_parse_serialised(deck_id, definition_id, serial_text, &
      text_length, errcode, params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: deck_id
    INTEGER(C_INT), VALUE, INTENT(IN) :: definition_id
    TYPE(C_PTR), VALUE, INTENT(IN) :: serial_text
    INTEGER(C_INT), VALUE, INTENT(IN) :: text_length
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    TYPE(eis_text_deck_parser), POINTER :: deck
    TYPE(eis_deck_definition), POINTER :: definition
    CHARACTER(KIND=C_CHAR), DIMENSION(:), POINTER :: serial_chars
    CHARACTER(LEN=:), ALLOCATABLE :: serial_f
    INTEGER(eis_error) :: errcode_f
    INTEGER :: i
    INTEGER, POINTER :: pmax_level
    LOGICAL, TARGET :: root_keys, empty_blocks, block_fatal, key_fatal
    LOGICAL, TARGET :: non_value, pass_end, final_error, bad_fatal
    LOGICAL, POINTER :: proot_keys, pempty_blocks, pblock_fatal, pkey_fatal
    LOGICAL, POINTER :: pnon_value, ppass_end, pfinal_error, pbad_fatal
    INTEGER(eis_bitmask), TARGET :: state_f
    INTEGER(eis_bitmask), POINTER :: pstate
    INTEGER(eis_bitmask_c), POINTER :: cstate
    TYPE(eis_deck_parse_serialised_params), POINTER :: p
    TYPE(eis_deck_parse_serialised_params), TARGET :: defaults

    errcode = INT(eis_err_none, eis_error_c)

    deck => eis_deck_get_interop_parser(deck_id)
    definition => eis_deck_get_interop_definition(definition_id)
    IF (.NOT. ASSOCIATED(deck) .OR. .NOT. ASSOCIATED(definition) &
        .OR. .NOT. C_ASSOCIATED(serial_text) .OR. text_length < 0) THEN
      errcode = IOR(errcode, INT(eis_err_bad_value, eis_error_c))
      RETURN
    END IF

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_deck_parse_serialised_params_init(defaults)
      p => defaults
    END IF

    pmax_level => NULL()
    IF (p%max_level >= 0) pmax_level => p%max_level
    proot_keys => NULL()
    IF (p%allow_root_keys >= 0) THEN
      root_keys = (p%allow_root_keys /= 0)
      proot_keys => root_keys
    END IF
    pempty_blocks => NULL()
    IF (p%allow_empty_blocks >= 0) THEN
      empty_blocks = (p%allow_empty_blocks /= 0)
      pempty_blocks => empty_blocks
    END IF
    pblock_fatal => NULL()
    IF (p%unknown_block_is_fatal >= 0) THEN
      block_fatal = (p%unknown_block_is_fatal /= 0)
      pblock_fatal => block_fatal
    END IF
    pkey_fatal => NULL()
    IF (p%unknown_key_is_fatal >= 0) THEN
      key_fatal = (p%unknown_key_is_fatal /= 0)
      pkey_fatal => key_fatal
    END IF
    pnon_value => NULL()
    IF (p%non_value_line_is_blank >= 0) THEN
      non_value = (p%non_value_line_is_blank /= 0)
      pnon_value => non_value
    END IF
    ppass_end => NULL()
    IF (p%end_pass_on_error >= 0) THEN
      pass_end = (p%end_pass_on_error /= 0)
      ppass_end => pass_end
    END IF
    pfinal_error => NULL()
    IF (p%finalize_on_error >= 0) THEN
      final_error = (p%finalize_on_error /= 0)
      pfinal_error => final_error
    END IF
    pbad_fatal => NULL()
    IF (p%bad_key_is_fatal >= 0) THEN
      bad_fatal = (p%bad_key_is_fatal /= 0)
      pbad_fatal => bad_fatal
    END IF

    pstate => NULL()
    cstate => NULL()
    IF (C_ASSOCIATED(p%state)) THEN
      CALL C_F_POINTER(p%state, cstate)
      state_f = INT(cstate, eis_bitmask)
      pstate => state_f
    END IF

    CALL C_F_POINTER(serial_text, serial_chars, [INT(text_length)])
    ALLOCATE(CHARACTER(LEN=text_length) :: serial_f)
    DO i = 1, text_length
      serial_f(i:i) = serial_chars(i)
    END DO

    errcode_f = eis_err_none
    CALL deck%parse_deck_serialised(serial_f, definition, errcode_f, &
        pass_number = INT(p%pass_number), max_passes = INT(p%max_passes), &
        max_level = pmax_level, allow_root_keys = proot_keys, &
        allow_empty_blocks = pempty_blocks, state = pstate, &
        initialise_all_blocks = (p%initialise_all_blocks /= 0), &
        unknown_block_is_fatal = pblock_fatal, &
        unknown_key_is_fatal = pkey_fatal, bad_key_is_fatal = pbad_fatal, &
        non_value_line_is_blank = pnon_value, end_pass_on_error = ppass_end, &
        finalize_on_error = pfinal_error)
    errcode = IOR(errcode, INT(errcode_f, eis_error_c))
    IF (ASSOCIATED(pstate)) cstate = INT(state_f, eis_bitmask_c)

  END SUBROUTINE eis_deck_parse_serialised



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to load a deck file and get a serialised
  !> representation of it that can be sent to other processes. The
  !> serialised text contains null characters so always use the returned length
  !> @param[in] deck_id - ID of the text deck parser
  !> @param[in] filename - Name of the file to load
  !> @param[in] text_length - Length of the buffer that "text" points to
  !> @param[inout] text - Pointer to the buffer to be populated. Passing NULL
  !> is acceptable to only probe for the required length
  !> @param[inout] errcode - Error code
  !> @param[in] params - Pointer to an eis_deck_get_serialised_params
  !> structure. NULL gives the default parameters
  !> @return eis_deck_get_serialised - Number of characters in the serialised
  !> deck. -1 if the deck could not be serialised
  FUNCTION eis_deck_get_serialised(deck_id, filename, text_length, text, &
      errcode, params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: deck_id
    TYPE(C_PTR), VALUE, INTENT(IN) :: filename
    INTEGER(C_INT), VALUE, INTENT(IN) :: text_length
    TYPE(C_PTR), VALUE, INTENT(IN) :: text
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    INTEGER(C_INT) :: eis_deck_get_serialised
    TYPE(eis_text_deck_parser), POINTER :: deck
    CHARACTER(LEN=:), ALLOCATABLE :: filename_f, serial_f
    CHARACTER(KIND=C_CHAR), DIMENSION(:), POINTER :: text_chars
    INTEGER(eis_error) :: errcode_f
    INTEGER :: i, copy_length
    PROCEDURE(filename_processor_proto_c), POINTER :: c_fp
    PROCEDURE(file_text_processor_proto_c), POINTER :: c_ftp
    TYPE(eis_deck_get_serialised_params), POINTER :: p
    TYPE(eis_deck_get_serialised_params), TARGET :: defaults

    errcode = INT(eis_err_none, eis_error_c)
    eis_deck_get_serialised = -1

    deck => eis_deck_get_interop_parser(deck_id)
    IF (.NOT. ASSOCIATED(deck) .OR. .NOT. C_ASSOCIATED(filename)) THEN
      errcode = IOR(errcode, INT(eis_err_bad_value, eis_error_c))
      RETURN
    END IF

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_deck_get_serialised_params_init(defaults)
      p => defaults
    END IF

    c_fp => NULL()
    c_ftp => NULL()
    IF (C_ASSOCIATED(p%filename_processor)) &
        CALL C_F_PROCPOINTER(p%filename_processor, c_fp)
    IF (C_ASSOCIATED(p%file_text_processor)) &
        CALL C_F_PROCPOINTER(p%file_text_processor, c_ftp)

    CALL eis_c_f_string(filename, filename_f)
    errcode_f = eis_err_none
    CALL deck%get_deck_serialised(filename_f, serial_f, errcode_f, &
        c_filename_processor = c_fp, c_file_text_processor = c_ftp)
    errcode = IOR(errcode, INT(errcode_f, eis_error_c))
    IF (.NOT. ALLOCATED(serial_f)) RETURN

    eis_deck_get_serialised = LEN(serial_f)
    IF (C_ASSOCIATED(text) .AND. text_length > 0) THEN
      CALL C_F_POINTER(text, text_chars, [INT(text_length)])
      copy_length = MIN(LEN(serial_f), text_length - 1)
      DO i = 1, copy_length
        text_chars(i) = serial_f(i:i)
      END DO
      text_chars(copy_length + 1) = C_NULL_CHAR
    END IF

  END FUNCTION eis_deck_get_serialised



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to initialise the blocks in a deck definition
  !> @param[in] deck_id - ID of the text deck parser
  !> @param[in] definition_id - ID of the deck definition
  !> @param[inout] errcode - Error code
  !> @param[in] params - Pointer to an eis_deck_initialise_params structure.
  !> NULL gives the default parameters
  SUBROUTINE eis_deck_initialise(deck_id, definition_id, errcode, params) &
      BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: deck_id
    INTEGER(C_INT), VALUE, INTENT(IN) :: definition_id
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    TYPE(eis_text_deck_parser), POINTER :: deck
    TYPE(eis_deck_definition), POINTER :: definition
    INTEGER(eis_error) :: errcode_f
    INTEGER(eis_bitmask), TARGET :: state_f
    INTEGER(eis_bitmask), POINTER :: pstate
    INTEGER(eis_bitmask_c), POINTER :: cstate
    TYPE(eis_deck_initialise_params), POINTER :: p
    TYPE(eis_deck_initialise_params), TARGET :: defaults

    errcode = INT(eis_err_none, eis_error_c)

    deck => eis_deck_get_interop_parser(deck_id)
    definition => eis_deck_get_interop_definition(definition_id)
    IF (.NOT. ASSOCIATED(deck) .OR. .NOT. ASSOCIATED(definition)) THEN
      errcode = IOR(errcode, INT(eis_err_bad_value, eis_error_c))
      RETURN
    END IF

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_deck_initialise_params_init(defaults)
      p => defaults
    END IF

    pstate => NULL()
    cstate => NULL()
    IF (C_ASSOCIATED(p%state)) THEN
      CALL C_F_POINTER(p%state, cstate)
      state_f = INT(cstate, eis_bitmask)
      pstate => state_f
    END IF

    CALL deck%initialise_deck(definition, errcode_f, state = pstate, &
        pass_number = INT(p%pass_number))
    errcode = IOR(errcode, INT(errcode_f, eis_error_c))
    IF (ASSOCIATED(pstate)) cstate = INT(state_f, eis_bitmask_c)

  END SUBROUTINE eis_deck_initialise



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to finalise the blocks in a deck definition
  !> @param[in] deck_id - ID of the text deck parser
  !> @param[in] definition_id - ID of the deck definition
  !> @param[inout] errcode - Error code
  !> @param[in] params - Pointer to an eis_deck_finalise_params structure. NULL
  !> gives the default parameters
  SUBROUTINE eis_deck_finalise(deck_id, definition_id, errcode, params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: deck_id
    INTEGER(C_INT), VALUE, INTENT(IN) :: definition_id
    INTEGER(eis_error_c), INTENT(INOUT) :: errcode
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    TYPE(eis_text_deck_parser), POINTER :: deck
    TYPE(eis_deck_definition), POINTER :: definition
    INTEGER(eis_error) :: errcode_f
    INTEGER(eis_bitmask), TARGET :: state_f
    INTEGER(eis_bitmask), POINTER :: pstate
    INTEGER(eis_bitmask_c), POINTER :: cstate
    TYPE(eis_deck_finalise_params), POINTER :: p
    TYPE(eis_deck_finalise_params), TARGET :: defaults

    errcode = INT(eis_err_none, eis_error_c)

    deck => eis_deck_get_interop_parser(deck_id)
    definition => eis_deck_get_interop_definition(definition_id)
    IF (.NOT. ASSOCIATED(deck) .OR. .NOT. ASSOCIATED(definition)) THEN
      errcode = IOR(errcode, INT(eis_err_bad_value, eis_error_c))
      RETURN
    END IF

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_deck_finalise_params_init(defaults)
      p => defaults
    END IF

    pstate => NULL()
    cstate => NULL()
    IF (C_ASSOCIATED(p%state)) THEN
      CALL C_F_POINTER(p%state, cstate)
      state_f = INT(cstate, eis_bitmask)
      pstate => state_f
    END IF

    CALL deck%finalise_deck(definition, errcode_f, state = pstate, &
        pass_number = INT(p%pass_number), last_pass = (p%last_pass /= 0))
    errcode = IOR(errcode, INT(errcode_f, eis_error_c))
    IF (ASSOCIATED(pstate)) cstate = INT(state_f, eis_bitmask_c)

  END SUBROUTINE eis_deck_finalise



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to get the number of errors reported on a text
  !> deck parser
  !> @param[in] deck_id - ID of the text deck parser
  !> @param[in] params - Pointer to an eis_deck_get_error_count_params
  !> structure. NULL gives the default parameters. Currently unused
  !> @return eis_deck_get_error_count - Number of errors. 0 if the ID is
  !> invalid
  FUNCTION eis_deck_get_error_count(deck_id, params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: deck_id
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    INTEGER(C_INT) :: eis_deck_get_error_count
    TYPE(eis_text_deck_parser), POINTER :: deck

    IF (C_ASSOCIATED(params)) CONTINUE

    eis_deck_get_error_count = 0
    deck => eis_deck_get_interop_parser(deck_id)
    IF (.NOT. ASSOCIATED(deck)) RETURN
    eis_deck_get_error_count = deck%get_error_count()

  END FUNCTION eis_deck_get_error_count



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to get the report on a specified error. The
  !> error ID is an integer between 1 and the result from
  !> eis_deck_get_error_count
  !> @param[in] deck_id - ID of the text deck parser
  !> @param[in] error_id - ID of the error
  !> @param[in] buflen - Size of the output buffer
  !> @param[out] string_out - char* array of at least length buflen that holds
  !> the report. It is always null terminated unless buflen = 0
  !> @param[in] params - Pointer to an eis_deck_get_error_report_params
  !> structure. NULL gives the default parameters. Currently unused
  !> @return eis_deck_get_error_report - Length of the error report + 1. A
  !> buffer at least this long holds the full report and the null terminator
  FUNCTION eis_deck_get_error_report(deck_id, error_id, buflen, string_out, &
      params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: deck_id, error_id, buflen
    TYPE(C_PTR), VALUE, INTENT(IN) :: string_out
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    INTEGER(C_INT) :: eis_deck_get_error_report
    CHARACTER(LEN=:), ALLOCATABLE :: errstr
    TYPE(eis_text_deck_parser), POINTER :: deck

    IF (C_ASSOCIATED(params)) CONTINUE

    eis_deck_get_error_report = 0
    deck => eis_deck_get_interop_parser(deck_id)
    IF (.NOT. ASSOCIATED(deck)) RETURN
    CALL deck%get_error_report(error_id, errstr)
    IF (.NOT. ALLOCATED(errstr)) RETURN
    CALL eis_f_c_string(errstr, buflen, string_out)
    eis_deck_get_error_report = LEN(errstr) + 1

  END FUNCTION eis_deck_get_error_report



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to empty the list of errors on a text deck
  !> parser
  !> @param[in] deck_id - ID of the text deck parser
  !> @param[in] params - Pointer to an eis_deck_flush_errors_params structure.
  !> NULL gives the default parameters. Currently unused
  SUBROUTINE eis_deck_flush_errors(deck_id, params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: deck_id
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    TYPE(eis_text_deck_parser), POINTER :: deck

    IF (C_ASSOCIATED(params)) CONTINUE

    deck => eis_deck_get_interop_parser(deck_id)
    IF (.NOT. ASSOCIATED(deck)) RETURN
    CALL deck%flush_errors()

  END SUBROUTINE eis_deck_flush_errors



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to get the name of a block of the deck that has
  !> been loaded into a text deck parser
  !> @param[in] deck_id - ID of the text deck parser
  !> @param[in] block_id - ID of the block in the loaded deck
  !> @param[in] text_length - Length of the buffer that "text" points to
  !> @param[inout] text - Pointer to string to be populated. Passing NULL
  !> is acceptable to only probe for the required length of string
  !> @param[in] params - Pointer to an eis_deck_get_parsed_block_name_params
  !> structure. NULL gives the default parameters. Currently unused
  !> @return eis_deck_get_parsed_block_name - Number of characters needed to
  !> hold the name. -1 if there is no such block
  FUNCTION eis_deck_get_parsed_block_name(deck_id, block_id, text_length, &
      text, params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: deck_id
    INTEGER(C_INT), VALUE, INTENT(IN) :: block_id
    INTEGER(C_INT), VALUE, INTENT(IN) :: text_length
    TYPE(C_PTR), VALUE, INTENT(IN) :: text
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    INTEGER(C_INT) :: eis_deck_get_parsed_block_name
    TYPE(eis_text_deck_parser), POINTER :: deck
    CHARACTER(LEN=:), ALLOCATABLE :: fstr

    IF (C_ASSOCIATED(params)) CONTINUE

    eis_deck_get_parsed_block_name = -1
    deck => eis_deck_get_interop_parser(deck_id)
    IF (.NOT. ASSOCIATED(deck)) RETURN

    CALL deck%get_block_name(block_id, fstr)
    IF (.NOT. ALLOCATED(fstr)) RETURN
    eis_deck_get_parsed_block_name = LEN(fstr)
    IF (C_ASSOCIATED(text)) CALL eis_f_c_string(fstr, text_length, text)

  END FUNCTION eis_deck_get_parsed_block_name



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to get the IDs of the parents of a block of the
  !> deck that has been loaded into a text deck parser. The last element is the
  !> ID of the block itself
  !> @param[in] deck_id - ID of the text deck parser
  !> @param[in] block_id - ID of the block in the loaded deck
  !> @param[in] array_length - Length of the array that "parents" points to
  !> @param[inout] parents - Pointer to the array to be populated. Passing NULL
  !> is acceptable to only probe for the required length
  !> @param[in] params - Pointer to an
  !> eis_deck_get_parsed_block_parents_params structure. NULL gives the
  !> default parameters. Currently unused
  !> @return eis_deck_get_parsed_block_parents - Number of IDs. -1 if there is
  !> no such block
  FUNCTION eis_deck_get_parsed_block_parents(deck_id, block_id, array_length, &
      parents, params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: deck_id
    INTEGER(C_INT), VALUE, INTENT(IN) :: block_id
    INTEGER(C_INT), VALUE, INTENT(IN) :: array_length
    TYPE(C_PTR), VALUE, INTENT(IN) :: parents
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    INTEGER(C_INT) :: eis_deck_get_parsed_block_parents
    TYPE(eis_text_deck_parser), POINTER :: deck
    INTEGER, DIMENSION(:), ALLOCATABLE :: parents_f
    INTEGER(C_INT), DIMENSION(:), POINTER :: parents_c
    INTEGER :: copy_length

    IF (C_ASSOCIATED(params)) CONTINUE

    eis_deck_get_parsed_block_parents = -1
    deck => eis_deck_get_interop_parser(deck_id)
    IF (.NOT. ASSOCIATED(deck)) RETURN

    CALL deck%get_block_parents(block_id, parents_f)
    IF (.NOT. ALLOCATED(parents_f)) RETURN
    eis_deck_get_parsed_block_parents = SIZE(parents_f)
    IF (C_ASSOCIATED(parents) .AND. array_length > 0) THEN
      CALL C_F_POINTER(parents, parents_c, [INT(array_length)])
      copy_length = MIN(SIZE(parents_f), INT(array_length))
      parents_c(1:copy_length) = INT(parents_f(1:copy_length), C_INT)
    END IF

  END FUNCTION eis_deck_get_parsed_block_parents



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to get a graphviz dot representation of the
  !> blocks, and optionally the keys, of the deck that has been loaded into a
  !> text deck parser
  !> @param[in] deck_id - ID of the text deck parser
  !> @param[in] text_length - Length of the buffer that "text" points to
  !> @param[inout] text - Pointer to string to be populated. Passing NULL
  !> is acceptable to only probe for the required length of string
  !> @param[in] params - Pointer to an eis_deck_get_block_structure_params
  !> structure. NULL gives the default parameters
  !> @return eis_deck_get_block_structure - Number of characters needed to hold
  !> the representation. -1 if no deck has been loaded
  FUNCTION eis_deck_get_block_structure(deck_id, text_length, text, params) &
      BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: deck_id
    INTEGER(C_INT), VALUE, INTENT(IN) :: text_length
    TYPE(C_PTR), VALUE, INTENT(IN) :: text
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    INTEGER(C_INT) :: eis_deck_get_block_structure
    TYPE(eis_text_deck_parser), POINTER :: deck
    CHARACTER(LEN=:), ALLOCATABLE :: fstr
    INTEGER, POINTER :: pmax_depth, pmax_key_depth
    TYPE(eis_deck_get_block_structure_params), POINTER :: p
    TYPE(eis_deck_get_block_structure_params), TARGET :: defaults

    eis_deck_get_block_structure = -1
    deck => eis_deck_get_interop_parser(deck_id)
    IF (.NOT. ASSOCIATED(deck)) RETURN

    IF (C_ASSOCIATED(params)) THEN
      CALL C_F_POINTER(params, p)
    ELSE
      CALL eis_deck_get_block_structure_params_init(defaults)
      p => defaults
    END IF

    pmax_depth => NULL()
    IF (p%max_depth >= 0) pmax_depth => p%max_depth
    pmax_key_depth => NULL()
    IF (p%max_key_depth >= 0) pmax_key_depth => p%max_key_depth

    CALL deck%get_block_structure(fstr, include_keys = (p%include_keys /= 0), &
        max_depth = pmax_depth, max_key_depth = pmax_key_depth)
    IF (.NOT. ALLOCATED(fstr)) RETURN
    eis_deck_get_block_structure = LEN(fstr)
    IF (C_ASSOCIATED(text)) CALL eis_f_c_string(fstr, text_length, text)

  END FUNCTION eis_deck_get_block_structure



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> C interoperable function to release the string representation of the
  !> deck that is held in a text deck parser
  !> @param[in] deck_id - ID of the text deck parser
  !> @param[in] params - Pointer to an eis_deck_release_strings_params
  !> structure. NULL gives the default parameters. Currently unused
  SUBROUTINE eis_deck_release_strings(deck_id, params) BIND(C)

    INTEGER(C_INT), VALUE, INTENT(IN) :: deck_id
    TYPE(C_PTR), VALUE, INTENT(IN) :: params
    TYPE(eis_text_deck_parser), POINTER :: deck

    IF (C_ASSOCIATED(params)) CONTINUE

    deck => eis_deck_get_interop_parser(deck_id)
    IF (.NOT. ASSOCIATED(deck)) RETURN
    CALL deck%release_strings()

  END SUBROUTINE eis_deck_release_strings

END MODULE eis_deck_text_interop
#else
#error Unsupported EIS_C_INTEROP_VERSION
#endif
