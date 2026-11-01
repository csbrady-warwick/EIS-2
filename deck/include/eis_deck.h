// Copyright (c) 2019-2026, C. S. Brady and H. Ratcliffe
// SPDX-License-Identifier: BSD-3-Clause

#ifndef EIS_INTEROP_DECK_H
#define EIS_INTEROP_DECK_H
#include "interop_version.inc"
#if (EIS_C_INTEROP_VERSION <= 2)
#include <stdint.h>
#include "eis.h"

  /*Definitions of the callback functions that can be attached to a deck
  definition*/
  typedef void (*deck_block_generic_fn)(char*, int*, int, int*, EIS_STATUS*,
      EIS_BITMASK*, EIS_ERROR*);
  typedef void (*deck_block_fn)(char*, int*, int, int*, int*, EIS_STATUS*,
      EIS_BITMASK*, EIS_ERROR*);
  typedef void (*deck_block_remap_fn)(char*, int*, int, char*, EIS_STATUS*,
      EIS_BITMASK*, EIS_ERROR*);
  typedef int (*deck_should_key_trigger_fn)(char*, int*, int, int*, int*,
      EIS_STATUS*, EIS_BITMASK*, EIS_ERROR*);
  typedef void (*deck_key_text_fn)(char*, int*, int, int*, int*, EIS_STATUS*,
      EIS_BITMASK*, EIS_ERROR*);
  typedef void (*deck_key_value_fn)(char*, char*, int*, int, int*, int*,
      EIS_STATUS*, EIS_BITMASK*, EIS_ERROR*);
  typedef void (*deck_key_numeric_value_fn)(char*, int, EIS_NUM*, int*,
      EIS_BITMASK, int, int, int*, int*, EIS_STATUS*, EIS_BITMASK*,
      EIS_ERROR*);
  typedef void (*deck_key_stack_fn)(char*, int, int*, int, int, int*, int*,
      EIS_STATUS*, EIS_BITMASK*, EIS_ERROR*);
  typedef void (*deck_event_fn)(char*, int*, int, int*, int*, EIS_STATUS*,
      EIS_BITMASK*, EIS_ERROR*);
  typedef void (*deck_generic_event_fn)(char*, int*, int, int*, EIS_STATUS*,
      EIS_BITMASK*, EIS_ERROR*);

  /*Definitions of the callback functions that process file names and file
  text when a deck file is loaded*/
  typedef void (*deck_filename_processor_fn)(char*, int, EIS_ERROR*);
  typedef int (*deck_file_text_processor_fn)(char*, char*, int, EIS_ERROR*);

  /*Parameter structures for the routines in the interoperable interface.
  Each routine takes a pointer to its structure as the last argument. Passing
  NULL gives the default parameters. Use the matching "_init" routine to set a
  structure to the default parameters before changing individual members*/
  typedef struct {
    deck_block_generic_fn init_deck;
    deck_block_generic_fn start_pass;
    deck_block_fn start_deck;
    deck_block_fn end_deck;
    deck_block_generic_fn end_pass;
    deck_block_generic_fn final_deck;
    deck_key_text_fn any_key_text;
    deck_key_value_fn any_key_value;
    deck_key_numeric_value_fn any_key_numeric_value;
    deck_key_stack_fn any_key_stack;
    deck_block_remap_fn block_remapper;
    deck_event_fn on_key_success;
    deck_event_fn on_key_failure;
    deck_event_fn on_key_no_trigger;
    deck_event_fn on_block_start;
    deck_event_fn on_block_end;
    deck_event_fn on_block_no_trigger;
    deck_event_fn on_block_failure;
    deck_generic_event_fn on_init;
    deck_generic_event_fn on_start_pass;
    deck_generic_event_fn on_end_pass;
    deck_generic_event_fn on_final;
    deck_generic_event_fn on_generic_block_failure;
    int interop_parser;
    int text_fallback;
    int no_interop;
  } eis_deck_create_definition_params;

  typedef struct {
    int dummy;
  } eis_deck_definition_inc_ref_params;

  typedef struct {
    int dummy;
  } eis_deck_definition_dec_ref_params;

  typedef struct {
    int dummy;
  } eis_deck_get_root_block_params;

  typedef struct {
    deck_block_generic_fn init_block;
    deck_block_generic_fn start_pass;
    deck_block_fn start_block;
    deck_block_fn end_block;
    deck_block_generic_fn end_pass;
    deck_block_generic_fn final_block;
    deck_key_text_fn any_key_text;
    deck_key_value_fn any_key_value;
    deck_key_numeric_value_fn any_key_numeric_value;
    deck_key_stack_fn any_key_stack;
    deck_block_remap_fn block_remapper;
    int pass_eq;
    int pass_le;
    int pass_ge;
    int* init_flag;
    int32_t* i32count;
    int64_t* i64count;
    char* description;
    int hidden;
    int text_fallback;
  } eis_deck_add_block_params;

  typedef struct {
    deck_key_text_fn key_text_fn;
    deck_key_value_fn key_value_fn;
    deck_key_numeric_value_fn key_numeric_value_fn;
    deck_key_stack_fn key_stack_fn;
    deck_should_key_trigger_fn should_key_trigger_fn;
    int32_t* i32value;
    int i32len;
    int64_t* i64value;
    int i64len;
    float* r32value;
    int r32len;
    double* r64value;
    int r64len;
    int stackvalue;
    int* init_flag;
    int32_t* i32count;
    int64_t* i64count;
    int expected_params;
    int pass_eq;
    int pass_le;
    int pass_ge;
    char* description;
    int hidden;
    int text_fallback;
  } eis_deck_add_key_params;

  typedef struct {
    int dummy;
  } eis_deck_optimise_definition_params;

  typedef struct {
    int dummy;
  } eis_deck_visualise_definition_params;

  typedef struct {
    char* title;
  } eis_deck_markdown_definition_params;

  typedef struct {
    int dummy;
  } eis_deck_get_block_name_params;

  typedef struct {
    int parser_id;
    void* host_params;
    int unknown_block_is_fatal;
    int unknown_key_is_fatal;
    int bad_key_is_fatal;
    int non_value_line_is_blank;
    int end_pass_on_error;
    int finalize_on_error;
  } eis_deck_create_parser_params;

  typedef struct {
    int dummy;
  } eis_deck_parser_inc_ref_params;

  typedef struct {
    int dummy;
  } eis_deck_parser_dec_ref_params;

  typedef struct {
    int pass_number;
    int max_passes;
    int max_level;
    int allow_root_keys;
    int allow_empty_blocks;
    EIS_BITMASK* state;
    int initialise_all_blocks;
    int unknown_block_is_fatal;
    int unknown_key_is_fatal;
    int non_value_line_is_blank;
    deck_filename_processor_fn filename_processor;
    deck_file_text_processor_fn file_text_processor;
    int end_pass_on_error;
    int finalize_on_error;
    int bad_key_is_fatal;
  } eis_deck_parse_file_params;

  typedef struct {
    int pass_number;
    int max_passes;
    int max_level;
    int allow_root_keys;
    int allow_empty_blocks;
    EIS_BITMASK* state;
    int initialise_all_blocks;
    int unknown_block_is_fatal;
    int unknown_key_is_fatal;
    int non_value_line_is_blank;
    char* filename;
    int end_pass_on_error;
    int finalize_on_error;
    int bad_key_is_fatal;
  } eis_deck_parse_string_params;

  typedef struct {
    int pass_number;
    int max_passes;
    int max_level;
    int allow_root_keys;
    int allow_empty_blocks;
    EIS_BITMASK* state;
    int initialise_all_blocks;
    int unknown_block_is_fatal;
    int unknown_key_is_fatal;
    int non_value_line_is_blank;
    int end_pass_on_error;
    int finalize_on_error;
    int bad_key_is_fatal;
  } eis_deck_reparse_params;

  typedef struct {
    int pass_number;
    int max_passes;
    int max_level;
    int allow_root_keys;
    int allow_empty_blocks;
    EIS_BITMASK* state;
    int initialise_all_blocks;
    int unknown_block_is_fatal;
    int unknown_key_is_fatal;
    int non_value_line_is_blank;
    int end_pass_on_error;
    int finalize_on_error;
    int bad_key_is_fatal;
  } eis_deck_parse_serialised_params;

  typedef struct {
    deck_filename_processor_fn filename_processor;
    deck_file_text_processor_fn file_text_processor;
  } eis_deck_get_serialised_params;

  typedef struct {
    EIS_BITMASK* state;
    int pass_number;
  } eis_deck_initialise_params;

  typedef struct {
    EIS_BITMASK* state;
    int pass_number;
    int last_pass;
  } eis_deck_finalise_params;

  typedef struct {
    int dummy;
  } eis_deck_get_error_count_params;

  typedef struct {
    int dummy;
  } eis_deck_get_error_report_params;

  typedef struct {
    int dummy;
  } eis_deck_flush_errors_params;

  typedef struct {
    int dummy;
  } eis_deck_get_parsed_block_name_params;

  typedef struct {
    int dummy;
  } eis_deck_get_parsed_block_parents_params;

  typedef struct {
    int include_keys;
    int max_depth;
    int max_key_depth;
  } eis_deck_get_block_structure_params;

  typedef struct {
    int dummy;
  } eis_deck_release_strings_params;

  /*Routines to set the parameter structures to their default values*/
  EXTERN void eis_deck_create_definition_params_init(
      eis_deck_create_definition_params*);
  EXTERN void eis_deck_definition_inc_ref_params_init(
      eis_deck_definition_inc_ref_params*);
  EXTERN void eis_deck_definition_dec_ref_params_init(
      eis_deck_definition_dec_ref_params*);
  EXTERN void eis_deck_get_root_block_params_init(
      eis_deck_get_root_block_params*);
  EXTERN void eis_deck_add_block_params_init(eis_deck_add_block_params*);
  EXTERN void eis_deck_add_key_params_init(eis_deck_add_key_params*);
  EXTERN void eis_deck_optimise_definition_params_init(
      eis_deck_optimise_definition_params*);
  EXTERN void eis_deck_visualise_definition_params_init(
      eis_deck_visualise_definition_params*);
  EXTERN void eis_deck_markdown_definition_params_init(
      eis_deck_markdown_definition_params*);
  EXTERN void eis_deck_get_block_name_params_init(
      eis_deck_get_block_name_params*);
  EXTERN void eis_deck_create_parser_params_init(
      eis_deck_create_parser_params*);
  EXTERN void eis_deck_parser_inc_ref_params_init(
      eis_deck_parser_inc_ref_params*);
  EXTERN void eis_deck_parser_dec_ref_params_init(
      eis_deck_parser_dec_ref_params*);
  EXTERN void eis_deck_parse_file_params_init(eis_deck_parse_file_params*);
  EXTERN void eis_deck_parse_string_params_init(
      eis_deck_parse_string_params*);
  EXTERN void eis_deck_reparse_params_init(eis_deck_reparse_params*);
  EXTERN void eis_deck_parse_serialised_params_init(
      eis_deck_parse_serialised_params*);
  EXTERN void eis_deck_get_serialised_params_init(
      eis_deck_get_serialised_params*);
  EXTERN void eis_deck_initialise_params_init(eis_deck_initialise_params*);
  EXTERN void eis_deck_finalise_params_init(eis_deck_finalise_params*);
  EXTERN void eis_deck_get_error_count_params_init(
      eis_deck_get_error_count_params*);
  EXTERN void eis_deck_get_error_report_params_init(
      eis_deck_get_error_report_params*);
  EXTERN void eis_deck_flush_errors_params_init(
      eis_deck_flush_errors_params*);
  EXTERN void eis_deck_get_parsed_block_name_params_init(
      eis_deck_get_parsed_block_name_params*);
  EXTERN void eis_deck_get_parsed_block_parents_params_init(
      eis_deck_get_parsed_block_parents_params*);
  EXTERN void eis_deck_get_block_structure_params_init(
      eis_deck_get_block_structure_params*);
  EXTERN void eis_deck_release_strings_params_init(
      eis_deck_release_strings_params*);

  /*Routines to create and use deck definitions*/
  EXTERN int eis_deck_create_definition(EIS_ERROR*,
      eis_deck_create_definition_params*);
  EXTERN void eis_deck_definition_inc_ref(int,
      eis_deck_definition_inc_ref_params*);
  EXTERN void eis_deck_definition_dec_ref(int,
      eis_deck_definition_dec_ref_params*);
  EXTERN int eis_deck_get_root_block(int, EIS_ERROR*,
      eis_deck_get_root_block_params*);
  EXTERN int eis_deck_add_block(int, int, char*, EIS_ERROR*,
      eis_deck_add_block_params*);
  EXTERN void eis_deck_add_key(int, int, char*, EIS_ERROR*,
      eis_deck_add_key_params*);
  EXTERN void eis_deck_optimise_definition(int, EIS_ERROR*,
      eis_deck_optimise_definition_params*);
  EXTERN int eis_deck_visualise_definition(int, int, char*,
      eis_deck_visualise_definition_params*);
  EXTERN int eis_deck_markdown_definition(int, int, char*,
      eis_deck_markdown_definition_params*);
  EXTERN int eis_deck_get_block_name(int, int, int, char*,
      eis_deck_get_block_name_params*);

  /*Routines to load and parse decks*/
  EXTERN int eis_deck_create_parser(EIS_ERROR*, eis_deck_create_parser_params*);
  EXTERN void eis_deck_parser_inc_ref(int, eis_deck_parser_inc_ref_params*);
  EXTERN void eis_deck_parser_dec_ref(int, eis_deck_parser_dec_ref_params*);
  EXTERN void eis_deck_parse_file(int, int, char*, EIS_ERROR*,
      eis_deck_parse_file_params*);
  EXTERN void eis_deck_parse_string(int, int, char*, EIS_ERROR*,
      eis_deck_parse_string_params*);
  EXTERN void eis_deck_reparse(int, int, EIS_ERROR*,
      eis_deck_reparse_params*);
  EXTERN void eis_deck_parse_serialised(int, int, char*, int, EIS_ERROR*,
      eis_deck_parse_serialised_params*);
  EXTERN int eis_deck_get_serialised(int, char*, int, char*, EIS_ERROR*,
      eis_deck_get_serialised_params*);
  EXTERN void eis_deck_initialise(int, int, EIS_ERROR*,
      eis_deck_initialise_params*);
  EXTERN void eis_deck_finalise(int, int, EIS_ERROR*,
      eis_deck_finalise_params*);
  EXTERN int eis_deck_get_error_count(int, eis_deck_get_error_count_params*);
  EXTERN int eis_deck_get_error_report(int, int, int, char*,
      eis_deck_get_error_report_params*);
  EXTERN void eis_deck_flush_errors(int, eis_deck_flush_errors_params*);
  EXTERN int eis_deck_get_parsed_block_name(int, int, int, char*,
      eis_deck_get_parsed_block_name_params*);
  EXTERN int eis_deck_get_parsed_block_parents(int, int, int, int*,
      eis_deck_get_parsed_block_parents_params*);
  EXTERN int eis_deck_get_block_structure(int, int, char*,
      eis_deck_get_block_structure_params*);
  EXTERN void eis_deck_release_strings(int, eis_deck_release_strings_params*);

#else
#error Unsupported EIS_C_INTEROP_VERSION
#endif
#endif
