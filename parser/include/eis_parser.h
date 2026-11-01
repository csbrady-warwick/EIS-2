#ifndef EIS_INTEROP_PARSER_H
#define EIS_INTEROP_PARSER_H
#include "interop_version.inc"
#if (EIS_C_INTEROP_VERSION == 1)
#include "eis.h"

  /*Definition of an EIS evaluator function*/
  typedef EIS_NUM (*parser_eval_fn)(int, EIS_NUM*, void*, EIS_STATUS*, 
      EIS_ERROR*);
  typedef EIS_NUM (*parser_functor_fn)(int, EIS_NUM*, void*, void*, 
      EIS_STATUS*, EIS_ERROR*);
  typedef void (*parser_emplace_fn)(char*, int, EIS_NUM*, void*, int*, 
      EIS_STATUS*, EIS_ERROR*);
  typedef void (*parser_result_fn)(int*, EIS_NUM*, void*, EIS_STATUS*, 
      EIS_ERROR*);

 /*Physics package constants*/
  const int eis_physics_none = 0;
  const int eis_physics_si = 1;
  const int eis_physics_cgs_gauss = 2;


  /*Definitions of the routines in the interoperable interface*/
  EXTERN int eis_create_parser(EIS_ERROR*, int, int, int, int, int);
  EXTERN int eis_create_stack (int, char*, EIS_BITMASK *, EIS_ERROR *);
  EXTERN int eis_append_stack(int, char*, EIS_BITMASK *, EIS_ERROR *);
  EXTERN int eis_populate_stack(int, char*, EIS_BITMASK *, EIS_ERROR *);
  EXTERN int eis_empty_stack(int, char*, EIS_BITMASK *, EIS_ERROR *);
  EXTERN void eis_bind_result_to_stack(int, parser_result_fn,  EIS_ERROR*, 
      char*, int, int, EIS_BITMASK, int);
  EXTERN int eis_evaluate_stack(int, int, EIS_NUM*, EIS_ERROR*, void*);
  EXTERN void eis_add_function(int, char*, parser_eval_fn, EIS_BITMASK, int,
      int, int, EIS_ERROR *);
  EXTERN void eis_add_emplaced_function(int, char*, parser_emplace_fn, 
      EIS_ERROR*, int);
  EXTERN void eis_emplace_stack(int, EIS_ERROR*, int, void*);
  EXTERN void eis_add_functor(int, char*, parser_functor_fn, void*, 
      EIS_BITMASK, int, int, int, EIS_ERROR *);
  EXTERN void eis_add_variable(int, char*, parser_eval_fn, EIS_BITMASK, int,
      int, EIS_ERROR *);
  EXTERN void eis_add_stack_variable(int, char*, int, int, EIS_ERROR *);
  EXTERN void eis_add_stack_expression_variable(int, char*, char*, EIS_BITMASK, 
      int, int, EIS_ERROR *);
  EXTERN void eis_add_pointer_variable(int, char*, void*, int, int, EIS_BITMASK,
      int, int, EIS_ERROR *);
  EXTERN void eis_add_constant(int, char*, EIS_NUM, EIS_BITMASK, int,
      int, EIS_ERROR *);
  EXTERN void eis_add_deferred_constant(int, char*, EIS_NUM, EIS_BITMASK, int,
      int, EIS_ERROR *);
  EXTERN int eis_parser_get_error_count(int);
  EXTERN int eis_parser_get_error_report(int, int, int, char*);
  EXTERN int eis_parser_flush_errors(int);
  EXTERN void eis_stack_inc_ref(int);
  EXTERN void eis_stack_dec_ref(int);
  EXTERN void eis_parser_inc_ref(int);
  EXTERN void eis_parser_dec_ref(int);
  EXTERN int eis_copy_stack(int);

  EXTERN int eis_visualise_stack(int, int, char*, char*);
  EXTERN int eis_get_infix_stack(int, int, char*, char*);

#elif (EIS_C_INTEROP_VERSION == 2)
#include "eis.h"

  /*Definition of an EIS evaluator function*/
  typedef EIS_NUM (*parser_eval_fn)(int, EIS_NUM*, void*, EIS_STATUS*,
      EIS_ERROR*);
  typedef EIS_NUM (*parser_functor_fn)(int, EIS_NUM*, void*, void*,
      EIS_STATUS*, EIS_ERROR*);
  typedef void (*parser_emplace_fn)(char*, int, EIS_NUM*, void*, int*,
      EIS_STATUS*, EIS_ERROR*);
  typedef void (*parser_result_fn)(int*, EIS_NUM*, void*, EIS_STATUS*,
      EIS_ERROR*);

 /*Physics package constants*/
  const int eis_physics_none = 0;
  const int eis_physics_si = 1;
  const int eis_physics_cgs_gauss = 2;

  /*Value of expected_params for a function that accepts any number of
  parameters*/
#define EIS_VARIADIC -1

  /*Parameter structures for the routines in the interoperable interface.
  Each routine takes a pointer to its structure as the last argument. Passing
  NULL gives the default parameters. Use the matching "_init" routine to set a
  structure to the default parameters before changing individual members*/
  typedef struct {
    int should_simplify;
    int should_minify;
    int allow_text;
    int no_import;
    int maths_fail_quickly;
    int errhandler;
  } eis_create_parser_params;

  typedef struct {
    int simplify;
    int minify;
    int allow_text;
    char* filename;
    int line_number;
    int char_offset;
  } eis_create_stack_params;

  typedef struct {
    int simplify;
    int minify;
    int allow_text;
    char* filename;
    int line_number;
    int char_offset;
  } eis_append_stack_params;

  typedef struct {
    int simplify;
    int minify;
    int allow_text;
    char* filename;
    int line_number;
    int char_offset;
  } eis_populate_stack_params;

  typedef struct {
    int dummy;
  } eis_empty_stack_params;

  typedef struct {
    char* filename;
    int line_number;
    int char_offset;
    EIS_BITMASK cap_bits;
    int append;
  } eis_bind_result_to_stack_params;

  typedef struct {
    int dummy;
  } eis_evaluate_stack_params;

  typedef struct {
    EIS_BITMASK cap_bits;
    int can_simplify;
    int defer;
    int global;
    char* description;
    int hidden;
    int text_params;
    int is_idempotent;
    EIS_BITMASK text_param_mask;
  } eis_add_function_params;

  typedef struct {
    char* description;
    int hidden;
  } eis_add_emplaced_function_params;

  typedef struct {
    int stack_id_out;
  } eis_emplace_stack_params;

  typedef struct {
    EIS_BITMASK cap_bits;
    int can_simplify;
    int defer;
    int global;
    char* description;
    int hidden;
    int text_params;
    int is_idempotent;
    EIS_BITMASK text_param_mask;
  } eis_add_functor_params;

  typedef struct {
    EIS_BITMASK cap_bits;
    int can_simplify;
    int defer;
    int global;
    char* description;
    int hidden;
  } eis_add_variable_params;

  typedef struct {
    int defer;
    int global;
    char* description;
    int hidden;
  } eis_add_stack_variable_params;

  typedef struct {
    int global;
    char* description;
    int hidden;
  } eis_add_stack_expression_variable_params;

  typedef struct {
    EIS_BITMASK cap_bits;
    int can_simplify;
    int defer;
    int global;
    char* description;
    int hidden;
  } eis_add_pointer_variable_params;

  typedef struct {
    EIS_BITMASK cap_bits;
    int can_simplify;
    int defer;
    int global;
    char* description;
    int hidden;
  } eis_add_constant_params;

  typedef struct {
    EIS_BITMASK cap_bits;
    int can_simplify;
    int global;
    char* description;
    int hidden;
  } eis_add_deferred_constant_params;

  typedef struct {
    int dummy;
  } eis_parser_get_error_count_params;

  typedef struct {
    int dummy;
  } eis_parser_get_error_report_params;

  typedef struct {
    int dummy;
  } eis_parser_flush_errors_params;

  typedef struct {
    int dummy;
  } eis_stack_inc_ref_params;

  typedef struct {
    int dummy;
  } eis_parser_inc_ref_params;

  typedef struct {
    int dummy;
  } eis_stack_dec_ref_params;

  typedef struct {
    int dummy;
  } eis_parser_dec_ref_params;

  typedef struct {
    int dummy;
  } eis_copy_stack_params;

  typedef struct {
    char* formatstr;
  } eis_visualise_stack_params;

  typedef struct {
    char* formatstr;
  } eis_get_infix_stack_params;

  typedef struct {
    int dummy;
  } eis_get_deriv_stack_params;

  typedef struct {
    int global;
  } eis_set_symbol_derivative_params;

  typedef struct {
    int global;
  } eis_lock_symbol_derivative_params;

  /*Routines to set the parameter structures to their default values*/
  EXTERN void eis_create_parser_params_init(eis_create_parser_params*);
  EXTERN void eis_create_stack_params_init(eis_create_stack_params*);
  EXTERN void eis_append_stack_params_init(eis_append_stack_params*);
  EXTERN void eis_populate_stack_params_init(eis_populate_stack_params*);
  EXTERN void eis_empty_stack_params_init(eis_empty_stack_params*);
  EXTERN void eis_bind_result_to_stack_params_init(
      eis_bind_result_to_stack_params*);
  EXTERN void eis_evaluate_stack_params_init(eis_evaluate_stack_params*);
  EXTERN void eis_add_function_params_init(eis_add_function_params*);
  EXTERN void eis_add_emplaced_function_params_init(
      eis_add_emplaced_function_params*);
  EXTERN void eis_emplace_stack_params_init(eis_emplace_stack_params*);
  EXTERN void eis_add_functor_params_init(eis_add_functor_params*);
  EXTERN void eis_add_variable_params_init(eis_add_variable_params*);
  EXTERN void eis_add_stack_variable_params_init(
      eis_add_stack_variable_params*);
  EXTERN void eis_add_stack_expression_variable_params_init(
      eis_add_stack_expression_variable_params*);
  EXTERN void eis_add_pointer_variable_params_init(
      eis_add_pointer_variable_params*);
  EXTERN void eis_add_constant_params_init(eis_add_constant_params*);
  EXTERN void eis_add_deferred_constant_params_init(
      eis_add_deferred_constant_params*);
  EXTERN void eis_parser_get_error_count_params_init(
      eis_parser_get_error_count_params*);
  EXTERN void eis_parser_get_error_report_params_init(
      eis_parser_get_error_report_params*);
  EXTERN void eis_parser_flush_errors_params_init(
      eis_parser_flush_errors_params*);
  EXTERN void eis_stack_inc_ref_params_init(eis_stack_inc_ref_params*);
  EXTERN void eis_parser_inc_ref_params_init(eis_parser_inc_ref_params*);
  EXTERN void eis_stack_dec_ref_params_init(eis_stack_dec_ref_params*);
  EXTERN void eis_parser_dec_ref_params_init(eis_parser_dec_ref_params*);
  EXTERN void eis_copy_stack_params_init(eis_copy_stack_params*);
  EXTERN void eis_visualise_stack_params_init(eis_visualise_stack_params*);
  EXTERN void eis_get_infix_stack_params_init(eis_get_infix_stack_params*);
  EXTERN void eis_get_deriv_stack_params_init(eis_get_deriv_stack_params*);
  EXTERN void eis_set_symbol_derivative_params_init(
      eis_set_symbol_derivative_params*);
  EXTERN void eis_lock_symbol_derivative_params_init(
      eis_lock_symbol_derivative_params*);

  /*Definitions of the routines in the interoperable interface*/
  EXTERN int eis_create_parser(int, EIS_ERROR*, eis_create_parser_params*);
  EXTERN int eis_create_stack(int, char*, EIS_BITMASK*, EIS_ERROR*,
      eis_create_stack_params*);
  EXTERN void eis_append_stack(int, char*, EIS_BITMASK*, EIS_ERROR*,
      eis_append_stack_params*);
  EXTERN void eis_populate_stack(int, char*, EIS_BITMASK*, EIS_ERROR*,
      eis_populate_stack_params*);
  EXTERN void eis_empty_stack(int, EIS_ERROR*, eis_empty_stack_params*);
  EXTERN void eis_bind_result_to_stack(int, parser_result_fn, EIS_ERROR*,
      eis_bind_result_to_stack_params*);
  EXTERN int eis_evaluate_stack(int, int, EIS_NUM*, void*, EIS_ERROR*,
      eis_evaluate_stack_params*);
  EXTERN void eis_add_function(int, char*, parser_eval_fn, int, EIS_ERROR*,
      eis_add_function_params*);
  EXTERN void eis_add_emplaced_function(int, char*, parser_emplace_fn, int,
      EIS_ERROR*, eis_add_emplaced_function_params*);
  EXTERN void eis_emplace_stack(int, void*, EIS_ERROR*,
      eis_emplace_stack_params*);
  EXTERN void eis_add_functor(int, char*, parser_functor_fn, void*, int,
      EIS_ERROR*, eis_add_functor_params*);
  EXTERN void eis_add_variable(int, char*, parser_eval_fn, EIS_ERROR*,
      eis_add_variable_params*);
  EXTERN void eis_add_stack_variable(int, char*, int, EIS_ERROR*,
      eis_add_stack_variable_params*);
  EXTERN void eis_add_stack_expression_variable(int, char*, char*, EIS_ERROR*,
      eis_add_stack_expression_variable_params*);
  EXTERN void eis_add_pointer_variable(int, char*, void*, int, int,
      EIS_ERROR*, eis_add_pointer_variable_params*);
  EXTERN void eis_add_constant(int, char*, EIS_NUM, EIS_ERROR*,
      eis_add_constant_params*);
  EXTERN void eis_add_deferred_constant(int, char*, EIS_ERROR*,
      eis_add_deferred_constant_params*);
  EXTERN int eis_parser_get_error_count(int,
      eis_parser_get_error_count_params*);
  EXTERN int eis_parser_get_error_report(int, int, int, char*,
      eis_parser_get_error_report_params*);
  EXTERN void eis_parser_flush_errors(int, eis_parser_flush_errors_params*);
  EXTERN void eis_stack_inc_ref(int, eis_stack_inc_ref_params*);
  EXTERN void eis_stack_dec_ref(int, eis_stack_dec_ref_params*);
  EXTERN void eis_parser_inc_ref(int, eis_parser_inc_ref_params*);
  EXTERN void eis_parser_dec_ref(int, eis_parser_dec_ref_params*);
  EXTERN int eis_copy_stack(int, eis_copy_stack_params*);

  EXTERN int eis_visualise_stack(int, int, char*, eis_visualise_stack_params*);
  EXTERN int eis_get_infix_stack(int, int, char*, eis_get_infix_stack_params*);
  EXTERN void eis_get_deriv_stack(int, int, char*, EIS_ERROR*,
      eis_get_deriv_stack_params*);
  EXTERN void eis_set_symbol_derivative(int, char*, char*, char*, EIS_ERROR*,
      eis_set_symbol_derivative_params*);
  EXTERN void eis_lock_symbol_derivative(int, char*, char*, EIS_ERROR*,
      eis_lock_symbol_derivative_params*);

#else
#error Unsupported EIS_C_INTEROP_VERSION
#endif
#endif
