#ifndef EIS_INTEROP_PARSER_H
#define EIS_INTEROP_PARSER_H
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

#endif
