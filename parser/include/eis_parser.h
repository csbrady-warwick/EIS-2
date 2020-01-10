#ifndef EIS_INTEROP_PARSER_H
#define EIS_INTEROP_PARSER_H
#include "eis.h"

  /*Definition of an EIS evaluator function*/
  typedef double (*parser_eval_fn)(int, EIS_NUM*, void*, EIS_STATUS*, EIS_ERROR*);

 /*Physics package constants*/
  const int eis_physics_none = 0;
  const int eis_physics_si = 1;
  const int eis_physics_cgs_gauss = 2;


  /*Definitions of the routines in the interoperable interface*/
  extern int eis_create_parser(EIS_ERROR*, char*, int, int, int, int);
  extern int eis_create_stack (int, char*, EIS_BITMASK *, EIS_ERROR *);
  extern int eis_evaluate_stack(int, int, EIS_NUM*, EIS_ERROR*, void*, int*);
  extern void eis_add_function(int, void*, parser_eval_fn, EIS_BITMASK, int, int,
      int, EIS_ERROR *);
  extern void eis_add_variable(int, void*, parser_eval_fn, EIS_BITMASK, int,
      int, EIS_ERROR *);
  extern void eis_add_constant(int, void*, EIS_NUM, EIS_BITMASK, int,
      int, EIS_ERROR *);
  extern int eis_get_error_count(int);
  extern int eis_get_error_report(int, int, int, char*);
  extern void eis_stack_inc_ref(int);
  extern void eis_stack_dec_ref(int);
  extern int eis_copy_stack(int);

#endif
