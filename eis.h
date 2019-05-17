#ifndef EIS_INTEROP_H
#define EIS_INTEROP_H
#define EIS_ERROR long long

  /*Definition of an EIS evaluator function*/
  typedef double (*parser_eval_fn)(int, double*, void*, long long*, long long*);

  /*Language constants*/
  const int eis_lang_en = 1;
  const int eis_lang_ru = 2;

  const int eis_physics_none = 0;
  const int eis_physics_si = 1;
  const int eis_physics_cgs_gauss = 2;

  /*Error constants*/
  const EIS_ERROR eis_err_none = 0x0;
  const EIS_ERROR eis_err_parser = 0x1;
  const EIS_ERROR eis_err_simplifier = 0x2;
  const EIS_ERROR eis_err_emplacer = 0x4;
  const EIS_ERROR eis_err_evaluator = 0x8;
  const EIS_ERROR eis_err_not_found = 0x10;
  const EIS_ERROR eis_err_malformed = 0x20;
  const EIS_ERROR eis_err_wrong_parameters = 0x40;
  const EIS_ERROR eis_err_maths_domain = 0x80;
  const EIS_ERROR eis_err_bad_value = 0x100;
  const EIS_ERROR eis_err_has_deferred = 0x200;
  const EIS_ERROR eis_err_has_emplaced = 0x400;
  const EIS_ERROR eis_err_where = 0x800;
  const EIS_ERROR eis_err_bracketed_constant = 0x1000;
  const EIS_ERROR eis_err_extra_bracket = 0x2000;
  const EIS_ERROR eis_err_bad_stack = 0x4000;
  const EIS_ERROR eis_err_extra_results = 0x8000;

  /*Definitions of the routines in the interoperable interface*/
  extern int eis_create_parser(int, int, int, int, int);
  extern int eis_create_stack (int, void*, long long *, long long *);
  extern int eis_evaluate_stack(int, int, double*, long long *, void*, int*);
  extern void eis_add_function(int, void*, parser_eval_fn, long long, int, int,
      int, long long *);

#endif
