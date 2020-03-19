#ifndef EIS_INTEROP_H
#define EIS_INTEROP_H

#include <stdint.h>

#define EIS_ERROR int64_t
#define EIS_STATUS int64_t
#define EIS_BITMASK int64_t
#define EIS_NUM double

#ifdef __cplusplus
#define EXTERN extern "C"
#else
#define EXTERN extern
#endif

  /*Error constants*/
  const EIS_ERROR eis_err_none                 = 0x0LL;
  const EIS_ERROR eis_err_parser               = 0x1LL;
  const EIS_ERROR eis_err_simplifier           = 0x2LL;
  const EIS_ERROR eis_err_emplacer             = 0x4LL;
  const EIS_ERROR eis_err_evaluator            = 0x8LL;
  const EIS_ERROR eis_err_file                 = 0x10LL;
  const EIS_ERROR eis_err_deck_file            = 0x20LL;
  const EIS_ERROR eis_err_deck_definition      = 0x40LL;
  const EIS_ERROR eis_err_deck_parser          = 0x80LL;
  const EIS_ERROR eis_err_not_found            = 0x100LL;
  const EIS_ERROR eis_err_malformed            = 0x200LL;
  const EIS_ERROR eis_err_wrong_parameters     = 0x400LL;
  const EIS_ERROR eis_err_maths_domain         = 0x800LL;
  const EIS_ERROR eis_err_bad_value            = 0x1000LL;
  const EIS_ERROR eis_err_has_deferred         = 0x2000LL;
  const EIS_ERROR eis_err_has_emplaced         = 0x4000LL;
  const EIS_ERROR eis_err_bracketed_constant   = 0x8000LL;
  const EIS_ERROR eis_err_extra_bracket        = 0x10000LL;
  const EIS_ERROR eis_err_bad_stack            = 0x20000LL;
  const EIS_ERROR eis_err_extra_results        = 0x400000LL;
  const EIS_ERROR eis_err_no_luns              = 0x800000LL;
  const EIS_ERROR eis_err_no_file              = 0x1000000LL;
  const EIS_ERROR eis_err_malformed_file       = 0x2000000LL;
  const EIS_ERROR eis_err_mismatched_begin_end = 0x4000000LL;
  const EIS_ERROR eis_err_dec_too_deep         = 0x8000000LL;
  const EIS_ERROR eis_err_deck_empty_block     = 0x10000000LL;
  const EIS_ERROR eis_err_root_keys            = 0x20000000LL;
  const EIS_ERROR eis_err_bad_deck_definition  = 0x40000000LL;
  const EIS_ERROR eis_err_unknown_block        = 0x80000000LL;
  const EIS_ERROR eis_err_unknown_key          = 0x100000000LL;
  const EIS_ERROR eis_err_bad_key              = 0x200000000LL;
  const EIS_ERROR eis_err_host                 = 0x400000000LL;
  const EIS_ERROR eis_err_text                 = 0x800000000LL;
  const EIS_ERROR eis_err_interop              = 0x1000000000LL;
  const EIS_ERROR eis_err_out_of_range         = 0x2000000000LL;
  const EIS_ERROR eis_err_stack_params         = 0x4000000000LL;
  const EIS_ERROR eis_err_invalid_minify       = 0x8000000000LL;
  const EIS_ERROR eis_err_bad_parser           = 0x1000000000LL;

#endif
