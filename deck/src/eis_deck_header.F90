! Copyright (c) 2019-2026, C. S. Brady and H. Ratcliffe
! SPDX-License-Identifier: BSD-3-Clause

MODULE eis_deck_header

  USE eis_header
  USE eis_parser_header
  USE eis_parser_constants, ONLY: eis_stack, eis_functor, eis_fsn_auto, &
      eis_fsn_always, eis_fsn_never, eis_physics_none, eis_physics_si, &
      eis_physics_cgs_gauss
  USE eis_deck_definition_mod
  USE eis_deck_from_text_mod
  USE eis_deck_caller_mod

END MODULE eis_deck_header
