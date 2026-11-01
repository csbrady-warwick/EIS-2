// Copyright (c) 2019-2026, C. S. Brady and H. Ratcliffe
// SPDX-License-Identifier: BSD-3-Clause

#include <stdio.h>
#include "eis_deck.h"

void init_block(char* block_text, int* pass_number, int nparents,
    int* parent_kind, EIS_STATUS* status, EIS_BITMASK* host_state,
    EIS_ERROR* errcode)
{
  printf("Calling init for block : %s\n", block_text);
}

void final_block(char* block_text, int* pass_number, int nparents,
    int* parent_kind, EIS_STATUS* status, EIS_BITMASK* host_state,
    EIS_ERROR* errcode)
{
  printf("Calling final for block : %s\n", block_text);
}

void start_block(char* block_text, int* pass_number, int nparents,
    int* parents, int* parent_kind, EIS_STATUS* status,
    EIS_BITMASK* host_state, EIS_ERROR* errcode)
{
  printf("Calling start for block : %s\n", block_text);
}

void end_block(char* block_text, int* pass_number, int nparents,
    int* parents, int* parent_kind, EIS_STATUS* status,
    EIS_BITMASK* host_state, EIS_ERROR* errcode)
{
  printf("Calling end for block : %s\n", block_text);
}

int main(int argc, char** argv)
{
  int definition, root, deck, ierr;
  char txt[10000];
  EIS_ERROR errcode = eis_err_none;
  eis_deck_add_block_params block_params;
  eis_deck_parse_file_params parse_params;

  printf("This example works with the demo1.deck file to demonstrate parsing ");
  printf("a simple deck file. It defines two blocks \"block1\" and ");
  printf("\"block2\" that *may* be present in a deck. Neither has any keys ");
  printf("defined so specifying any keys or any other blocks will raise an ");
  printf("error\n");

  /*Create a deck definition and get the root block that all other blocks
  are added to. Both return integer handles*/
  definition = eis_deck_create_definition(&errcode, NULL);
  root = eis_deck_get_root_block(definition, &errcode, NULL);

  /*Add two blocks to the root block. The callbacks for a block are given in
  the parameters structure. Both blocks use the same callbacks so the same
  structure is used for both*/
  eis_deck_add_block_params_init(&block_params);
  block_params.init_block = init_block;
  block_params.start_block = start_block;
  block_params.end_block = end_block;
  block_params.final_block = final_block;
  eis_deck_add_block(definition, root, "block1", &errcode, &block_params);
  eis_deck_add_block(definition, root, "block2", &errcode, &block_params);

  /*Create a deck parser and use it to parse the file against the
  definition*/
  deck = eis_deck_create_parser(&errcode, NULL);
  eis_deck_parse_file_params_init(&parse_params);
  parse_params.allow_empty_blocks = 1;
  eis_deck_parse_file(deck, definition, "demos/decks/demo1.deck", &errcode, &parse_params);
  if (errcode != eis_err_none) {
    for (ierr = 1; ierr <= eis_deck_get_error_count(deck, NULL); ++ierr) {
      eis_deck_get_error_report(deck, ierr, 10000, txt, NULL);
      printf("%s\n", txt);
    }
  }

  return 0;
}
