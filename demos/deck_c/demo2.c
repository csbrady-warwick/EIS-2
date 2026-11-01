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

void key_sub(char* key_text, char* value_text, int* pass_number,
    int nparents, int* parents, int* parent_kind, EIS_STATUS* status,
    EIS_BITMASK* host_state, EIS_ERROR* errcode)
{
  int i;

  printf(" Found in block :");
  for (i = 0; i < nparents; ++i) printf(" %d", parents[i]);
  printf("\n Found in block of type :");
  for (i = 0; i < nparents; ++i) printf(" %d", parent_kind[i]);
  printf("\n Found known key : %s with value %s\n", key_text, value_text);
}

int main(int argc, char** argv)
{
  int definition, root, block, deck, ierr;
  char txt[10000];
  EIS_ERROR errcode = eis_err_none;
  eis_deck_add_block_params block_params;
  eis_deck_add_key_params key_params;
  eis_deck_parse_file_params parse_params;

  printf("This extends demo1 by adding keys to the blocks. \"block1\" now has ");
  printf("keys \"key1\" and \"key2\" and \"block2\" has the key \"new_key\". ");
  printf("When any or all of these keys are encountered they are printed. If ");
  printf("any other keys are encountered an error occurs\n");

  definition = eis_deck_create_definition(&errcode, NULL);
  root = eis_deck_get_root_block(definition, &errcode, NULL);

  eis_deck_add_block_params_init(&block_params);
  block_params.init_block = init_block;
  block_params.start_block = start_block;
  block_params.end_block = end_block;
  block_params.final_block = final_block;
  block = eis_deck_add_block(definition, root, "block1", &errcode,
      &block_params);

  /*Keys are added to the block that the integer handle returned by
  eis_deck_add_block refers to. The callback for a key is given in its
  parameters structure*/
  eis_deck_add_key_params_init(&key_params);
  key_params.key_value_fn = key_sub;
  eis_deck_add_key(definition, block, "key1", &errcode, &key_params);
  eis_deck_add_key(definition, block, "key2", &errcode, &key_params);

  block = eis_deck_add_block(definition, root, "block2", &errcode,
      &block_params);
  eis_deck_add_key(definition, block, "new_key", &errcode, &key_params);

  deck = eis_deck_create_parser(&errcode, NULL);
  eis_deck_parse_file_params_init(&parse_params);
  parse_params.allow_empty_blocks = 1;
  eis_deck_parse_file(deck, definition, "demos/decks/demo2.deck", &errcode, &parse_params);
  if (errcode != eis_err_none) {
    for (ierr = 1; ierr <= eis_deck_get_error_count(deck, NULL); ++ierr) {
      eis_deck_get_error_report(deck, ierr, 10000, txt, NULL);
      printf("%s\n", txt);
    }
  }

  return 0;
}
