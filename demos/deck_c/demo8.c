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

void print_errors(int deck)
{
  int ierr;
  char txt[10000];

  for (ierr = 1; ierr <= eis_deck_get_error_count(deck, NULL); ++ierr) {
    eis_deck_get_error_report(deck, ierr, 10000, txt, NULL);
    printf("%s\n", txt);
  }
}

int main(int argc, char** argv)
{
  int definition, root, block, deck;
  EIS_ERROR errcode = eis_err_none;
  eis_deck_add_block_params block_params;
  eis_deck_add_key_params key_params;
  eis_deck_parse_file_params parse_params;

  printf("This demonstrates multiple passes through a deck. The block ");
  printf("\"block1\" is only parsed on the first pass through the deck. ");
  printf("\"block2\" is only parsed on the second pass through\n");

  definition = eis_deck_create_definition(&errcode, NULL);
  root = eis_deck_get_root_block(definition, &errcode, NULL);

  eis_deck_add_key_params_init(&key_params);
  key_params.key_value_fn = key_sub;

  /*A block is restricted to a particular pass through the deck using pass_eq
  in its parameters. pass_le and pass_ge restrict it to passes less than or
  equal to, or greater than or equal to, a given pass*/
  eis_deck_add_block_params_init(&block_params);
  block_params.init_block = init_block;
  block_params.start_block = start_block;
  block_params.end_block = end_block;
  block_params.final_block = final_block;
  block_params.pass_eq = 1;
  block = eis_deck_add_block(definition, root, "block1", &errcode,
      &block_params);
  eis_deck_add_key(definition, block, "key1", &errcode, &key_params);
  eis_deck_add_key(definition, block, "key2", &errcode, &key_params);

  block_params.pass_eq = 2;
  block = eis_deck_add_block(definition, root, "block2", &errcode,
      &block_params);
  eis_deck_add_key(definition, block, "new_key", &errcode, &key_params);

  deck = eis_deck_create_parser(&errcode, NULL);

  /*Each pass is a separate call to parse the deck. The pass number and the
  total number of passes are part of the parse parameters*/
  eis_deck_parse_file_params_init(&parse_params);
  parse_params.allow_empty_blocks = 1;
  parse_params.max_passes = 2;

  printf("*** First pass through deck ***\n");
  parse_params.pass_number = 1;
  eis_deck_parse_file(deck, definition, "demos/decks/demo8.deck", &errcode,
      &parse_params);
  if (errcode != eis_err_none) print_errors(deck);

  printf("*** Second pass through deck ***\n");
  parse_params.pass_number = 2;
  eis_deck_parse_file(deck, definition, "demos/decks/demo8.deck", &errcode,
      &parse_params);
  if (errcode != eis_err_none) print_errors(deck);

  return 0;
}
