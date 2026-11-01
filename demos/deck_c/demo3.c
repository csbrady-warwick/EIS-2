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

/*A numeric key callback is passed the results of evaluating the value of the
key with the EIS parser, along with the capability bits of the evaluated stack
and the handle of the parser that evaluated it*/
void key_sub(char* key_text, int nvalues, EIS_NUM* values, int* pass_number,
    EIS_BITMASK cap_bits, int parser, int nparents, int* parents,
    int* parent_kind, EIS_STATUS* status, EIS_BITMASK* host_state,
    EIS_ERROR* errcode)
{
  printf("Found key %s. It has %d elements\n", key_text, nvalues);
  printf("Value of first result is %g\n", values[0]);
  printf("Value of first result -1 is %g\n", values[0] - 1.0);
}

int main(int argc, char** argv)
{
  int definition, root, block, deck, ierr;
  char txt[10000];
  EIS_ERROR errcode = eis_err_none;
  eis_deck_add_block_params block_params;
  eis_deck_add_key_params key_params;
  eis_deck_parse_file_params parse_params;

  printf("This extends demo2 with keys that have numerical values. So long ");
  printf("as the keys have values that are evaluable by the EIS parser they ");
  printf("will be evaluated and the result printed. Unknown keys or keys ");
  printf("that cannot be evaluated by the EIS parser will cause an error\n");

  definition = eis_deck_create_definition(&errcode, NULL);
  root = eis_deck_get_root_block(definition, &errcode, NULL);

  eis_deck_add_block_params_init(&block_params);
  block_params.init_block = init_block;
  block_params.start_block = start_block;
  block_params.end_block = end_block;
  block_params.final_block = final_block;
  block = eis_deck_add_block(definition, root, "block1", &errcode,
      &block_params);

  eis_deck_add_key_params_init(&key_params);
  key_params.key_numeric_value_fn = key_sub;
  eis_deck_add_key(definition, block, "key1", &errcode, &key_params);
  eis_deck_add_key(definition, block, "key2", &errcode, &key_params);

  block = eis_deck_add_block(definition, root, "block2", &errcode,
      &block_params);
  eis_deck_add_key(definition, block, "new_key", &errcode, &key_params);

  deck = eis_deck_create_parser(&errcode, NULL);
  eis_deck_parse_file_params_init(&parse_params);
  parse_params.allow_empty_blocks = 1;
  eis_deck_parse_file(deck, definition, "demos/decks/demo3.deck", &errcode,
      &parse_params);
  if (errcode != eis_err_none) {
    for (ierr = 1; ierr <= eis_deck_get_error_count(deck, NULL); ++ierr) {
      eis_deck_get_error_report(deck, ierr, 10000, txt, NULL);
      printf("%s\n", txt);
    }
  }

  return 0;
}
