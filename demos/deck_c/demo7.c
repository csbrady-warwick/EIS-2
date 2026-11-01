// Copyright (c) 2019-2026, C. S. Brady and H. Ratcliffe
// SPDX-License-Identifier: BSD-3-Clause

#include <stdio.h>
#include <string.h>
#include "eis_deck.h"

int definition, deck;

/*A block remapper is given a buffer to write the replacement block name into.
Setting the status to eis_status_not_handled leaves the block name unchanged*/
void remapper(char* block_text, int* pass_number, int len_remap_text,
    char* remap_text, EIS_STATUS* status, EIS_BITMASK* host_state,
    EIS_ERROR* errcode)
{
  if (strstr(block_text, "block") != NULL) {
    snprintf(remap_text, len_remap_text, "generic_block");
  } else {
    *status = eis_status_not_handled;
  }
}

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
  char name[1000];

  printf("Found key %s with value %s\n", key_text, value_text);

  /*The last entry of parent_kind is the block ID in the definition and the
  last entry of parents is the ID of the block in the parsed deck*/
  eis_deck_get_block_name(definition, parent_kind[nparents-1], 1000, name,
      NULL);
  printf("Block generic name for key is %s\n", name);
  eis_deck_get_parsed_block_name(deck, parents[nparents-1], 1000, name, NULL);
  printf("Block specific name for key is %s\n", name);
}

int main(int argc, char** argv)
{
  int root, block, ierr;
  char txt[10000];
  EIS_ERROR errcode = eis_err_none;
  eis_deck_create_definition_params definition_params;
  eis_deck_add_block_params block_params;
  eis_deck_parse_file_params parse_params;

  printf("This example demonstrates block remapping. Block remapping allows ");
  printf("an EIS host code to tell EIS to look up a block under a name other ");
  printf("than the name that actually appears in the deck. In this case it ");
  printf("involves remapping any block containing the word \"block\" to the ");
  printf("block \"generic_block\"\n");

  eis_deck_create_definition_params_init(&definition_params);
  definition_params.block_remapper = remapper;
  definition = eis_deck_create_definition(&errcode, &definition_params);
  root = eis_deck_get_root_block(definition, &errcode, NULL);

  eis_deck_add_block_params_init(&block_params);
  block_params.init_block = init_block;
  block_params.start_block = start_block;
  block_params.end_block = end_block;
  block_params.final_block = final_block;
  block_params.any_key_value = key_sub;
  block = eis_deck_add_block(definition, root, "generic_block", &errcode,
      &block_params);

  deck = eis_deck_create_parser(&errcode, NULL);
  eis_deck_parse_file_params_init(&parse_params);
  parse_params.allow_empty_blocks = 1;
  eis_deck_parse_file(deck, definition, "demos/decks/demo7.deck", &errcode,
      &parse_params);
  if (errcode != eis_err_none) {
    for (ierr = 1; ierr <= eis_deck_get_error_count(deck, NULL); ++ierr) {
      eis_deck_get_error_report(deck, ierr, 10000, txt, NULL);
      printf("%s\n", txt);
    }
  }

  return 0;
}
