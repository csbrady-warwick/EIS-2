// Copyright (c) 2019-2026, C. S. Brady and H. Ratcliffe
// SPDX-License-Identifier: BSD-3-Clause

#include <stdio.h>
#include <string.h>
#include "eis_deck.h"

void key_str_sub(char* key_text, char* key_value, int* pass_number,
    int nparents, int* parents, int* parent_kind, EIS_STATUS* status,
    EIS_BITMASK* host_state, EIS_ERROR* errcode)
{
  char *lq, *uq;

  lq = strchr(key_value, '"');
  if (lq == NULL) {
    *status = eis_status_not_handled;
    return;
  }

  uq = strrchr(key_value, '"');
  if (uq - lq < 2) {
    *errcode = eis_err_bad_value;
    return;
  }

  printf("Found text key %s. Value is %.*s\n", key_text, (int)(uq - lq - 1),
      lq + 1);
}

void key_val_sub(char* key_text, int nvalues, EIS_NUM* values,
    int* pass_number, EIS_BITMASK cap_bits, int parser, int nparents,
    int* parents, int* parent_kind, EIS_STATUS* status,
    EIS_BITMASK* host_state, EIS_ERROR* errcode)
{
  int i;

  printf("Found numerical key %s. Values are", key_text);
  for (i = 0; i < nvalues; ++i) printf(" %g", values[i]);
  printf("\n");
}

int main(int argc, char** argv)
{
  int definition, root, block, deck, ierr;
  char txt[10000];
  EIS_ERROR errcode = eis_err_none;
  eis_deck_add_block_params block_params;
  eis_deck_add_key_params key_params;
  eis_deck_parse_file_params parse_params;

  printf("This example shows how to create subblocks. \"block2\" is made a ");
  printf("sub block of \"block1\". It behaves exactly the same as \"block2\" ");
  printf("in the previous examples but \"block2\" should now be defined ");
  printf("inside \"block1\". If you put \"block2\" outside \"block1\" it is ");
  printf("now an invalid block\n");

  definition = eis_deck_create_definition(&errcode, NULL);
  root = eis_deck_get_root_block(definition, &errcode, NULL);

  eis_deck_add_block_params_init(&block_params);
  block_params.any_key_value = key_str_sub;
  block_params.any_key_numeric_value = key_val_sub;
  block = eis_deck_add_block(definition, root, "block1", &errcode,
      &block_params);

  /*A sub block is added by using the handle of its parent block in place of
  the root block*/
  block = eis_deck_add_block(definition, block, "block2", &errcode, NULL);

  eis_deck_add_key_params_init(&key_params);
  key_params.key_value_fn = key_str_sub;
  key_params.key_numeric_value_fn = key_val_sub;
  eis_deck_add_key(definition, block, "new_key", &errcode, &key_params);

  deck = eis_deck_create_parser(&errcode, NULL);
  eis_deck_parse_file_params_init(&parse_params);
  parse_params.allow_empty_blocks = 1;
  eis_deck_parse_file(deck, definition, "demos/decks/demo6.deck", &errcode,
      &parse_params);
  if (errcode != eis_err_none) {
    for (ierr = 1; ierr <= eis_deck_get_error_count(deck, NULL); ++ierr) {
      eis_deck_get_error_report(deck, ierr, 10000, txt, NULL);
      printf("%s\n", txt);
    }
  }

  return 0;
}
