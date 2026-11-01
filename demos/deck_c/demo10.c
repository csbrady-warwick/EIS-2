// Copyright (c) 2019-2026, C. S. Brady and H. Ratcliffe
// SPDX-License-Identifier: BSD-3-Clause

#include <stdio.h>
#include <stdint.h>
#include "eis_deck.h"

int main(int argc, char** argv)
{
  int definition, root, block, deck, ierr, i;
  char txt[10000];
  int32_t scalar = 0;
  float array[4] = {0, 0, 0, 0};
  EIS_ERROR errcode = eis_err_none;
  eis_deck_add_key_params key_params;
  eis_deck_parse_file_params parse_params;

  printf("This shows using assignment variables in the deck. You can point a ");
  printf("deck key to a specific variable and that variable will be ");
  printf("automatically filled when the key is encountered. Multiple keys ");
  printf("will overwrite previous values without warning unless other ");
  printf("features of EIS are used to prevent this. It is possible to store ");
  printf("float, double, int32_t and int64_t values like this. It is also ");
  printf("possible to store single values or arrays. Parser expressions for ");
  printf("storing to single values must return a single value. Expressions ");
  printf("for storing to arrays must return <= the size of the array.\n");

  definition = eis_deck_create_definition(&errcode, NULL);
  root = eis_deck_get_root_block(definition, &errcode, NULL);
  block = eis_deck_add_block(definition, root, "block1", &errcode, NULL);

  /*A key is pointed at a variable using the matching pointer in its
  parameters. The "len" member gives the number of elements that the variable
  can hold. It is 1 by default*/
  eis_deck_add_key_params_init(&key_params);
  key_params.i32value = &scalar;
  eis_deck_add_key(definition, block, "scalar_value", &errcode, &key_params);

  eis_deck_add_key_params_init(&key_params);
  key_params.r32value = array;
  key_params.r32len = 4;
  eis_deck_add_key(definition, block, "array_value", &errcode, &key_params);

  deck = eis_deck_create_parser(&errcode, NULL);
  eis_deck_parse_file_params_init(&parse_params);
  eis_deck_parse_file(deck, definition, "demos/decks/demo10.deck", &errcode,
      &parse_params);
  if (errcode != eis_err_none) {
    for (ierr = 1; ierr <= eis_deck_get_error_count(deck, NULL); ++ierr) {
      eis_deck_get_error_report(deck, ierr, 10000, txt, NULL);
      printf("%s\n", txt);
    }
    return 1;
  }

  printf("Scalar value is %d\n", scalar);
  printf("Array value is");
  for (i = 0; i < 4; ++i) printf(" %g", array[i]);
  printf("\n");

  return 0;
}
