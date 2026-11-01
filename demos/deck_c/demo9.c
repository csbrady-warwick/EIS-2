// Copyright (c) 2019-2026, C. S. Brady and H. Ratcliffe
// SPDX-License-Identifier: BSD-3-Clause

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "eis_deck.h"

typedef struct block1 {
  char *key1, *key2;
  int block_uid;
  struct block1* next;
} block1;

typedef struct block2 {
  char* new_key;
  bool is_child;
  int block_uid;
  struct block2* next;
} block2;

block1 *b1head = NULL, *b1tail = NULL;
block2 *b2head = NULL, *b2tail = NULL;

void start_block1(char* block_text, int* pass_number, int nparents,
    int* parents, int* parent_kind, EIS_STATUS* status,
    EIS_BITMASK* host_state, EIS_ERROR* errcode)
{
  block1* new = calloc(1, sizeof(block1));

  if (b1tail) b1tail->next = new; else b1head = new;
  b1tail = new;
  b1tail->block_uid = parents[nparents-1];
}

void start_block2(char* block_text, int* pass_number, int nparents,
    int* parents, int* parent_kind, EIS_STATUS* status,
    EIS_BITMASK* host_state, EIS_ERROR* errcode)
{
  block2* new = calloc(1, sizeof(block2));

  if (b2tail) b2tail->next = new; else b2head = new;
  b2tail = new;
  b2tail->block_uid = parents[nparents-1];
  if (nparents > 2) b2tail->is_child = true;
}

void key1_sub(char* key_text, char* value_text, int* pass_number,
    int nparents, int* parents, int* parent_kind, EIS_STATUS* status,
    EIS_BITMASK* host_state, EIS_ERROR* errcode)
{
  b1tail->key1 = strdup(value_text);
}

void key2_sub(char* key_text, char* value_text, int* pass_number,
    int nparents, int* parents, int* parent_kind, EIS_STATUS* status,
    EIS_BITMASK* host_state, EIS_ERROR* errcode)
{
  b1tail->key2 = strdup(value_text);
}

void new_key_sub(char* key_text, char* value_text, int* pass_number,
    int nparents, int* parents, int* parent_kind, EIS_STATUS* status,
    EIS_BITMASK* host_state, EIS_ERROR* errcode)
{
  b2tail->new_key = strdup(value_text);
}

void print_dashes(int count)
{
  int i;

  printf("|");
  for (i = 0; i < count; ++i) printf("-");
}

/*Event callbacks are given to the definition and are called for every key or
block rather than for a specific one*/
void on_key_ok(char* key_text, int* pass_number, int nparents, int* parents,
    int* parent_kind, EIS_STATUS* status, EIS_BITMASK* host_state,
    EIS_ERROR* errcode)
{
  print_dashes(nparents + 1);
  printf("Key \"%s\" processed OK\n", key_text);
}

void on_key_fail(char* key_text, int* pass_number, int nparents, int* parents,
    int* parent_kind, EIS_STATUS* status, EIS_BITMASK* host_state,
    EIS_ERROR* errcode)
{
  print_dashes(nparents + 1);
  printf("Key \"%s\" failed processing\n", key_text);
  *errcode = eis_err_none;
}

void on_block_start(char* block_text, int* pass_number, int nparents,
    int* parents, int* parent_kind, EIS_STATUS* status,
    EIS_BITMASK* host_state, EIS_ERROR* errcode)
{
  if (strcmp(block_text, "{ROOT}") == 0) return;

  if (parents[nparents-1] != 1) printf("|\n");
  print_dashes(nparents);
  printf("Started block : %s\n", block_text);
}

void on_block_end(char* block_text, int* pass_number, int nparents,
    int* parents, int* parent_kind, EIS_STATUS* status,
    EIS_BITMASK* host_state, EIS_ERROR* errcode)
{
  if (strcmp(block_text, "{ROOT}") == 0) return;

  print_dashes(nparents);
  printf("Ended block : %s\n", block_text);
}

int main(int argc, char** argv)
{
  int definition, deck, root, block, ierr, len, i;
  char txt[10000];
  char* dot;
  FILE* fp;
  block1* p1;
  block2* p2;
  EIS_ERROR errcode = eis_err_none;
  eis_deck_create_definition_params definition_params;
  eis_deck_add_block_params block_params;
  eis_deck_add_key_params key_params;
  eis_deck_parse_file_params parse_params;
  eis_deck_get_block_structure_params structure_params;

  printf("This example shows how to use the visualisation routines in EIS. ");
  printf("It outputs two files \"fort.10\" contains a dot file showing the ");
  printf("structure of the actual deck that was being parsed. Blocks that ");
  printf("appear multiple times are shown multiple times. \"fort.20\" ");
  printf("contains a dot file describing the deck definition. It is not ");
  printf("related to the deck file that was actually read and only shows ");
  printf("the blocks and keys that could be in the file. You will need ");
  printf("tools such as Graphviz to convert dot files to viewable picture ");
  printf("files\n");

  eis_deck_create_definition_params_init(&definition_params);
  definition_params.on_key_success = on_key_ok;
  definition_params.on_key_failure = on_key_fail;
  definition_params.on_block_start = on_block_start;
  definition_params.on_block_end = on_block_end;
  definition = eis_deck_create_definition(&errcode, &definition_params);
  root = eis_deck_get_root_block(definition, &errcode, NULL);

  eis_deck_add_block_params_init(&block_params);
  eis_deck_add_key_params_init(&key_params);

  block_params.start_block = start_block1;
  block = eis_deck_add_block(definition, root, "block1", &errcode,
      &block_params);
  key_params.key_value_fn = key1_sub;
  eis_deck_add_key(definition, block, "key1", &errcode, &key_params);
  key_params.key_value_fn = key2_sub;
  eis_deck_add_key(definition, block, "key2", &errcode, &key_params);

  block_params.start_block = start_block2;
  key_params.key_value_fn = new_key_sub;
  block = eis_deck_add_block(definition, block, "block2", &errcode,
      &block_params);
  eis_deck_add_key(definition, block, "new_key", &errcode, &key_params);

  block = eis_deck_add_block(definition, root, "block2", &errcode,
      &block_params);
  eis_deck_add_key(definition, block, "new_key", &errcode, &key_params);

  deck = eis_deck_create_parser(&errcode, NULL);
  eis_deck_parse_file_params_init(&parse_params);
  parse_params.allow_empty_blocks = 1;
  eis_deck_parse_file(deck, definition, "demos/decks/demo9.deck", &errcode,
      &parse_params);
  if (errcode != eis_err_none) {
    for (ierr = 1; ierr <= eis_deck_get_error_count(deck, NULL); ++ierr) {
      eis_deck_get_error_report(deck, ierr, 10000, txt, NULL);
      printf("%s\n", txt);
    }
    return 1;
  }

  /*The routines that return text are called with a NULL buffer to find out
  how long the text is and then again with a buffer big enough to hold it*/
  eis_deck_get_block_structure_params_init(&structure_params);
  structure_params.include_keys = 1;
  len = eis_deck_get_block_structure(deck, 0, NULL, &structure_params);
  dot = malloc(len + 1);
  eis_deck_get_block_structure(deck, len + 1, dot, &structure_params);
  fp = fopen("fort.10", "w");
  fprintf(fp, "%s\n", dot);
  fclose(fp);
  free(dot);

  len = eis_deck_visualise_definition(definition, 0, NULL, NULL);
  dot = malloc(len + 1);
  eis_deck_visualise_definition(definition, len + 1, dot, NULL);
  fp = fopen("fort.20", "w");
  fprintf(fp, "%s\n", dot);
  fclose(fp);
  free(dot);

  for (i = 0; i < 80; ++i) printf("*");
  printf("\n");

  for (p1 = b1head; p1; p1 = p1->next) {
    printf("Unique block ID is %d\n", p1->block_uid);
    if (p1->key1) printf("Key1 is %s\n", p1->key1);
    if (p1->key2) printf("Key2 is %s\n", p1->key2);
    printf("\n");
  }

  for (p2 = b2head; p2; p2 = p2->next) {
    printf("Unique block ID is %d\n", p2->block_uid);
    if (p2->is_child) printf("Block is a child block\n");
    if (p2->new_key) printf("New key is %s\n", p2->new_key);
    printf("\n");
  }

  return 0;
}
