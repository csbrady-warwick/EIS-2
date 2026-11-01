// Copyright (c) 2019-2026, C. S. Brady and H. Ratcliffe
// SPDX-License-Identifier: BSD-3-Clause

#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include "eis_parser.h"

bool success(int parser, EIS_ERROR errcode){
  char txt[10000];
  int actual_chars;
  if (errcode == eis_err_none) return true;
  for (int i=1; i<=eis_parser_get_error_count(parser, NULL); ++i){
    actual_chars = eis_parser_get_error_report(parser, i, 10000, txt, NULL);
    printf("%s\n", txt);
  }
  eis_parser_flush_errors(parser, NULL);
  return false;
}

int main(int argc, char** argv)
{
  int parser, stack, ct, i;
  double res[10];
  char txt[1000];
  EIS_ERROR errcode;
  EIS_BITMASK bm;

  printf("This example creates a named constant called `myconst`. ");
  printf("Constants always have the same value but don't need a ");
  printf("getter function\n");

  parser = eis_create_parser(eis_physics_si, &errcode, NULL);
  /* Constants are defined by a name and a value. You specify the parser
  ID of the parser to add the constant to, the name and value of the constant
  and a pointer to a variable to return an error code in. The final NULL means
  use the default parameters. By default the constant has no capability bits,
  it is simplifiable (at present there is no reason to make constants not
  simplifiable but the parameter is here for future use) and it is added to
  just this parser rather than to all parsers */
  eis_add_constant(parser, "myconst", 1.2345, &errcode, NULL);
  if (!success(parser, errcode)) exit(1);
  stack = eis_create_stack(parser, NULL, &bm, &errcode, NULL);
  while(true){
    printf("Please input a mathematical expression :");
    if (fgets(txt,1000,stdin) == NULL) break;
    txt[strcspn(txt,"\n")] = 0;
    bm = 0;
    eis_populate_stack(stack, txt, &bm, &errcode, NULL);
    if (success(parser, errcode)) {
      ct = eis_evaluate_stack(stack, 10, res, NULL, &errcode, NULL);
      if (success(parser, errcode)){
        printf("Result is ");
        for (i = 0;i<ct ;++i){
          printf("%g", res[i]);
          if (i < ct-1) printf(", ");
        }
        printf("\n");
      }
    }
  }
}
