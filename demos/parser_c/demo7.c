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
  int parser, stack, ct, i ;
  double res[10];
  char txt[1000];
  EIS_ERROR errcode;
  EIS_BITMASK bm;

  printf("This example demonstrates using stack variables to store a value ");
  printf("that a user provides and making it available for later use. First ");
  printf("specify an expression that evaluates to a single value. That ");
  printf("expression is then stored under the name `stackvar` and can be ");
  printf("used in the second part of the code. NB stackvar is NOT just ");
  printf("storing the value of the expression and returning it but storing ");
  printf("the stack from your expression and putting it in place when you ");
  printf("use it by name. Functions and variables that have changing values ");
  printf("will change as expected\n\n");

  parser = eis_create_parser(eis_physics_si, &errcode, NULL);
  printf("Please input a mathematical expression for storage : ");
  fgets(txt,1000,stdin);
  txt[strcspn(txt,"\n")] = 0;

  stack = eis_create_stack(parser, txt, &bm, &errcode, NULL);
  /*Store the stack under the name `stackvar` so that later expressions can
  use it. The arguments are the parser, the name, the stack and the error
  code*/
  eis_add_stack_variable(parser, "stackvar", stack, &errcode, NULL);
  printf("Stack variable has been stored as `stackvar`\n\n");
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
