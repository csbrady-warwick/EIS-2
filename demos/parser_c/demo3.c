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

EIS_NUM myvarfunc(int nparams, EIS_NUM *params, void *host_params, 
    EIS_STATUS *status_code, EIS_ERROR *errcode)
{
  static int val = 0;
  val = val + 1;
  return val;
}

int main(int argc, char** argv)
{
  int parser, stack, ct, i;
  double res[10];
  char txt[1000];
  EIS_ERROR errcode;
  EIS_BITMASK bm;

  printf("This example creates a named variable called `myvar` that ");
  printf("increases by one every time you call it. Use it in expressions");
  printf("exactly like any other named constant\n");

  parser = eis_create_parser(eis_physics_si, &errcode, NULL);
  /*Create the variable. Specify the parser to add the variable to,
  the name of the variable, the getter function for the variable and a
  pointer to the error code value. The final NULL means use the default
  parameters. By default the variable has no capability bits, it can't be
  simplified (which is what we want here because it should return the new
  value every time) and it is added to just the specified parser rather than
  to all parsers*/
  eis_add_variable(parser, "myvar", myvarfunc, &errcode, NULL);
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
