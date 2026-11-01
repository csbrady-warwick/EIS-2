// Copyright (c) 2019-2026, C. S. Brady and H. Ratcliffe
// SPDX-License-Identifier: BSD-3-Clause

#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include "eis_parser.h"

int val = 0;

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

  printf("This shows an example of using pointer variables. Pointer\n");
  printf("variables are variables that work by holding a pointer to a \n");
  printf("variable in your code and having the *current* value of that \n");
  printf("variable be returned when a maths expression involving the pointer\n");
  printf("variable is encountered. In order to work correctly your pointer \n");
  printf("must have a lifespan as long as the parser object that makes use\n");
  printf("of it\n");

  parser = eis_create_parser(eis_physics_si, &errcode, NULL);
  /*Add a 32 bit integer pointer variable set cap bits to 1 so I can
  detect when it is used in an expression*/
  eis_add_pointer_variable_params ptr_params;
  eis_add_pointer_variable_params_init(&ptr_params);
  ptr_params.cap_bits = 1;
  ptr_params.can_simplify = 1;
  eis_add_pointer_variable(parser, "ptrvar", &val, 1, 0, &errcode,
      &ptr_params);
  if (!success(parser, errcode)) exit(1);
  stack = eis_create_stack(parser, NULL, &bm, &errcode, NULL);
  while(true){
    printf("Please input a mathematical expression :");
    if (fgets(txt,1000,stdin) == NULL) break;
    txt[strcspn(txt,"\n")] = 0;
    bm = 0;
    eis_populate_stack(stack, txt, &bm, &errcode, NULL);
    if (bm == 1) val++;
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
