#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include "./parser/include/eis_parser.h"

bool success(int parser, EIS_ERROR errcode){
  char txt[10000];
  int actual_chars;
  if (errcode == eis_err_none) return true;
  for (int i=1; i<=eis_parser_get_error_count(parser); ++i){
    actual_chars = eis_parser_get_error_report(parser, i, 10000, txt);
    printf("%s\n", txt);
  }
  eis_parser_flush_errors(parser);
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

  parser = eis_create_parser(&errcode, 1, 0, 0, eis_physics_si, 1);
  /*Create the variable. Specify the parser to add the variable to
  the name of the variable, the getter function for the variable
  the capability bit values for the variable, whether the variable
  can be simplified or not (here it can't be because it should
  return the new value every time), whether to add the variable
  to all parsers or just the specified one and a pointer to
  the error code value*/
  eis_add_variable(parser, "myvar", myvarfunc, 0, 0, 0, &errcode); 
  if (!success(parser, errcode)) exit(1);
  stack = eis_create_stack(parser, NULL, &bm, &errcode);
  while(true){
    printf("Please input a mathematical expression :");
    fgets(txt,1000,stdin);
    txt[strcspn(txt,"\n")] = 0;
    bm = 0;
    eis_populate_stack(stack, txt, &bm, &errcode);
    if (success(parser, errcode)) {
      ct = eis_evaluate_stack(stack, 10, res, &errcode, NULL);
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
