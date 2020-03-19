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

int main(int argc, char** argv)
{
  int parser, stack, ct, i ;
  double res[10];
  char txt[1000], txt2[1000];
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

  /*Here we have turned off simplification in the parser so that you
  can see that the stack is pasted into place*/
  parser = eis_create_parser(&errcode, 0, 0, 0, eis_physics_si, 1);
  printf("Please input a mathematical expression for storage : ");
  fgets(txt,1000,stdin);
  txt[strcspn(txt,"\n")] = 0;

  stack = eis_create_stack(parser, txt, &bm, &errcode);
  eis_add_stack_variable(parser, "stackvar", stack, 0, &errcode);
  printf("Stack variable has been stored as `stackvar`\n\n");
  while(true){
    printf("Please input a mathematical expression :");
    fgets(txt,1000,stdin);
    txt[strcspn(txt,"\n")] = 0;
    bm = 0;
    /*The stack was copied when you called eis_add_stack_variable so you can
    reuse the same stack here */
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
/*We now get the infix maths version of the full stack and print it*/
        eis_get_infix_stack(stack, 1000, txt2, NULL);
        printf("Full expanded expression was %s\n\n", txt2);
      }
    }
  }
}
