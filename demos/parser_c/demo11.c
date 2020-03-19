#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include "./parser/include/eis_parser.h"

int val = 0;

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

  parser = eis_create_parser(&errcode, 1, 0, 0, eis_physics_si, 1);
  /*Add a 32 bit integer pointer variable set cap bits to 1 so I can
  detect when it is used in an expression*/
  eis_add_pointer_variable(parser, "ptrvar", &val, 1, 0, 1, 1, 0, &errcode);
  if (!success(parser, errcode)) exit(1);
  stack = eis_create_stack(parser, NULL, &bm, &errcode);
  while(true){
    printf("Please input a mathematical expression :");
    fgets(txt,1000,stdin);
    txt[strcspn(txt,"\n")] = 0;
    bm = 0;
    eis_populate_stack(stack, txt, &bm, &errcode);
    if (bm == 1) val++;
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
