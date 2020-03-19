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
  int parser, stack, ct, i;
  double res[10];
  char txt[1000];
  EIS_ERROR errcode;
  EIS_BITMASK bm;

  printf("This example creates a named constant called `myconst`. ");
  printf("Constants always have the same value but don't need a ");
  printf("getter function\n");

  parser = eis_create_parser(&errcode, 1, 0, 0, eis_physics_si, 1);
  /* Constants are defined by a name and a value. You specify the parser
  ID of the parser to add the constant to, the value of the constant, 
  the capability bits of the constant, whether or not the constant
  is simplifiable (at present there is no reason to make constants
  not simplifiable but the parameter is here for future use) whether to
  add the constant to just this parser or all parsers and a pointer to a
  variable to return an error code in */
  eis_add_constant(parser, "myconst", 1.2345, 0, 1, 0, &errcode);
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
