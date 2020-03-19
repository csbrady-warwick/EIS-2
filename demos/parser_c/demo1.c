#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include "./parser/include/eis_parser.h"

bool success(int parser, EIS_ERROR errcode){
  char txt[10000];
  int actual_chars;
  /*This function tests for error states and prints any errors
  that may have occured.It uses a fixed 10k character buffer but
  should test the returned number of characters from the
  eis_parser_get_error_report function and allocate a larger
  buffer if needed*/
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

  printf("This example uses the base parser to calculate maths expressions\n");
  /*Create a parser. Function returns an integer handle to the parser*/
  parser = eis_create_parser(&errcode, 1, 0, 0, eis_physics_none, 1);
  /*Create a stack object. The NULL for the second parameter means that an
  empty stack is created. Put a char* string in this location and the text
  will be parsed and the stack populated immediately. Here we want to 
  create the stack later so no need to specify an expression*/
  stack = eis_create_stack(parser, NULL, &bm, &errcode);
  while(true){
    printf("Please input a mathematical expression :");
    /*get the input string. Use fgets to capture white space 
    and then replace the captured newline character with a NULL*/
    fgets(txt,1000,stdin);
    txt[strcspn(txt,"\n")] = 0;
    bm = 0;
    /*Now populate rather than create the stack. eis_create_stack would
    work here but would keep creating new stacks*/
    eis_populate_stack(stack, txt, &bm, &errcode);
    /*If creating the stack succeeded then evaluate it to numbers*/
    if (success(parser, errcode)) {
      /*Evaluate the stack. Here I'm using an array of a fixed size
      of 10 items for the returned results. EIS won't return more items
      than you specify but the returned value will be the number of
      actual values obtained by parsing the stack. You should allocate
      a larger array and reevaluate the stack if ct is larger than your array*/
      ct = eis_evaluate_stack(stack, 10, res, &errcode, NULL);
      /*If evaluation succeeded then print the results*/
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
