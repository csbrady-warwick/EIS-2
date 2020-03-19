#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "./parser/include/eis_parser.h"

int stacks[2];

bool success(int parser, EIS_ERROR errcode){
  char txt[10000];
  int actual_chars;
  if (errcode == eis_err_none) return true;
  if (errcode != eis_err_bad_stack && errcode != eis_err_bad_parser) {
    for (int i=1; i<=eis_parser_get_error_count(parser); ++i){
      actual_chars = eis_parser_get_error_report(parser, i, 10000, txt);
      printf("%s\n", txt);
    }
    eis_parser_flush_errors(parser);
  }
  if (errcode & eis_err_bad_stack) {
    printf("Developer has requested a bad stack\n");
  }
  if (errcode & eis_err_bad_parser) {
    printf("Developer has requested a bad parser\n");
  }
  return false;
}

void emplace_func(char* string, int nparams, EIS_NUM *params, 
    void *host_params, int *stack_out, EIS_STATUS *status, EIS_ERROR *errcode)
{
  int nint = round(params[0]);
  if (nint < 1 || nint > 2) {
    errcode[0] = eis_err_bad_value;
    return;
  }
  stack_out[0] = stacks[nint-1];
}

int main(int argc, char** argv)
{
  int parser, stack, ct, i ;
  double res[10];
  char txt[1000], txt2[1000];
  EIS_ERROR errcode;
  EIS_BITMASK bm;

  /*Here we have turned off simplification in the parser so that you
  can see that the stack is pasted into place*/
  parser = eis_create_parser(&errcode, 0, 0, 0, eis_physics_si, 1);
  if (!success(parser, errcode)) exit(-1);
  printf("Input expression for stack 1 : ");
  fgets(txt,1000,stdin);
  txt[strcspn(txt,"\n")] = 0;
  stacks[0] = eis_create_stack(parser, txt, &bm, &errcode);
  if (!success(parser, errcode)) exit(-1);

  printf("Input expression for stack 2 : ");
  fgets(txt,1000,stdin);
  txt[strcspn(txt,"\n")] = 0;
  stacks[1] = eis_create_stack(parser, txt, &bm, &errcode);
  if (!success(parser, errcode)) exit(-1);

  eis_add_emplaced_function(parser, "emfunc", emplace_func, &errcode, 1);
  if (!success(parser, errcode)) exit(-1);

  stack = eis_create_stack(parser, NULL, &bm, &errcode);
  if (!success(parser, errcode)) exit(-1);
  while(true){
    printf("Please input a mathematical expression :");
    fgets(txt,1000,stdin);
    txt[strcspn(txt,"\n")] = 0;
    bm = 0;
    /*The stack was copied when you called eis_add_stack_variable so you can
    reuse the same stack here */
    eis_populate_stack(stack, txt, &bm, &errcode);
    if (success(parser, errcode)) {
      eis_emplace_stack(stack, &errcode, -1, NULL);
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
}
