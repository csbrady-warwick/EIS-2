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
/*This struct holds my data that I will use in the getter functions*/
typedef struct {EIS_NUM x; EIS_NUM y;} data;

/*This is the getter function for x position*/
EIS_NUM get_x(int nparams, EIS_NUM *params, void *host_params,
    EIS_STATUS *status_code, EIS_ERROR *errcode)
{
  data *mydata = (data*) host_params;
  if (mydata) return mydata->x;
  return 0.0;
}

/*This is the getter function for y position*/
EIS_NUM get_y(int nparams, EIS_NUM *params, void *host_params,
    EIS_STATUS *status_code, EIS_ERROR *errcode)
{
  data *mydata = (data*) host_params;
  if(mydata) return mydata->y;
  return 0.0;
}


int main(int argc, char** argv)
{
  int parser, stack, ct, i;
  double res;
  char txt[1000];
  data mydata;
  int ix, iy;
  EIS_ERROR errcode;
  EIS_BITMASK bm;
  FILE *f;

  printf("This example uses host parameters to evaluate an expression. ");
  printf("Specify an expression involving `x` and/or `y` and it will");
  printf("be evaluated on  the domain [[0,1],[0,1]]. The result will be");
  printf("to a formatter file called `fort.10`\n");

  parser = eis_create_parser(eis_physics_si, &errcode, NULL);

  if (!success(parser, errcode)) exit(1);
  eis_add_variable(parser, "x", get_x, &errcode, NULL);
  if (!success(parser, errcode)) exit(1);
  eis_add_variable(parser, "y", get_y, &errcode, NULL);
  if (!success(parser, errcode)) exit(1);
  mydata.x = 0.0;
  mydata.y = 0.0;
  while (true) {
    printf("Please input a mathematical expression :");
    if (fgets(txt,1000,stdin) == NULL) exit(0);
    txt[strcspn(txt,"\n")] = 0;
    if (strlen(txt) == 0) exit(0);
    bm = 0;
    stack = eis_create_stack(parser, txt, &bm, &errcode, NULL);
    if (success(parser, errcode)) {
      ct = eis_evaluate_stack(stack, 1, &res, &mydata, &errcode, NULL);
      if (success(parser, errcode)) break;
    }
    eis_stack_dec_ref(stack, NULL);
  }
  f = fopen("fort.10","w");
/* This iterates around on a 100x100 grid over a range [0,1] and evalautes your
function setting x and y as it goes. It writes the results straight out to disk
as a Fortran ordered array in a file called fort.10. Neither the Fortran
nor the filename are necessary or meaningful for a C code but they match the
Fortran example. If you reverse the loops then the array will be written C
ordered and you can obviously select any filename that you want in the 
fopen command */
  for (iy = 0; iy < 100 ; ++iy){
    mydata.y = (float)iy/99.0;
    for (ix = 0; ix < 100; ++ix) {
      mydata.x = (float)ix/99.0;
      /*The host parameters that are passed to the getter functions are the
      fourth argument to eis_evaluate_stack*/
      ct = eis_evaluate_stack(stack, 1, &res, &mydata, &errcode, NULL);
      if (!success(parser, errcode)) exit(1);
      fprintf(f,"%g ", res);
    }
    fprintf(f,"\n");
  }
  fclose(f);
/* These are not necessary but this is how you release stacks and parsers
when you are done with them.*/
  eis_stack_dec_ref(stack, NULL);
  eis_parser_dec_ref(parser, NULL);
}
