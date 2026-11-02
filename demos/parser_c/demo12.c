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

/*These are the getter functions for x and y*/
EIS_NUM get_x(int nparams, EIS_NUM *params, void *host_params,
    EIS_STATUS *status_code, EIS_ERROR *errcode)
{
  data *mydata = (data*) host_params;
  if (mydata) return mydata->x;
  return 0.0;
}

EIS_NUM get_y(int nparams, EIS_NUM *params, void *host_params,
    EIS_STATUS *status_code, EIS_ERROR *errcode)
{
  data *mydata = (data*) host_params;
  if (mydata) return mydata->y;
  return 0.0;
}

/*Evaluate a stack on [0,1],[0,1] and write the results to a file*/
void write_grid(int parser, int stack, data *mydata, FILE *out)
{
  int ix, iy, ct;
  double res;
  EIS_ERROR errcode;

  for (iy = 0; iy < 100; ++iy) {
    mydata->y = (double)iy / 99.0;
    for (ix = 0; ix < 100; ++ix) {
      mydata->x = (double)ix / 99.0;
      ct = eis_evaluate_stack(stack, 1, &res, mydata, &errcode, NULL);
      if (!success(parser, errcode)) exit(1);
      fprintf(out, "%g\n", res);
    }
  }
}

int main(int argc, char** argv)
{
  int parser, fstack, dstackdx, dstackdy, dstackdx2, dstackdy2, dstackdxdy;
  char txt[1000], str[1000];
  data mydata;
  FILE *out;
  EIS_ERROR errcode;
  EIS_BITMASK bm;

  printf("This example takes a stack expression parameterised by two ");
  printf("variables, x and y. It then calculates all of the first and second ");
  printf("partial derivatives of the expression\n");

  parser = eis_create_parser(eis_physics_si, &errcode, NULL);
  if (!success(parser, errcode)) exit(1);

  /*Add the variables x and y and tell the parser that the derivative of each
  with respect to itself is 1. The fourth argument of
  eis_set_symbol_derivative is the variable that the derivative is with respect
  to*/
  eis_add_variable(parser, "x", get_x, &errcode, NULL);
  eis_set_symbol_derivative(parser, "x", "1", "x", &errcode, NULL);
  eis_add_variable(parser, "y", get_y, &errcode, NULL);
  eis_set_symbol_derivative(parser, "y", "1", "y", &errcode, NULL);
  if (!success(parser, errcode)) exit(1);

  printf("Please input a mathematical expression :");
  if (fgets(txt,1000,stdin) == NULL) exit(0);
  txt[strcspn(txt,"\n")] = 0;
  bm = 0;
  fstack = eis_create_stack(parser, txt, &bm, &errcode, NULL);
  if (!success(parser, errcode)) exit(1);
  eis_get_infix_stack(fstack, 1000, str, NULL);
  printf("Specified Function : %s\n", str);

  /*The stacks that hold the derivatives must already exist so create empty
  stacks first. Each is then filled by eis_get_deriv_stack*/
  dstackdx = eis_create_stack(parser, NULL, &bm, &errcode, NULL);
  dstackdy = eis_create_stack(parser, NULL, &bm, &errcode, NULL);
  dstackdx2 = eis_create_stack(parser, NULL, &bm, &errcode, NULL);
  dstackdy2 = eis_create_stack(parser, NULL, &bm, &errcode, NULL);
  dstackdxdy = eis_create_stack(parser, NULL, &bm, &errcode, NULL);

  eis_get_deriv_stack(fstack, dstackdx, "x", &errcode, NULL);
  if (!success(parser, errcode)) exit(1);
  eis_get_infix_stack(dstackdx, 1000, str, NULL);
  printf("df/dx : %s\n", str);

  eis_get_deriv_stack(fstack, dstackdy, "y", &errcode, NULL);
  if (!success(parser, errcode)) exit(1);
  eis_get_infix_stack(dstackdy, 1000, str, NULL);
  printf("df/dy : %s\n", str);

  eis_get_deriv_stack(dstackdx, dstackdx2, "x", &errcode, NULL);
  if (!success(parser, errcode)) exit(1);
  eis_get_infix_stack(dstackdx2, 1000, str, NULL);
  printf("d2f/dx2 : %s\n", str);

  eis_get_deriv_stack(dstackdy, dstackdy2, "y", &errcode, NULL);
  if (!success(parser, errcode)) exit(1);
  eis_get_infix_stack(dstackdy2, 1000, str, NULL);
  printf("df2/dy2 : %s\n", str);

  eis_get_deriv_stack(dstackdx, dstackdxdy, "y", &errcode, NULL);
  if (!success(parser, errcode)) exit(1);
  eis_get_infix_stack(dstackdxdy, 1000, str, NULL);
  printf("d2f/dxdy : %s\n", str);

  printf("Writing expressions evaluated on [0,1],[0,1] to \"fort.10\"\n");

  out = fopen("fort.10", "w");
  write_grid(parser, fstack, &mydata, out);
  write_grid(parser, dstackdx, &mydata, out);
  write_grid(parser, dstackdy, &mydata, out);
  write_grid(parser, dstackdx2, &mydata, out);
  write_grid(parser, dstackdy2, &mydata, out);
  write_grid(parser, dstackdxdy, &mydata, out);
  fclose(out);

  return 0;
}
