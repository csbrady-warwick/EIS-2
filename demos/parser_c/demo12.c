// Copyright (c) 2019-2026, C. S. Brady and H. Ratcliffe
// SPDX-License-Identifier: BSD-3-Clause

#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
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

/*This struct holds my data that I will use in the getter function*/
typedef struct {EIS_NUM x;} data;

/*This is the getter function for x*/
EIS_NUM get_x(int nparams, EIS_NUM *params, void *host_params,
    EIS_STATUS *status_code, EIS_ERROR *errcode)
{
  data *mydata = (data*) host_params;
  if (mydata) return mydata->x;
  return 0.0;
}

int main(int argc, char** argv)
{
  int parser, fstack, dstack, ct, iter;
  double res, fval, dval;
  char txt[1000], str[1000];
  data mydata;
  EIS_ERROR errcode;
  EIS_BITMASK bm;
  const double tolerance = 1.0e-12;
  const int max_iter = 50;

  printf("This example shows the use of a derivative stack. It takes an ");
  printf("expression in the variable x, uses the parser to get a stack that ");
  printf("calculates the derivative of the expression with respect to x and ");
  printf("then uses both stacks to find a root of the expression using ");
  printf("Newton-Raphson iteration\n");

  parser = eis_create_parser(eis_physics_si, &errcode, NULL);
  if (!success(parser, errcode)) exit(1);

  /*Add the variable x and tell the parser that the derivative of x with
  respect to x is 1. The fourth argument of eis_set_symbol_derivative is the
  variable that the derivative is with respect to. NULL means any variable*/
  eis_add_variable(parser, "x", get_x, &errcode, NULL);
  eis_set_symbol_derivative(parser, "x", "1", "x", &errcode, NULL);
  if (!success(parser, errcode)) exit(1);

  printf("Please input a mathematical expression :");
  if (fgets(txt,1000,stdin) == NULL) exit(0);
  txt[strcspn(txt,"\n")] = 0;
  bm = 0;
  fstack = eis_create_stack(parser, txt, &bm, &errcode, NULL);
  if (!success(parser, errcode)) exit(1);
  eis_get_infix_stack(fstack, 1000, str, NULL);
  printf("Specified Function : %s\n", str);

  /*Get a stack that calculates the derivative of the function with respect to
  x. The stack that holds the derivative must already exist so create an empty
  stack first*/
  dstack = eis_create_stack(parser, NULL, &bm, &errcode, NULL);
  eis_get_deriv_stack(fstack, dstack, "x", &errcode, NULL);
  if (!success(parser, errcode)) exit(1);
  eis_get_infix_stack(dstack, 1000, str, NULL);
  printf("df/dx : %s\n", str);

  printf("Please input a starting guess for x :");
  if (scanf("%lf", &mydata.x) != 1) exit(0);

  for (iter = 1; iter <= max_iter; ++iter) {
    ct = eis_evaluate_stack(fstack, 1, &res, &mydata, &errcode, NULL);
    fval = res;
    ct = eis_evaluate_stack(dstack, 1, &res, &mydata, &errcode, NULL);
    dval = res;
    if (!success(parser, errcode)) exit(1);

    printf("Iteration %d x = %g f(x) = %g\n", iter, mydata.x, fval);
    if (fabs(fval) < tolerance) {
      printf("Root found at x = %g\n", mydata.x);
      break;
    }
    if (dval == 0.0) {
      printf("The derivative is zero at x = %g. Cannot continue\n", mydata.x);
      break;
    }
    mydata.x = mydata.x - fval / dval;
  }

  if (iter > max_iter) printf("No root found in %d iterations\n", max_iter);

  return 0;
}
