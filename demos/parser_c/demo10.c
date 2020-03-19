#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "./parser/include/eis_parser.h"

#define PI 3.14159

/*This struct holds my data that I will use in the result function*/
typedef struct {EIS_NUM x; EIS_NUM y;} data;

void get_values(int *nparams, EIS_NUM *results, void* host_params,
    EIS_STATUS *status_code, EIS_ERROR *errcode){

  data * mydata = (data*) host_params;

  nparams[0] = 1;
  results[0] = sin(4.0 * PI * (mydata->x-0.5)) * cos(6.0 * PI 
      * (mydata->y-0.5));

}

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
  double res;
  char txt[1000];
  data mydata;
  int ix, iy;
  EIS_ERROR errcode;
  EIS_BITMASK bm;
  FILE *f;

  parser = eis_create_parser(&errcode, 1, 0, 0, eis_physics_si, 1);
  if (!success(parser, errcode)) exit(1);

  stack = eis_create_stack(parser, txt, &bm, &errcode);
  eis_bind_result_to_stack(stack, get_values, &errcode, NULL, 0, 0, 0, 0);

  printf("This example makes use of a result function to show how to have\n");
  printf("EIS return a stack with a value that is returned by a host code\n");
  printf("provided function rather than a user provided stack. As with \n");
  printf("previous examples it writes a function to \"fort.10\" but the\n");
  printf("result returned from the \"get_values\" function rather than a \n");
  printf("text mode stack. Note that no text stack needs to be created, \n");
  printf("the result function can be bound directly to an unused stack.\n");

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
      ct = eis_evaluate_stack(stack, 1, &res, &errcode, &mydata);
      fprintf(f,"%g ", res);
    }
    fprintf(f,"\n");
  }
  fclose(f);
/* These are not necessary but this is how you release stacks and parsers
when you are done with them.*/
  eis_stack_dec_ref(stack);
  eis_parser_dec_ref(parser);
}
