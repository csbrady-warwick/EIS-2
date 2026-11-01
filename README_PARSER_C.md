# Using the EIS parser from C

This document describes the C interface to the EIS parser. The interface has the same behaviour whichever implementation of EIS it is built on, and the examples in it are based on the programs in `demos/parser_c`. The Fortran interface is described in `README_PARSER.md`.

## How the C interface works

The C interface is a set of functions declared in `eis_parser.h`. A few conventions apply to all of them.

* **Handles.** Parsers and stacks live inside EIS and C code refers to them by an `int` handle. IDs start from 1 and a handle that is not valid is reported through the error code. Creating a parser with `eis_create_parser` gives you its handle and creating a stack with `eis_create_stack` gives you the handle of a stack.
* **Types.** `EIS_NUM` is the type of the numbers that EIS calculates with. `EIS_ERROR`, `EIS_STATUS` and `EIS_BITMASK` are 64 bit integers. Error codes and status codes are bitmasks, so test them with `&` rather than `==` unless you only want to know whether there was an error at all.
* **Error codes.** Most functions take a pointer to an `EIS_ERROR` as one of their arguments and set it. The value `eis_err_none` means that there was no error.
* **Optional parameters.** Most functions take a pointer to a structure of optional parameters as their last argument. See the next section.
* **Strings.** Strings are normal null terminated `char*` strings. Functions that return text fill a buffer that you supply. They take the length of the buffer and return the number of characters needed, so you can pass `NULL` as the buffer to find out the size.
* **Reference counts.** Parsers and stacks are released with `eis_parser_dec_ref` and `eis_stack_dec_ref`. A handle starts with a reference count of one. `eis_parser_inc_ref` and `eis_stack_inc_ref` add a reference if another part of your code wants to keep an object alive.

## Optional parameters

Many of the functions in the C interface have optional behaviour. In the Fortran interface this is done with optional arguments. In the C interface a function that has options takes a pointer to a structure that holds them as its last argument, and this document calls the members of these structures optional parameters. When a description in this document says that a function takes "the parameters" it means this structure.

### Finding the names

The structure for a function has the name of the function followed by `_params`, so the optional parameters of `eis_add_variable` are held in a structure of type `eis_add_variable_params`. The function that sets the defaults for a structure has the name of the structure followed by `_init`, so here it is `eis_add_variable_params_init`. The members of each structure, and what they do, are described in the API documentation for the structure and in `eis_parser.h`.

### Setting them

If you pass `NULL` as the pointer then every optional parameter has its default value. To change one or more of them, declare the structure, set it to the defaults with its `_init` function, change the members that you want and pass a pointer to it.

```c
eis_add_variable_params var_params;
eis_add_variable_params_init(&var_params);
var_params.can_simplify = 1;
eis_add_variable(parser, "x", get_x, &errcode, &var_params);
```

Always call the `_init` function before you change anything rather than filling in the structure yourself. Members that you do not set would otherwise hold arbitrary values, and the defaults of members that are added in the future are only set by `_init`. A few integer members, for example `simplify`, use -1 to mean that the value is not specified and the setting of the parser is used. The structure, and any strings that it points to, only need to exist for the duration of the call, so you can change a structure and use it again for another call. Functions that have no options at present still take the argument, so pass `NULL` for those.

### How this document shows them

Whenever the text or an example uses an optional parameter it names the parameter and the structure that holds it, and code examples that set optional parameters have a comment saying so. Arguments that are not optional parameters, such as the number of expected parameters to a function, are ordinary arguments of the function.

## Concepts

The core of the EIS parser is a parser. You create one with `eis_create_parser` and then hand it a mathematical expression as a string to turn it into a stack. A stack represents a parsed expression and can be evaluated multiple times without the overhead of parsing the string again. When you evaluate a stack you can optionally pass it a `void*` that points to information provided by the host code rather than read in from the user. None of the built in routines make use of this information because it is entirely specified by the host code. This might, for example, point to a structure that holds where in space a stack should be evaluated and you could add functions or variables that give a different result based on the values in it.

The C interface always evaluates through a stack. To evaluate a string once, create a stack from it and evaluate that.

## Terms

Parser - A parser object, referred to by an `int` handle

Expression - A string to be parsed by the parser

Stack - A parsed expression, referred to by an `int` handle

Tokenization - Converting an expression into a stack. This is what `eis_create_stack`, `eis_populate_stack` and `eis_append_stack` do

Evaluation - Converting a stack into an array of numerical results

Literal - A numerical value entered directly into the text to be parsed as a number (e.g. 1)

Constant - A value known by name to the parser to be always a fixed numerical value (e.g. kb for Boltzmann's constant)

Host Parameters - A `void*` given by the host code that is available to all of the routines that evaluate the parts of a stack

Variable - A value known by name to the parser but where the value can only be determined when the stack is created or evaluated. It cannot take any parameters from the user but will in general use the host parameters

Stack Variable - A preexisting stack that is known by name to the parser. When the name is encountered the stack is placed into the new expression at the location of the name. This means that a mathematical expression can be stored and then reused in other expressions

Function - A routine known by name to the parser that takes a number of parameters from the user before evaluating

Variadic Function - A function that takes as many parameters as it is given rather than expecting a fixed number. `EIS_VARIADIC` is the number of expected parameters for these

Functor - A function that also carries a `void*` of data of your own with it

Parser Item - A function, variable, stack variable or constant

Deferred - A function, variable, constant or stack variable that is given a name but not an evaluation function or value when it is first registered. A deferred item can be used to create stacks anyway. An evaluation function or value must be assigned before the stack can be evaluated

Emplaced - A function that returns a stack rather than a value. The stack is then put into place

Capability bits - Any variable, function or constant can have capability bits associated with it. These capability bits are combined when a stack is parsed and the capability bits of the stack are the bitwise OR of the capability bits of its contents

Description - A text description of a symbol given when it is added to the parser

## Simple EIS parser program

The simplest parser program is just a glorified calculator. You create a parser, create a stack and then populate it from the text that the user types in. `eis_create_parser` takes the physics package to use (`eis_physics_none`, `eis_physics_si` or `eis_physics_cgs_gauss`) and returns the handle of the new parser. `eis_create_stack` takes the handle of the parser, an expression to parse (`NULL` makes an empty stack), a pointer to an `EIS_BITMASK` that is set to the capability bits of the stack and an error code. `eis_populate_stack` replaces the contents of an existing stack with a new expression.

`eis_evaluate_stack` takes the stack, the number of results that your array can hold, the array, the host parameters (`NULL` if there are none) and an error code. It returns the number of results that the stack actually produced. If this is more than your array can hold then the error `eis_err_extra_results` is set and only the first results are copied, so allocate a larger array and evaluate again. If the evaluation failed to produce results then it returns -1.

If an error is reported then the parser holds a list of error reports. The error count and each report are read with `eis_parser_get_error_count` and `eis_parser_get_error_report`, where the reports are numbered from 1. The report function takes the size of your buffer and returns the length of the report plus one, so a buffer of that size holds the whole report. `eis_parser_flush_errors` empties the list. The following program is a calculator that keeps taking expressions until the input ends.

```c
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include "eis_parser.h"

/*Test for an error and print the error reports if there was one*/
bool success(int parser, EIS_ERROR errcode){
  char txt[10000];
  if (errcode == eis_err_none) return true;
  for (int i = 1; i <= eis_parser_get_error_count(parser, NULL); ++i){
    eis_parser_get_error_report(parser, i, 10000, txt, NULL);
    printf("%s\n", txt);
  }
  eis_parser_flush_errors(parser, NULL);
  return false;
}

int main(int argc, char** argv)
{
  int parser, stack, ct, i;
  double res[10];
  char txt[1000];
  EIS_ERROR errcode;
  EIS_BITMASK bm;

  parser = eis_create_parser(eis_physics_none, &errcode, NULL);
  stack = eis_create_stack(parser, NULL, &bm, &errcode, NULL);
  while (true) {
    printf("Please input a mathematical expression :");
    if (fgets(txt, 1000, stdin) == NULL) break;
    txt[strcspn(txt, "\n")] = 0;
    eis_populate_stack(stack, txt, &bm, &errcode, NULL);
    if (success(parser, errcode)) {
      ct = eis_evaluate_stack(stack, 10, res, NULL, &errcode, NULL);
      if (success(parser, errcode)) {
        for (i = 0; i < ct; ++i) printf("%g ", res[i]);
        printf("\n");
      }
    }
  }
  eis_stack_dec_ref(stack, NULL);
  eis_parser_dec_ref(parser, NULL);
}
```

## Using stacks to separate tokenization and evaluation

At the most basic level the EIS parser is a fairly simple combination of two things:

1. An implementation of Dijkstra's Shunting Yard Algorithm to convert the normal infix maths notation that the user enters into reverse Polish notation. At the same time it is converted from text to a stream of tokens that say what functions should be called to evaluate the expression.
2. An RPN calculator to evaluate the token stream into a set of numbers

EIS is more complicated than this in various ways but that is a good approximation to its core. A fairly large chunk of the time taken to evaluate EIS expressions is the time taken to convert the text expression into the token stream. In the C interface this is always a separate step because you create a stack from the text and then call `eis_evaluate_stack` on it. If you evaluate the same expression repeatedly then create the stack once and evaluate it many times.

`eis_create_stack` makes a new stack each time that you call it, so use `eis_populate_stack` to reuse a stack for a new expression. `eis_append_stack` adds a new expression to the end of an existing stack, which usually produces a stack with several results. `eis_empty_stack` empties a stack. `eis_copy_stack` makes a copy and returns the handle of the copy.

## Selecting a physics package

EIS includes several common physical constants. By default they are only available by typing their full name. To make them easily available, select a physics package when you create the parser by passing `eis_physics_si` or `eis_physics_cgs_gauss` as the first argument to `eis_create_parser`.

```c
parser = eis_create_parser(eis_physics_si, &errcode, NULL);
```

The first loads the physical constants in SI into the default known constants and the second CGS Gaussian units. Other physics models may be added later.

If you want to use a physical constant without setting up a global physics model, or want a constant from a different physics model than the one selected, you have to use the full name. The full names are the names of the constants prefixed with either

1) physics.si. (e.g. physics.si.kb is Boltzmann's constant)

2) physics.cgs.gauss. (e.g. physics.cgs.gauss.kb is Boltzmann's constant)

## Adding a constant

EIS comes with a decent list of physical and mathematical constants built in but one of the most common things to want to do is to add extra constants. Use `eis_add_constant`, which takes the parser, the name, the value, an error code and the parameters.

```c
eis_add_constant(parser, "myconst", 1.2345, &errcode, NULL);
```

You can then use `myconst` by name in any expression and the value 1.2345 is used.

## Adding variables

Constants are just names that map to fixed values. If you want to (potentially) return a different value each time that a name is used in an expression then you want a variable. Variables are defined by a getter function rather than a fixed value. A getter function looks like

```c
EIS_NUM getter(int nparams, EIS_NUM *params, void *host_params,
    EIS_STATUS *status_code, EIS_ERROR *errcode);
```

The getter functions are used in several places in EIS and almost all of the parameters are not needed for variables. The main thing for a variable is just to return its value. Add a variable with `eis_add_variable`, which takes the parser, the name, the getter, an error code and the parameters.

```c
EIS_NUM get_var(int nparams, EIS_NUM *params, void *host_params,
    EIS_STATUS *status_code, EIS_ERROR *errcode)
{
  static EIS_NUM store = 0.0;
  store = store + 1.0;
  return store;
}

eis_add_variable(parser, "myvar", get_var, &errcode, NULL);
```

Because the variable is not simplifiable by default the value is read each time that the stack is evaluated, so `myvar` increases by one each time. If you set the optional parameter `can_simplify` to 1 then the value can be calculated once at the point that the stack is simplified.

## Parser Simplifier

To improve performance the parser uses an abstract syntax tree based simplification system. This works by converting every operator, variable and function to a node on a tree, with each node having subnodes for the parameters where relevant. It then recursively goes through those nodes replacing them with a constant value if possible and then rebuilding the simplified tree into a new stack. To calculate the constant value for the simplifier the code evaluates the nodes with `NULL` as the host parameters. This replacement only happens if the node is flagged as being simplifiable, otherwise the simplifier simplifies as many nodes as possible and leaves the remaining nodes to be evaluated every time the stack is evaluated. All constants and functions are simplifiable by default. Variables are not simplifiable by default. The optional parameter `can_simplify` changes this when you add a variable or function.

Simplification can be turned off at several points

1. By setting the optional parameter `should_simplify` to 0 when you create the parser. No stacks are simplified by default
2. By setting the optional parameter `simplify` to 0 when you call `eis_create_stack`, `eis_populate_stack` or `eis_append_stack`. If the parser default is no simplification, setting `simplify` to 1 turns it on for this stack
3. A getter function can set `*status_code` to `eis_status_no_simplify` to prevent simplification of a tree node during the simplification stage.

## Using Host Parameters

Host parameters are specified at the point that you call `eis_evaluate_stack` and are passed to all of the getter functions that are called as the stack evaluates. They are a simple way of making information available in getter functions in a generic fashion. Typically you use them to specify a location in space, the positions of particles and so on. Host parameters are not the only way to pass data to getter functions because getter functions can also read global data in the normal way. They are mostly useful because they can be prepared for a specific call.

The classic example is to define a position in a spatial domain and then use the host code to iterate over that domain. A user can then define an expression in terms of spatial variables. This needs

1. A structure that holds the position
2. A getter function for each variable that reads from that structure

```c
typedef struct {EIS_NUM x; EIS_NUM y;} data;

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
```

Then register the two getters as variables and pass a pointer to the structure as the host parameters when you evaluate the stack.

```c
eis_add_variable(parser, "x", get_x, &errcode, NULL);
eis_add_variable(parser, "y", get_y, &errcode, NULL);
stack = eis_create_stack(parser, txt, &bm, &errcode, NULL);
for (iy = 0; iy < 100; ++iy) {
  mydata.y = (float)iy / 99.0;
  for (ix = 0; ix < 100; ++ix) {
    mydata.x = (float)ix / 99.0;
    ct = eis_evaluate_stack(stack, 1, &res, &mydata, &errcode, NULL);
  }
}
```

*Even if you always specify host parameters there may be occasions where EIS calls a getter function with the host parameters set to `NULL`, so getter functions that use the host parameters should always test for this and deal with it accordingly.*

## Adding functions

Functions take parameters that are specified by the user and act on them to return a value. They use the same getter signature as variables and you read the parameters from the `params` array. The parameters are in left to right order in elements 0 to `nparams - 1`. The array already contains the completely evaluated numbers and it is not possible to get the original text of the parameters.

Add a function with `eis_add_function`, which takes the parser, the name, the getter function, the number of expected parameters, an error code and the parameters. If the number of expected parameters is given then an error is reported automatically if the user provides a different number. Pass `EIS_VARIADIC` for a variadic function. A variadic function reports the number of parameters that it was called with in `nparams` and should set `*errcode` to `eis_err_wrong_parameters` if that is not acceptable. Do *NOT* infer the number of parameters from anything other than `nparams`. The number of expected parameters is an ordinary argument in C and is not one of the optional parameters.

```c
EIS_NUM cauchy_dist(int nparams, EIS_NUM *params, void *host_params,
    EIS_STATUS *status_code, EIS_ERROR *errcode)
{
  EIS_NUM pi = 3.14159;
  return 1.0/(pi * params[2]) * (pow(params[2],2)
      / pow(params[0] - params[1], 2) + pow(params[2],2));
}

eis_add_function(parser, "cauchy", cauchy_dist, 3, &errcode, NULL);
```

### Functors

A functor is a function that also has a `void*` of data of your own that is passed back to it every time. Add one with `eis_add_functor`, which takes the parser, the name, the function, the data pointer, the number of expected parameters, an error code and the parameters. The function has an extra `bound_data` argument.

```c
EIS_NUM functor_fn(int nparams, EIS_NUM *params, void *host_params,
    void *bound_data, EIS_STATUS *status_code, EIS_ERROR *errcode);
```

The derivative of a functor is locked when it is added. See the section on derivatives.

## Errors in getter functions

All getter functions can report errors that occur while they are evaluating variables or functions by setting `*errcode`. The current errors that should be reported by a getter function are

* `eis_err_wrong_parameters` Wrong number of parameters supplied to a function
* `eis_err_maths_domain` Parameter to this function is mathematically invalid (log of negative number etc.)
* `eis_err_out_of_range` Parameter to this function is out of valid range in a non-mathematical way
* `eis_err_bad_value` Parameter to this function is invalid in some non-mathematical way

## Stack variables

As well as normal variables that are defined by a getter function there are stack variables. These map a name to an existing stack. This allows a host code to let a user save stacks for later reuse. Stack variables are *NOT* implemented by evaluating the stack and storing the value. The stack is stored and pasted into the new stack where the name appears. A stack variable therefore has exactly the same behaviour as typing the text that created the stack into the expression that uses it. Only stack variables that return a single value can validly be used in mathematical expressions.

Create a stack variable from an existing stack with `eis_add_stack_variable`, which takes the parser, the name, the handle of the stack, an error code and the parameters. The stack is copied, so you can carry on using your stack. Create one directly from text with `eis_add_stack_expression_variable`, which takes the parser, the name, the text and the same remaining arguments.

```c
stack = eis_create_stack(parser, txt, &bm, &errcode, NULL);
eis_add_stack_variable(parser, "stackvar", stack, &errcode, NULL);
```

## Deferred constants/variables/stack variables/functions

Constants, variables, stack variables and functions can be introduced into the parser as deferred. Deferred items have names registered for them but not implementations. It is valid to create stacks that contain deferred items but not to evaluate them until implementations are provided.

* `eis_add_function` and `eis_add_variable` with a `NULL` function make a deferred function or variable
* `eis_add_deferred_constant` makes a deferred constant
* `eis_add_stack_variable` with a stack handle of -1, or `eis_add_stack_expression_variable` with a `NULL` text, makes a deferred stack variable
* `eis_add_pointer_variable` with a `NULL` pointer makes a deferred pointer variable

To provide the implementation make a normal call to the function that adds that kind of item, specifying the value, function or stack.

## Emplaced functions

Stack variables replace a single name with an existing stack, but sometimes you want the user to select the stack with a parameter to a function. For example, in the EPOCH code that EIS comes from the `density` function takes the name of a species as a parameter and provides the stack for the density of that species.

Emplaced functions are defined by an emplacement function

```c
void emplace_fn(char *name, int nparams, EIS_NUM *params, void *host_params,
    int *stack_out, EIS_STATUS *status, EIS_ERROR *errcode);
```

It does not return a value. `name` is the symbol that the user typed and `params` holds the parameters, which are numbers. Select a stack and set `*stack_out` to its handle. Report errors through `*errcode` in the same way as for a normal function. If the stack that you hand back is a temporary that you want EIS to delete once it has been put in place then set `*status` to include `eis_status_delete_stack`.

```c
int stacks[2];

void emplace_func(char *name, int nparams, EIS_NUM *params,
    void *host_params, int *stack_out, EIS_STATUS *status, EIS_ERROR *errcode)
{
  int n = round(params[0]);
  if (n < 1 || n > 2) {
    errcode[0] = eis_err_bad_value;
    return;
  }
  stack_out[0] = stacks[n-1];
}

eis_add_emplaced_function(parser, "emfunc", emplace_func, 1, &errcode, NULL);
```

`eis_add_emplaced_function` takes the parser, the name, the emplacement function, the number of expected parameters, an error code and the parameters. The function must not be `NULL`. Emplaced functions cannot be added globally or deferred.

An expression that uses an emplaced function produces a special emplacement item in the stack. The stack cannot be evaluated until an emplacement action is performed. Do this with `eis_emplace_stack`, which takes the stack, the host parameters that are passed to the emplacement function, an error code and the parameters.

```c
eis_emplace_stack(stack, NULL, &errcode, NULL);
```

By default `eis_emplace_stack` permanently changes the stack, replacing the emplacement item with the stack from the emplacement function. If you set the optional parameter `stack_id_out` in `eis_emplace_stack_params` to the handle of another existing stack then the emplaced stack is built in that stack and the original keeps its emplacement item, so it can be emplaced again with different host parameters. The default for `stack_id_out` is -1, which means emplace in place.

Evaluating a stack that has not been emplaced reports the error `eis_err_has_emplaced`.

## Changing a parser item

You can change the behaviour of a parser item by calling the function that adds an item of that kind again with the same name but a different value, function or stack. The previous behaviour for that name is forgotten and all future stacks created by this parser use the new behaviour. Stacks that have *already* been created are unaffected and keep using the original values and getter functions. If you want to change the behaviour of a stack after it has been created then use emplaced functions.

There is no way to prevent this overriding. The C interface currently has no routine to test whether a symbol is already known to the parser.

## Capability bits

Quite commonly you will want to flag a stack as having certain effects, such as being time varying or space varying. This can be automated with capability bits. Capability bits are a 64 bit bitmask that you can specify with the optional parameter `cap_bits` when you add a variable, function or constant. When a stack is created the bitwise OR of the capability bits of all of its elements is returned through the `EIS_BITMASK` pointer that you pass to `eis_create_stack`, `eis_populate_stack` or `eis_append_stack`. If a variable that indicates time is given the `is_time_varying` bit then any stack containing that variable is flagged as time varying.

## Multiple parsers

Parsers are fully self contained by default. Adding a function, variable or constant to one parser does not add it to any other. Set the optional parameter `global` to 1 to add the item to all parsers. Items are searched for in each parser's local list before the global list, so you can hide a global item for a single parser by adding an item with the same name without `global`.

As a consequence, take care when you evaluate a stack with a parser other than the one that created it. A stack that does not contain deferred or emplaced items will evaluate as expected because the stack elements point directly to their evaluation functions. A stack that does contain emplaced or deferred items uses the names, getter functions and stacks of the parser that you evaluate or emplace it against. Evaluating through the C interface always uses the parser that the stack was created with.

As a general rule it is recommended that codes use a single parser unless they have a definite reason to do otherwise.

## Parsers and thread safety

The tables that hold the parser and stack handles are not synchronised. Creating, copying and releasing parsers and stacks, and anything that adds an item to a parser, must therefore be done from one thread at a time, either from a single thread or inside a critical section. Create each parser and add all of its constants, functions and variables on a single thread before using it.

The safe way to use EIS from several threads is one parser per thread, with all of your getter functions being thread safe. The getter functions in EIS are thread safe. Sharing one parser between threads is only safe if the implementation that you have built EIS with says that it is.

## Overriding the built in parser items

The built in items are added to the global lists when the parser is first created. They can be overridden by adding items with the same name. No warning is given if you do.

## Parser result functions

Sometimes a host code wants a stack that returns the values from a function rather than evaluating a textual expression. This might provide higher performance for common operations, allow the code to override user supplied expressions or tie a scripting language to a program. This is done with a result function

```c
void result_fn(int *nresults, EIS_NUM *results, void *host_params,
    EIS_STATUS *status_code, EIS_ERROR *errcode);
```

Bind it to a stack with `eis_bind_result_to_stack`, which takes the stack, the function, an error code and the parameters. When the stack is evaluated the result function is called. It receives the host parameters in the same way as the getter functions. It should write one or more results to `results` and set `*nresults` to the number of results, or report an error. The initial value of `*nresults` is the size of the `results` array. If this is not enough then set `*nresults` to the number of results that you want and do not write to `results`. EIS makes the array that large and calls the function again. If the number of results is not constant then the function must cope with this repeat call or it is not guaranteed to finish. The initial value of `*nresults` is at least 1. A stack that has a result function can be used as a stack variable but must return a single value to be used in a mathematical expression. Report errors through `*errcode` in the usual way.

```c
void get_values(int *nresults, EIS_NUM *results, void *host_params,
    EIS_STATUS *status_code, EIS_ERROR *errcode)
{
  data *mydata = (data*) host_params;
  nresults[0] = 1;
  results[0] = sin(4.0 * PI * (mydata->x - 0.5))
      * cos(6.0 * PI * (mydata->y - 0.5));
}

stack = eis_create_stack(parser, NULL, &bm, &errcode, NULL);
eis_bind_result_to_stack(stack, get_values, &errcode, NULL);
```

## Pointer variables

A parser variable can be defined by a pointer to a variable in your code rather than a getter function. The parser keeps the pointer and returns the current value of the variable when a stack that uses the name is evaluated. If the variable is flagged as simplifiable then the value at the time the stack is simplified is used instead. Add a pointer variable with `eis_add_pointer_variable`, which takes the parser, the name, the pointer, whether the pointer is to an integer (1) or a real (0), whether it is to a 64 bit (1) or 32 bit (0) value, an error code and the parameters. The pointer can be to an `int32_t`, an `int64_t`, a `float` or a `double`. A `NULL` pointer makes a deferred variable.

```c
int val = 0;

/*Optional parameters are used here*/
eis_add_pointer_variable_params ptr_params;
eis_add_pointer_variable_params_init(&ptr_params);
ptr_params.cap_bits = 1;
ptr_params.can_simplify = 1;
eis_add_pointer_variable(parser, "ptrvar", &val, 1, 0, &errcode, &ptr_params);
```

This example uses the optional parameters `cap_bits` and `can_simplify`. It gives the variable a capability bit of 1 so that you can tell when an expression uses it, which then lets the code increment `val` and see that the new value is used.

Pointer variables have substantial drawbacks. EIS cannot check that the pointer is still valid when the stack is evaluated. Since the pointer is carried by every stack that uses the symbol, a pointer to memory that is freed makes every stack that used the symbol invalid and they cannot be repaired without being created again. The pointer must live as long as the parser. Getter functions are therefore the main way of defining variables.

## Derivatives

EIS can differentiate a stack symbolically. You give `eis_get_deriv_stack` a stack, a stack to hold the result and the name to differentiate with respect to. The stack that is to hold the derivative must already exist, for example one made with `eis_create_stack` with a `NULL` expression, and its previous contents are replaced. The result is an ordinary stack that you evaluate with `eis_evaluate_stack`. If the parser is set to simplify then the derivative stack is simplified.

```c
dstack = eis_create_stack(parser, NULL, &bm, &errcode, NULL);
eis_get_deriv_stack(fstack, dstack, "x", &errcode, NULL);
```

Stack variables and stack functions are expanded when an expression is parsed, so the derivative is taken of the fully expanded expression.

### Built in derivatives

The operators and many of the built in mathematical functions have derivatives registered for them, so expressions that only use built in symbols can be differentiated straight away. The built in functions `gamma`, `log_gamma`, `dsemigauss` and `deriv_interpolate` have locked derivatives, so differentiating through one is an error. Functions that you add yourself can be differentiated once you have registered a derivative for them.

### Symbols with no derivative

A symbol that has no derivative registered is treated as having a derivative of zero. That is correct for constants but is silently wrong for a function or variable that depends on the variable that you are differentiating with respect to. A functor is the exception. It is locked when it is added, so differentiating through one is an error until you register a derivative for it.

### Registering a derivative

Register a derivative with `eis_set_symbol_derivative`, which takes the parser, the name of the symbol, the derivative expression, the name of the variable that the derivative is with respect to, an error code and the parameters. The symbol must already have been added.

```c
eis_set_symbol_derivative(parser, symbol, deriv, wrt, &errcode, NULL);
```

The derivative expression is a normal parser expression that can also use four special items

* `param(n)` - The nth parameter that the function was called with. `n` is 1 for the first parameter
* `dparam(n)` - The derivative of the nth parameter with respect to the variable being differentiated. This applies the chain rule, so the derivative expression for a function normally multiplies by `dparam` of the parameter that it differentiates
* `func(...)` - The original function, called with the parameters that you give it
* `all_params` and `all_dparams` - Every parameter, or every derivative of a parameter, passed on as a list. These can only be given to a variadic function

For example, a function `cube` that returns the cube of its single parameter has the derivative `3 * param(1)^2 * dparam(1)`. A variable has no parameters, so its derivative with respect to itself is simply 1. Pass `NULL` as the name of the variable to say that the derivative applies with respect to any variable, which is the usual choice for a function. A variable normally needs its own name here because its derivative with respect to a different variable is zero.

```c
eis_add_variable(parser, "x", get_x, &errcode, NULL);
eis_set_symbol_derivative(parser, "x", "1", "x", &errcode, NULL);

eis_add_function(parser, "cube", cube_fn, 1, &errcode, NULL);
eis_set_symbol_derivative(parser, "cube", "3 * param(1)^2 * dparam(1)", NULL,
    &errcode, NULL);

fstack = eis_create_stack(parser, "cube(x) + 2 * x", &bm, &errcode, NULL);
dstack = eis_create_stack(parser, NULL, &bm, &errcode, NULL);
eis_get_deriv_stack(fstack, dstack, "x", &errcode, NULL);
ct = eis_evaluate_stack(dstack, 1, &res, &mydata, &errcode, NULL);
```

The expression is parsed by the same parser, so every name that it uses must already be known. If it cannot be parsed then the error is returned in the error code. The optional parameter `global` of `eis_set_symbol_derivative_params` registers the derivative in the global list shared by all parsers.

The program in `demos/parser_c/demo12.c` uses a derivative stack to find the root of an expression by Newton-Raphson iteration.

### Locking a derivative

Some symbols have no meaningful derivative, for example a function that returns a rounded value from a table lookup. To stop these being treated as having a derivative of zero, lock them with `eis_lock_symbol_derivative`, which takes the parser, the name of the symbol, the name of the variable (or `NULL` for any variable), an error code and the parameters.

```c
eis_lock_symbol_derivative(parser, "mytable", NULL, &errcode, NULL);
```

Taking the derivative of a stack that contains a locked symbol fails with the error `eis_err_no_deriv`. The optional parameter `global` of `eis_lock_symbol_derivative_params` locks the symbol in the global list. Registering a derivative for a locked symbol replaces the lock for the same variable.

### Changing a symbol

Adding a constant, variable or function again with the same name replaces the whole entry for that name, which includes its derivatives, so register the derivative again after changing one. Stacks that were created before the change are not affected.

## Looking at stacks

`eis_get_infix_stack` converts a stack back into infix maths, after simplification, emplacement and inlining of stack variables. The bracketing is not guaranteed to match the original expression but the result is mathematically equivalent. `eis_visualise_stack` returns a graphviz dot description of the stack. Both take the handle of the stack, the size of your buffer, the buffer and the parameters, and return the number of characters needed, so pass a `NULL` buffer to find out the size.

```c
char str[1000];
eis_get_infix_stack(stack, 1000, str, NULL);
printf("Expanded expression was %s\n", str);
```

## Descriptions

When adding any symbol you can give it a description by setting the optional parameter `description`. Descriptions are not persistent. If a symbol is redefined its description is lost unless you supply it again. There is currently no C routine that returns the generated markdown description of a parser, but the descriptions are stored with the symbols.

## Releasing parsers and stacks

When you have finished with them release stacks with `eis_stack_dec_ref` and parsers with `eis_parser_dec_ref`. These are optional for a program that exits straight away. Using a handle after it has been released is an error that is reported as `eis_err_bad_stack` or `eis_err_bad_parser` by the functions that return an error code.
