# The EPOCH input system (EIS)

## Overview

EIS is intended to be an easy to use but powerful and performant system to allow control of Fortran and C codes from external user specified files using standard mathematical expressions. It is split into three sections, two of which are currently released.

1) eis\_common (common subdirectory) - The common core of the EIS library, used by the other parts
2) eis\_parser (parser subdirectory) - The maths expression parser - currently released
3) eis\_deck - The input deck parser - not currently released

## Terms

Host Code - the code that is using the EIS library

Developer - the person who is writing the Host Code

User - The person using the Host Code

## What is EIS?

EIS is intended to be a simple way of adding an input deck control system to a Fortran or C code. The parser allows you to easily add evaluation of mathematical expressions to numbers, allowing you to easily add custom functions, constants and variables. As well as the parameters that users pass to the functions in the evaluated expressions your code can also pass parameters to the evaluation system so you can easily choose to evaluate an expression over a spatial range, a temporal range or simply provide a unique ID for the type of item that you want to evaluate the expression over. The EIS deck system (when it is released) will be able to read heirarchical "input deck" files. These files will be in the style of the EPOCH particle in cell code and consist of separate blocks of information with associated keys

## Getting EIS

If you have this file you have probably downloaded EIS or are using a code with EIS in it. EIS can be obtained from Github at https://github.com/csbrady-warwick/EIS-2

## EIS License

EIS is licenced under the 3 clause BSD license. This means that EIS can be used freely in both open and closed source software so long as the copyright and license notices are retained. We ask that codes that make use or EIS make note of the fact that they are using EIS and link to the Github page.

## EIS Support

EIS is provided with no warranties as to fitness or suitability for any particular purpose as described in the license. Support for using the EIS parser core will be provided on a "best effort" basis through the support forums on Github.

## Compiling EIS

EIS is compiled using the GNU make system. There is a make system for each section and a global Makefile in the root directory that builds all of the sections of the library. The makefile needs to be told which compiler will be building EIS.

EIS's makefiles have built in support for the following compilers
1) Portland Group (command line is pgf90)
2) Intel (command line is ifort)
3) gfortran (command line is gfortran)
4) IBM XL Fortran (command line is xlf{numbers}, EIS must be compiled with xlf2003)
5) Cray Fortran (command line is ftn)

To select a compiler you pass a **COMPILER** command to the make command, e.g. `make COMPILER={flag}`. The compiler commands are

1) Portland Group - pgi
2) Intel - intel
3) gfortran - gfortran
4) IBM XL Fortran - ibm
5) Cray - cray

So to compile EIS with the intel compiler, type `make COMPILER=intel` in the root directory of EIS. 

based on information from the [Fortran Wiki compatability matrix] (http://fortranwiki.org/fortran/show/Fortran+2003+status) NAG and Oracle compilers should also work with EIS but since we do not have access to these compilers to test we are not providing compilation information.


## Note on Fortran libraries

Unlike C, Fortran doesn't have any concept of a header file to describe the interfaces for functions and the equivalent `.mod` files are compiler specific. The compilers similarly have different standards of how they create names for functions and variables. This means that it is not possible to compile EIS with one Fortran compiler and then use the compiled library with another Fortran compiler. You have to compile both the library and the code that uses it with the same compiler.

Some parts of EIS are written using the Fortran 2003 Fortran/C interoperability routines. These routines do have a defined interface so can be called from C code compiled with a different compiler, but these routines are not as fully featured or user friendly as the Fortran interfaces.

## Compiling against EIS - Fortran

Details of the modules in EIS are provided in the guide on using the library, but to compile codes using EIS you must add the directories `common/include`, `parser/include` and `deck/include` in your compilation instructions

## Linking against EIS
By default EIS builds to one static library per section, with each section depending on the sections before it. So to link against eis\_deck you have to compile using `gfortran {host_code_files} libeis_deck.a libeis_parser.a libeis_common.a`.


## Using the parser

### Concepts

The core of the EIS parser is a parser object called an `eis_parser`. You then hand this parser a mathematical expression as a string and it either evaluates that string directly to an array of numbers or an `eis_stack` object that can be evaluated multiple times without the overhead of parsing the string again. When you evaluate a string or a stack you can optionally pass it an object that contains information provided by the host code rather than directly read in from the user. None of the built in routines make use of this information because it is entirely specified by the host code. This object might, for example, contain information about where in space a stack should be evaluated and you could add functions or variables that give a different result based on the values in this object

### Terms

Parser - A single instance of the `eis_parser` type

Literal  - A numerical value entered directly into the text to be parsed as a number (e.g. 1)

Constant - A value known by name to the parser to be always a fixed numerical value (e.g. kb for Boltzmann's constant)

Host Parameters - An object given by the host code that is available to all of the routines that evaluate the parts of a stack

Variable - A value known by name to the parser but where the value of it can only be determined when the stack is created or evaluated. It cannot take any parameters from the user but will in general use the host parameters

Stack Variable - A preexisting stack that is known by name to the parser. When the name is encounted the stack is placed into the new expression at the location of the name. This means that a mathematical expression can be stored and then reused in other expressions

Function - A routine known by name to the parser that takes a number of parameters from the user before evaluating

Variadic Function - A function that takes as many parameters as it is given rather than expecting a fixed number

Parser Item - A Function, Variable, Stack Variable or Constant

Deck Variable - A stack that is known by name to the parser so that it can be used in other expressions

Deferred - A Function, Variable or Constant that is assigned a name not assigned an evaluation function or value when it is first registered. A defered but can be used to create stacks anyway. An evaluation function or value must be assigned before the stack can be evaluated

Emplaced - A function that should return a stack rather than a value when it is evaluated. The stack is then put into place 

### Simple EIS parser program

The simplest parser program is just a glorified calculator and that can be implemented very easily indeed. You simply have to create a parser object and call it's 'evaluate' function. The evaluate function takes either a pretokenized stack (if you want to evaluate the same expression multiple times) or a string (for one off evaluation). It also takes an allocatable REAL array of kind `eis_num` that it will fill with the results of evaluating your stack. If the array handed to it is not allocated or not large enough to hold the number of returned items then it will be reallocated as needed. The number of results actually generated by the evaluation is returned from the function. The final parameter to evaluate is the error code which is an INTEGER of kind `eis_error`. This result is a bitmask that details the location and type of the error, but in general most codes will only want to check if it is the special value `eis_err_none`, and if it isn't then obtain the error report from the parser. The following code snippet shows how to use EIS as a simple calculator which will keep taking strings to evaluate until the program is killed.

```fortran
PROGRAM test

  USE eis_parser_mod
  USE eis_header
  TYPE(eis_parser) :: parser
  CHARACTER(LEN=1000) :: input
  INTEGER(eis_error) :: errcode
  REAL(eis_num), DIMENSION(:), ALLOCATABLE :: result
  INTEGER :: ct

  DO WHILE(.TRUE.)
    WRITE(*,'(A)', ADVANCE = 'NO') "Please input a mathematical expression :"
    READ(*,'(A)') input
    ct = parser%evaluate(input, result, errcode)
    IF (errcode == eis_err_none) THEN
      PRINT *,'Result is ', result(1:ct)
    ELSE
      CALL parser%print_errors()
    END IF
  END DO

END PROGRAM test

```

### Selecting a physics package
EIS includes several common physical constants built in but by default they are only available by typing their full name. To make them easily available you have to manually initialise the parser with a physics constant. This is easy to do. Before you use the parser for any other purpose simply issue the init instruction

```fortran
CALL parser%init(errcode)
```

Once again `errcode` is an integer of kind `eis\_error\_kind`. On it's own this function doesn't do much because the parser performs this type of minimal initialization if needed when it is first used. You can optionally specify several commands to the init routine but here we want to specify the physics type. You can run either of

```fortran
CALL parser%init(errcode, physics = eis_physics_si)
CALL parser%init(errcode, physics = eis_physics_cgs_gaussian)
```

The former will load the physical constants in SI into the default known constants and the second CGS Gaussian units. Other physics models may be added later.

If you want to access a physical constant without setting up a global physics model or want to access constants from a different physics model than the one selected then you have to use the full name. The full names are the names of the constants (see list of physical constants) but prepended with either

1) physics.si. (e.g. physics.si.kb is Boltzmann's constant)

2) physics.cgs.gaussian. (eg. physics.cgs.gaussian.kb is Boltzmann's constant)

### Adding a constant
EIS comes with a decent list of physical and mathematical constants built in

### More complex parser program

There isn't a huge need for evaluating simple mathematical expressions from compiled code so a more useful code would be one that allows you to specify a function in space and then evaluate it for some purpose. EIS has no intrinsic concept of a spatial domain so you have to write 

1) A type to store spatial information

2) Functions to allow the parser to access this information

The first is a simple Fortran Type, but it does have to be flagged as a C interoperable type and comply with the Fortran/C interoperability rules for types. The next section explains why this is required, for the moment just do it. The requirements for what can be in an interoperable type are quite strict and depend on the Fortran standard that you want to follow but if you stick to simple values, other interoperable types and arrays of fixed size then you should have no problems. If you are using Fortran/C interoperability then in general you will want to use the intrinsic `ISO\_C\_BINDING` module (although it isn't needed for everything). To mark a type as C interoperable simply add `BIND(C)` to the definition.

```fortran
  TYPE, BIND(C) :: data_item
    REAL(eis_num) :: x = 0.0_eis_num
    REAL(eis_num) :: y = 0.0_eis_num
  END TYPE data_item
```

In this case my type contains two reals of kind `eis\_num`. I didn't have to use an EIS defined kind, I've just chosen to do so here because it means that I'm passing my data at the same precision as EIS is using for calculation. I now need to create two Fortran functions that will be called when a user requests my `x` and `y` data. These functions have the same structure

```fortran
  FUNCTION getter(nparams, params, host_params, status_code, errcode) &
      RESULT(res) BIND(C)
    INTEGER(eis_i4), VALUE, INTENT(IN) :: nparams
    REAL(eis_num), DIMENSION(nparams), INTENT(IN) :: params
    TYPE(C_PTR), VALUE, INTENT(IN) :: host_params
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    REAL(eis_num) :: res
```

This is the definition of a Fortran function that is called by EIS when ever any function or variable is evaluated. Note that this function is also defined `BIND(C)` because it is possible to specify functions for the parser from C code. The function takes 3 parameters and returns 3 things, two through the argument list and one as the function result. The `nparams` argument is the number of parameters that were passed to a function and are irrelevant here because we are going to define a parser variable not a parser function but the underlying Fortran function is the same. `params` is an array containing the parameters that are passed to a parser function. Again they are not used here because we are defining a variable. `host_params` is going to be used here. It is a `C\_PTR`type, that is a C pointer that is going to contain the reference to the `data\_item` type that we are going to pass in.

```fortran
MODULE mymod

  USE ISO_C_BINDING
  USE eis_header

  TYPE, BIND(C) :: data_item
    REAL(eis_num) :: x = 0.0_eis_num
    REAL(eis_num) :: y = 0.0_eis_num
  END TYPE data_item

  CONTAINS

  FUNCTION get_x(nparams, params, host_params, status_code, errcode) &
      RESULT(res) BIND(C)
    INTEGER(eis_i4), VALUE, INTENT(IN) :: nparams
    REAL(eis_num), DIMENSION(nparams), INTENT(IN) :: params
    TYPE(C_PTR), VALUE, INTENT(IN) :: host_params
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    REAL(eis_num) :: res
    TYPE(data_item), POINTER :: dat

    CALL C_F_POINTER(host_params, dat)
    res = dat%x

  END FUNCTION get_x



  FUNCTION get_y(nparams, params, host_params, status_code, errcode) &
      RESULT(res) BIND(C)
    INTEGER(eis_i4), VALUE, INTENT(IN) :: nparams
    REAL(eis_num), DIMENSION(nparams), INTENT(IN) :: params
    TYPE(C_PTR), VALUE, INTENT(IN) :: host_params
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    REAL(eis_num) :: res
    TYPE(data_item), POINTER :: dat

    CALL C_F_POINTER(host_params, dat)
    res = dat%y

  END FUNCTION get_y

END MODULE mymod

PROGRAM test

  USE eis_parser_mod
  USE eis_header
  USE eis_parser_header
  USE mymod
  TYPE(eis_parser) :: parser
  TYPE(eis_stack) :: stack
  CHARACTER(LEN=1000) :: input
  INTEGER(eis_error) :: errcode
  REAL(eis_num), DIMENSION(:), ALLOCATABLE :: result
  INTEGER :: ct, ix, iy
  TYPE(data_item), TARGET :: item
  CHARACTER(LEN=:), ALLOCATABLE :: str

  CALL parser%add_variable('x', get_x, errcode, can_simplify = .FALSE.)
  CALL parser%add_variable('y', get_y, errcode, can_simplify = .FALSE.)

  WRITE(*,'(A)', ADVANCE = 'NO') "Please input a mathematical expression :"
  READ(*,'(A)') input
  CALL parser%tokenize(input, stack, errcode)
  ct = parser%evaluate(stack, result, errcode, host_params = C_LOC(item))
  IF (errcode /= eis_err_none) THEN
    CALL parser%print_errors()
    STOP
  END IF
  DO ix = 1, 100
    item%x = REAL(ix-1, eis_num)/99.0_eis_num
    DO iy = 1 , 100
      item%y = REAL(iy-1, eis_num)/99.0_eis_num
      ct = parser%evaluate(stack, result, errcode, host_params = C_LOC(item))
      WRITE(10,*) result(1)
    END DO
  END DO

END PROGRAM test

```
