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
