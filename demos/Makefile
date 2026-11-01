# Choose Fortran compiler (intel, intel_ifx, pgi, gfortran, flang, ibm or cray)
# using COMPILER= when invoking make or get the default of gfortran
COMPILER ?= gfortran

# Choose C compiler (intel, intel_ifx, pgi, gcc, clang, ibm or cray) using
# C_COMPILER= when invoking make or get the default of gcc
C_COMPILER ?= gcc

# Architecture... (default 64 bit, use ARCH=linux-ix86 on command line
# for 32 bit)
ARCH ?= linux-ix86_64

BINDIR = bin
MODDIR = modules
COMDIR = ../common/include
PARDIR = ../parser/include
DECKDIR = ../deck/include

CPU = p7

ifeq (linux-ix86_64,$(ARCH))
   M64_FLAG = -m64
else
   M64_FLAG =
endif

# Standard to build against (default f2018, use STANDARD=f2008 or
# STANDARD=f2003 on command line for f2008 or f2003)
STANDARD ?= f2018

# Support for multiple compiler flags

FC ?= gfortran
D = -D
DEFINES := $(DEFINE)
ifeq (f2008,$(findstring f2008,$(STANDARD)))
  DEFINES += $(D)F2008
endif
ifeq (f2018,$(findstring f2018,$(STANDARD)))
  DEFINES += $(D)F2008 $(D)F2018
endif

# Fortran compilers
# =================

# PGI
# ===
ifeq ($(strip $(COMPILER)),pgi)
  FFLAGS = -O2 -Mvect -Munroll
  ifeq ($(strip $(MODE)),debug)
    FFLAGS = -O0 -g -C -Ktrap=denorm -Mbounds -Mchkfpstk -Mdepchk -Mstandard
  endif
  ifeq (prof,$(findstring prof,$(MODE)))
    FFLAGS += -Mprof=func,lines
  endif
  FFLAGS += -Mnodefaultunit -Ktrap=fp -Mdclchk -tp $(CPU) -mcmodel=medium
  MODFLAG := -I/usr/include -I$(MODDIR) -I$(COMDIR) -I$(PARDIR) -I$(DECKDIR)
  MODULEFLAG := $(MODFLAG) -module $(MODDIR)
  FC = pgf90
endif

# Intel
# =====
ifeq ($(strip $(COMPILER)),intel)
  FFLAGS = -O3 -g
  ifeq (debug,$(findstring debug,$(MODE)))
    FFLAGS = -O0 -g -u -C -warn -nothreads -traceback -fltconsistency -ftrapuv \
        -fpic
    ifeq ($(strip $(SYSTEM)),Darwin)
      FFLAGS += -Wl,-no_pie
    endif
  endif
  ifeq (prof,$(findstring prof,$(MODE)))
    FFLAGS += -p
  endif
  FFLAGS += -fpe0 -mcmodel=medium -heap-arrays 64
  MODFLAG := -I/usr/include -I$(MODDIR) -I$(COMDIR) -I$(PARDIR) -I$(DECKDIR)
  MODULEFLAG := $(MODFLAG) -module $(MODDIR)
  FC = ifort
  ifeq (f2018,$(findstring f2018,$(STANDARD)))
    FFLAGS += -stand f18
  else ifeq (f2008,$(findstring f2008,$(STANDARD)))
    FFLAGS += -stand f08
  else
    FFLAGS += -stand f03
  endif
endif

# Intel (oneAPI)
# ==============
ifeq ($(strip $(COMPILER)),intel_ifx)
  FFLAGS = -O3 -g
  ifeq (debug,$(findstring debug,$(MODE)))
    FFLAGS = -O0 -g -u -check all -warn all -traceback -ftrapuv -fpic
  endif
  ifeq (prof,$(findstring prof,$(MODE)))
    FFLAGS += -p
  endif
  FFLAGS += -fpe0 -mcmodel=medium -heap-arrays 64
  MODFLAG := -I/usr/include -I$(MODDIR) -I$(COMDIR) -I$(PARDIR) -I$(DECKDIR)
  MODULEFLAG := $(MODFLAG) -module $(MODDIR)
  FC = ifx
  ifeq (f2018,$(findstring f2018,$(STANDARD)))
    FFLAGS += -stand f18
  else ifeq (f2008,$(findstring f2008,$(STANDARD)))
    FFLAGS += -stand f08
  else
    FFLAGS += -stand f03
  endif
endif

# gfortran
# ========
ifeq ($(strip $(COMPILER)),gfortran)
  ifeq (f2008,$(findstring f2008,$(STANDARD)))
    FFLAGS = -O3 -g -std=f2008
  else ifeq (f2018,$(findstring f2018,$(STANDARD)))
    FFLAGS = -O3 -g -std=f2018
  else
    FFLAGS = -O3 -g -std=f2003
  endif
  ifeq (debug,$(findstring debug,$(MODE)))
    FFLAGS = -O0 -g -Wall -Wextra -pedantic -fbounds-check \
             -ffpe-trap=invalid,zero,overflow -Wno-unused-parameter \
             -Wno-surprising -ffpe-trap=underflow,denormal -fimplicit-none

    GNUVER := $(shell gfortran -dumpversion | head -1 \
        | sed 's/[^0-9\.]*\([0-9\.]\+\).*/\1/')
    GNUMAJOR := $(shell echo $(GNUVER) | cut -f1 -d\.)
    GNUMINOR := $(shell echo $(GNUVER) | cut -f2 -d\.)

    # Allow for 99 minor revisions
    GNUVER := $(shell expr 100 \* $(GNUMAJOR) + $(GNUMINOR))

    # gfortran-4.3
    GNUGE43 := $(shell expr $(GNUVER) \>= 403)
    ifeq "$(GNUGE43)" "1"
      FFLAGS += -fbacktrace -fdump-core

      # gfortran-4.6
      GNUGE46 := $(shell expr $(GNUVER) \>= 406)
      ifeq "$(GNUGE46)" "1"
        FFLAGS += -Wno-unused-dummy-argument

        # gfortran-4.8
        GNUGE48 := $(shell expr $(GNUVER) \>= 408)
        ifeq "$(GNUGE48)" "1"
          FFLAGS += -Wno-target-lifetime
        endif
      endif
    endif
  endif
  ifeq (prof,$(findstring prof,$(MODE)))
    FFLAGS += -p
  endif
  FFLAGS += -frecord-marker=4
  MODFLAG := -I$(MODDIR) -I$(COMDIR) -I$(PARDIR) -I$(DECKDIR)
  ifneq ($(wildcard /usr/include/.),)
    MODFLAG := -I/usr/include $(MODFLAG)
  endif
  MODULEFLAG := $(MODFLAG) -J$(MODDIR)
  FC = gfortran
endif

# flang
# =====
ifeq ($(strip $(COMPILER)),flang)
  FFLAGS = -O3 -g
  ifeq (debug,$(findstring debug,$(MODE)))
    FFLAGS = -O0 -g
  endif
  ifeq (prof,$(findstring prof,$(MODE)))
    FFLAGS += -p
  endif
  MODFLAG := -I$(MODDIR) -I$(COMDIR) -I$(PARDIR) -I$(DECKDIR)
  ifneq ($(wildcard /usr/include/.),)
    MODFLAG := -I/usr/include $(MODFLAG)
  endif
  MODULEFLAG := $(MODFLAG) -J$(MODDIR)
  FC = flang
  ifeq (f2018,$(findstring f2018,$(STANDARD)))
    FFLAGS += -std=f2018
  endif
endif

# IBM Bluegene
# ============
ifeq ($(strip $(COMPILER)),ibm)
  FFLAGS = -O5 -qhot -qipa # Optimised
  ifeq ($(strip $(MODE)),debug)
    FFLAGS = -O0 -C -g -qfullpath -qinfo
  endif
  MODFLAG := -I/usr/include -I$(MODDIR) -I$(COMDIR) -I$(PARDIR) -I$(DECKDIR)
  MODULEFLAG := $(MODFLAG) -qmoddir=$(MODDIR)
  ifeq (f2008,$(findstring f2008,$(STANDARD)))
    FC = xlf2008
  else ifeq (f2018,$(findstring f2018,$(STANDARD)))
    FC = xlf2018
  else
    FC = xlf2003
  endif

  # IBM compiler needs a -WF to recognise preprocessor directives
  D = -WF,-D
endif

# Cray
# ========
ifeq ($(strip $(COMPILER)),cray)
  FFLAGS = -O3
  ifeq ($(strip $(MODE)),debug)
    FFLAGS = -O0 -g -ea -ec -eC -eD -eI -en -hfp_trap -Ktrap=fp -m0 -M1438,7413
  endif
  MODFLAG := -em -I/usr/include -I$(MODDIR) -I$(COMDIR) -I$(PARDIR) \
      -I$(DECKDIR)
  MODULEFLAG := $(MODFLAG) -J$(MODDIR)
  FC = ftn
endif

PREPROFLAGS = $(DEFINES)

# C compilers. FRTLIBS are the Fortran runtime libraries that the C compiler
# needs to be told about when linking against the Fortran libraries
# ==========================================================================

# PGI
ifeq ($(strip $(C_COMPILER)),pgi)
  CC = pgcc
  CFLAGS = -O2
  ifeq ($(strip $(MODE)),debug)
    CFLAGS = -O0 -g
  endif
  ifeq (prof,$(findstring prof,$(MODE)))
    CFLAGS += -Mprof=func,lines
  endif
  FRTLIBS = -pgf90libs
endif

# Intel
ifeq ($(strip $(C_COMPILER)),intel)
  CC = icc
  CFLAGS = -O3 -g
  ifeq (debug,$(findstring debug,$(MODE)))
    CFLAGS = -O0 -g -traceback -fpic
  endif
  ifeq (prof,$(findstring prof,$(MODE)))
    CFLAGS += -p
  endif
  FRTLIBS = -lifcore
endif

# Intel (oneAPI)
ifeq ($(strip $(C_COMPILER)),intel_ifx)
  CC = icx
  CFLAGS = -O3 -g
  ifeq (debug,$(findstring debug,$(MODE)))
    CFLAGS = -O0 -g -traceback -fpic
  endif
  ifeq (prof,$(findstring prof,$(MODE)))
    CFLAGS += -p
  endif
  FRTLIBS = -lifcore
endif

# GNU
ifeq ($(strip $(C_COMPILER)),gcc)
  CC = gcc
  CFLAGS = -O3 -g
  ifeq (debug,$(findstring debug,$(MODE)))
    CFLAGS = -O0 -g -Wall -Wextra -pedantic
  endif
  ifeq (prof,$(findstring prof,$(MODE)))
    CFLAGS += -p
  endif
  FRTLIBS = -lgfortran
endif

# clang
ifeq ($(strip $(C_COMPILER)),clang)
  CC = clang
  CFLAGS = -O3 -g
  ifeq (debug,$(findstring debug,$(MODE)))
    CFLAGS = -O0 -g -Wall -Wextra -pedantic
  endif
  ifeq (prof,$(findstring prof,$(MODE)))
    CFLAGS += -p
  endif
  FRTLIBS = -lflang -lflangrti
endif

# IBM Bluegene
ifeq ($(strip $(C_COMPILER)),ibm)
  CC = xlc
  CFLAGS = -O5 -qhot -qipa
  ifeq ($(strip $(MODE)),debug)
    CFLAGS = -O0 -g -qfullpath -qinfo
  endif
  FRTLIBS = -lxlf90
endif

# Cray
ifeq ($(strip $(C_COMPILER)),cray)
  CC = cc
  CFLAGS = -O3
  ifeq ($(strip $(MODE)),debug)
    CFLAGS = -O0 -g
  endif
  FRTLIBS =
endif

# utils
RM      = rm
MKDIR   = mkdir

CINCFLAGS = -I$(COMDIR) -I$(PARDIR) -I$(DECKDIR)

LIBDIRS = -L../deck/lib -L../parser/lib -L../common/lib
PARSERLIBS = -leis_parser -leis_common
DECKLIBS = -leis_deck -leis_parser -leis_common

PARSERDIR = $(BINDIR)/Fortran/parser
DECKDIR_F = $(BINDIR)/Fortran/deck
PARSERDIR_C = $(BINDIR)/C/parser
DECKDIR_C = $(BINDIR)/C/deck
OUTDIRS = $(PARSERDIR) $(DECKDIR_F) $(PARSERDIR_C) $(DECKDIR_C)

PARSER_F = $(patsubst parser/%.F90,$(PARSERDIR)/%,$(wildcard parser/*.F90))
DECK_F = $(patsubst deck/%.F90,$(DECKDIR_F)/%,$(wildcard deck/*.F90))
PARSER_C = $(patsubst parser_c/%.c,$(PARSERDIR_C)/%,$(wildcard parser_c/*.c))
DECK_C = $(patsubst deck_c/%.c,$(DECKDIR_C)/%,$(wildcard deck_c/*.c))

# Not real file targets
.PHONY: all demo democlean

all: demo

demo: $(OUTDIRS) $(MODDIR) $(PARSER_F) $(DECK_F) $(PARSER_C) $(DECK_C)

# cleanup
democlean:
	-$(RM) -rf $(BINDIR)
	-$(RM) -rf $(MODDIR)

$(OUTDIRS) $(MODDIR):
	$(MKDIR) -p $@

$(PARSERDIR)/%: parser/%.F90 | $(PARSERDIR) $(MODDIR)
	$(FC) $(PREPROFLAGS) $(FFLAGS) $(MODULEFLAG) -o $@ $< $(LIBDIRS) \
	    $(PARSERLIBS)

$(DECKDIR_F)/%: deck/%.F90 | $(DECKDIR_F) $(MODDIR)
	$(FC) $(PREPROFLAGS) $(FFLAGS) $(MODULEFLAG) -o $@ $< $(LIBDIRS) \
	    $(DECKLIBS)

$(PARSERDIR_C)/%: parser_c/%.c | $(PARSERDIR_C)
	$(CC) $(CFLAGS) $(CINCFLAGS) -o $@ $< $(LIBDIRS) $(PARSERLIBS) \
	    $(FRTLIBS) -lm

$(DECKDIR_C)/%: deck_c/%.c | $(DECKDIR_C)
	$(CC) $(CFLAGS) $(CINCFLAGS) -o $@ $< $(LIBDIRS) $(DECKLIBS) \
	    $(FRTLIBS) -lm
