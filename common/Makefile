# Choose compiler (intel, pgi, g95 or gfortran) using COMPILER= when
# invoking make or get the default of pgi
COMPILER ?= pgi

# Architecture... (default 64 bit, use ARCH=linux-ix86 on command line
# for 32 bit)
ARCH ?= linux-ix86_64

SRCDIR = src
OBJDIR = obj
LIBDIR = lib
INCDIR = include
MODDIR = modules

GIT_WORK_TREE = "."
PACK_PREFIX = sdf
PACK_SOURCE_CODE = 1
PACK_GIT_DIFF = 1
PACK_GIT_DIFF_FROM_ORIGIN = 1
GENERATE_CHECKSUM = 1
F77_OUTPUT = 0
PACK_OPTS = $(GIT_WORK_TREE) sdf $(PACK_SOURCE_CODE) $(PACK_GIT_DIFF) \
    $(PACK_GIT_DIFF_FROM_ORIGIN) $(GENERATE_CHECKSUM) $(F77_OUTPUT)

CPU = p7
AR  = ar

ifeq (linux-ix86_64,$(ARCH))
   M64_FLAG = -m64
else
   M64_FLAG =
endif

# Support for multiple compiler flags

FC ?= gfortran
D = -D

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
  MODFLAG := -I/usr/include -I$(OBJDIR) -I$(MODDIR)
  MODULEFLAG := $(MODFLAG) -module $(MODDIR)
  FC = pgf90
endif

# Intel
# =====
ifeq ($(strip $(COMPILER)),intel)
  FFLAGS = -O3 -g
  # If you use the -ipo flag then you must also create the library file using
  # Intel's xiar utility.
  #AR = xiar
  #FFLAGS = -O3 -ipo
  #FFLAGS = -O3 -ipo -xHost # Optimised (B)
  #FFLAGS = -O3 -ipo -xAVX  # Optimised (W)
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
  MODFLAG := -I/usr/include -I$(MODDIR) -I$(OBJDIR)
  MODULEFLAG := $(MODFLAG) -module $(MODDIR)
  FC = ifort
endif

# gfortran
# ========
ifeq ($(strip $(COMPILER)),gfortran)
  FFLAGS = -O3 -g
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
  MODFLAG := -I$(MODDIR) -I$(OBJDIR)
  ifneq ($(wildcard /usr/include/.),)
    MODFLAG := -I/usr/include $(MODFLAG)
  endif
  MODULEFLAG := $(MODFLAG) -J$(MODDIR)
  INFO_FLAGS = -Wno-conversion -fno-range-check
  FC = gfortran
endif

# g95
# ========
ifeq ($(strip $(COMPILER)),g95)
  FFLAGS = -O3
  ifeq (debug,$(findstring debug,$(MODE)))
    FFLAGS = -O0 -g -Wall -Wimplicit-none -ftrace=full -fbounds-check -fzero
  endif
  ifeq (prof,$(findstring prof,$(MODE)))
    FFLAGS += -p
  endif
  FFLAGS += -fPIC -Wno=155 $(M64_FLAG)
  MODFLAG := -I/usr/include -I$(MODDIR) -I$(OBJDIR)
  MODULEFLAG := $(MODFLAG) -fmod=$(MODDIR)
  FC = g95
endif

# IBM Bluegene
# ============
ifeq ($(strip $(COMPILER)),ibm)
  FFLAGS = -O5 -qhot -qipa # Optimised
  ifeq ($(strip $(MODE)),debug)
    FFLAGS = -O0 -C -g -qfullpath -qinfo #-qkeepparm -qflttrap \
          -qnosmp -qxflag=dvz -Q! -qnounwind -qnounroll # Debug
    #FFLAGS = -O0 -qarch=qp -qtune=qp
    #FFLAGS = -qthreaded -qsmp=noauto -qsmp=omp # Hybrid stuff
  endif
  MODFLAG := -I/usr/include -I$(MODDIR) -I$(OBJDIR)
  MODULEFLAG := $(MODFLAG) -qmoddir=$(MODDIR)
  FC = xlf2003

  # IBM compiler needs a -WF to recognise preprocessor directives
  D = -WF,-D
endif

# HECToR
# ========
ifeq ($(strip $(COMPILER)),hector)
  FFLAGS = -O3
  ifeq ($(strip $(MODE)),debug)
    FFLAGS = -O0 -g -ea -ec -eC -eD -eI -en -hfp_trap -Ktrap=fp -m0 -M1438,7413
  endif
  MODFLAG := -em -I/usr/include -I$(MODDIR) -I$(OBJDIR)
  MODULEFLAG := $(MODFLAG) -J$(MODDIR)
  FC = ftn
endif

# Don't compile encoded source if MODE=debug or ENC=no
# Do compile encoded source if MODE=debug and ENC=yes
ifeq ($(strip $(ENC)),no)
  ENCODED_SOURCE = sdf_source_info_dummy.o
else
  ENCODED_SOURCE = sdf_source_info.o
  ifneq ($(strip $(ENC)),yes)
    ifeq ($(strip $(MODE)),debug)
      ENCODED_SOURCE = sdf_source_info_dummy.o
    endif
  endif
endif

# utils
ECHO    = echo
RM      = rm
MKDIR   = mkdir

# compiler & archiver
RANLIB = ranlib

FC_INFO := $(shell ${FC} --version 2>/dev/null \
    || ${FC} -V 2>/dev/null | grep '[a-zA-Z]' | head -n 1)

# objectlist file
include Makefile-objs

# target name
LIB = $(LIBDIR)/libeis_common.a

VPATH = $(SRCDIR):$(OBJDIR):$(LIBDIR):$(MODDIR)

# target
all: $(LIB)

# Not real file targets
.PHONY: Makefile all clean Makefile-objs Makefile-deps

.SUFFIXES: .o .f90 .F90

# implicit rules
%.o: %.F90
	$(FC) -c $(FFLAGS) $(MODULEFLAG) -o $(OBJDIR)/$@ $<

$(LIB): $(OBJDIR) $(MODDIR) $(INCDIR) $(LIBDIR) $(OBJS)
	$(RM) -f $@
	$(AR) -crs $@ $(addprefix $(OBJDIR)/,$(OBJS))
	$(RANLIB) $@
	@cp $(MODDIR)/*.mod $(INCDIR)

$(OBJDIR):
	$(MKDIR) -p $(OBJDIR)

$(MODDIR):
	$(MKDIR) -p $(MODDIR)

$(INCDIR):
	$(MKDIR) -p $(INCDIR)

$(LIB): | $(LIBDIR)

$(LIBDIR):
	$(MKDIR) -p $(LIBDIR)

# cleanup
clean:
	-$(RM) -rf $(OBJDIR)
	-$(RM) -rf $(MODDIR)
	-$(RM) $(INCDIR)/*.mod

# dependencies file
include Makefile-deps
