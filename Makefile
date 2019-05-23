# Not real file targets
.PHONY: all clean

.SUFFIXES: .o .f90 .F90

all:
	$(MAKE) -C common
	$(MAKE) -C parser
# cleanup
clean:
	$(MAKE) -C common clean
	$(MAKE) -C parser clean
