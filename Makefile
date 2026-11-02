# Not real file targets
.PHONY: all clean demo democlean

.SUFFIXES: .o .f90 .F90

all:
	$(MAKE) -C common
	$(MAKE) -C parser
	$(MAKE) -C deck
demo: all
	$(MAKE) -C demos demo
# remove the built demos
democlean:
	$(MAKE) -C demos democlean
# cleanup
clean:
	$(MAKE) -C common clean
	$(MAKE) -C parser clean
	$(MAKE) -C deck clean
