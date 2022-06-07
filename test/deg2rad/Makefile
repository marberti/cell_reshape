.PHONY: default
default: warning compile

.PHONY: warning
warning:
	@echo "Please compile the main program before compiling this test"

.PHONY: compile
compile:
	gfortran -J../../mod -c main.f90
	gfortran -J../../mod -o test_deg2rad.x ../../obj/mod_* main.o

