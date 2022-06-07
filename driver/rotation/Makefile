.PHONY: default
default: warning compile

.PHONY: warning
warning:
	@echo "Please compile the main program before compiling this driver"

.PHONY: compile
compile:
	gfortran -J../../mod -c main.f90
	gfortran -J../../mod -o driver_rotate.x ../../obj/mod_* main.o

