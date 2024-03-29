FC       = gfortran
FLAGS    = -g -cpp -O2 -Wall -Wunused -Wpedantic -Wno-maybe-uninitialized -std=f2008
LIBS     = -L/usr/lib/x86_64-linux-gnu -lblas -llapack

SRCDIR = src
OBJDIR = obj
MODDIR = mod

SOURCES = mod_parameters.f90              \
          mod_p3d_t.f90                   \
          mod_xyz_t.f90                   \
          mod_cell_t.f90                  \
          mod_error.f90                   \
          mod_logical.f90                 \
          mod_rotation.f90                \
          mod_periodic_table.f90          \
          mod_euclidean_distance.f90      \
          mod_get_field.f90               \
          mod_van_der_waals.f90           \
          mod_connectivity.f90            \
          mod_read_cell.f90               \
          mod_write_cell.f90              \
          mod_cell_check_distances.f90    \
          mod_cell_reshape.f90            \
          mod_cell_saturate.f90           \
          main.f90

SRC = $(addprefix $(SRCDIR)/, $(SOURCES))
OBJ = $(addprefix $(OBJDIR)/, $(SOURCES:%.f90=%.o))
EXE = cell_reshape.x

# main compilation options --------------------------------
.PHONY: default
default: $(EXE)

.PHONY: fresh
fresh: clean $(EXE)

.PHONY: debug
debug: FLAGS += -DDEBUG
debug: fresh

# utility -------------------------------------------------
.PHONY: clean
clean:
	@printf "Cleaning..."
	@rm -f $(OBJDIR)/*.o $(MODDIR)/*.mod
	@printf " DONE\n"

# core ----------------------------------------------------
$(EXE): $(OBJ)
	$(FC) $(FLAGS) -J$(MODDIR) -o $(EXE) $(OBJ) $(LIBS)

$(OBJ): | $(OBJDIR)
$(OBJ): | $(MODDIR)
$(OBJ): $(OBJDIR)/%.o: $(SRCDIR)/%.f90
	$(FC) $(FLAGS) -J$(MODDIR) -c -o $@ $<

$(OBJDIR):
	mkdir -p $(OBJDIR)

$(MODDIR):
	mkdir -p $(MODDIR)

