# Cell Reshape

The program takes a _.xyz_ file as input, containing the position of the
atomic nuclei within a cell. The cell is then reshaped in order to center it
on the desired atomic nucleus, while maintaining its translational symmetry.
Finally, it is possible to optionally saturate the outermost atoms with
hydrogens, which is useful if you want to use the output geometry in a
calculation without periodic boundary conditions.
