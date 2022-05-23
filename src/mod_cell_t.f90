module mod_cell_t

  use mod_parameters
  use mod_xyz_t

  implicit none
  save
  public

  type :: cell_t
    real(dbl) :: a
    real(dbl) :: b
    real(dbl) :: c
    type(xyz_t) :: xyz
  end type

end module mod_cell_t
