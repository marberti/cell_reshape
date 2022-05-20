module mod_p3d_t

  use mod_parameters

  implicit none
  save
  public

  type :: p3d_t
    real(dbl) :: x
    real(dbl) :: y
    real(dbl) :: z
  end type

end module mod_p3d_t
