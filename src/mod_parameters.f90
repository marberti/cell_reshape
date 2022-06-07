module mod_parameters

  use, intrinsic :: iso_fortran_env

  implicit none
  save
  public

  integer, parameter :: dbl = REAL64
  real(dbl), parameter :: pi = 3.14159265358979323846_dbl

end module mod_parameters
