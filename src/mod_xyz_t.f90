module mod_xyz_t

  use mod_parameters

  implicit none
  save
  public

  type :: xyz_t
    integer :: n
    character(2), allocatable, dimension(:) :: e
    real(dbl), allocatable, dimension(:) :: x
    real(dbl), allocatable, dimension(:) :: y
    real(dbl), allocatable, dimension(:) :: z
  end type

end module mod_xyz_t
