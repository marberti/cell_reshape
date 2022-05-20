module mod_cell_reshape

  use mod_parameters
  use mod_error
  use mod_p3d_t
  use mod_xyz_t

  implicit none
  save
  private

  public :: cell_reshape

contains

subroutine cell_reshape(cin,cout,center)

  type(xyz_t), intent(in)  :: cin
  type(xyz_t), intent(out) :: cout
  type(p3d_t), intent(in)  :: center
  character(*), parameter :: my_name = "cell_reshape"
  integer :: n
  real(dbl) :: x_min
  real(dbl) :: x_max
  real(dbl) :: y_min
  real(dbl) :: y_max
  real(dbl) :: z_min
  real(dbl) :: z_max
  integer :: err_n
  character(120) :: err_msg

  n = cin%n
  cout%n = n
  allocate(cout%e(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  allocate(cout%x(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  allocate(cout%y(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  allocate(cout%z(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  x_min = minval(cin%x)
  x_max = maxval(cin%x)
  y_min = minval(cin%y)
  y_max = maxval(cin%y)
  z_min = minval(cin%z)
  z_max = maxval(cin%z)

  write(*,*) x_min, x_max
  write(*,*) y_min, y_max
  write(*,*) z_min, z_max

end subroutine cell_reshape

end module mod_cell_reshape
