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

  cout%e = cin%e
  cout%x = cin%x
  cout%y = cin%y
  cout%z = cin%z

  call reshape_array(cout%x,n,center%x)
  call reshape_array(cout%y,n,center%y)
  call reshape_array(cout%z,n,center%z)

end subroutine cell_reshape

subroutine reshape_array(a,n,c)

  real(dbl), dimension(:), intent(inout) :: a
  integer, intent(in) :: n
  real(dbl), intent(in) :: c
  integer :: i
  real(dbl) :: a_min
  real(dbl) :: a_max
  real(dbl) :: d
  real(dbl) :: t ! threshold
  real(dbl) :: f ! fuzziness

  f = 0.0_dbl

  a_min = minval(a)
  a_max = maxval(a)
  d = a_max - a_min
  t = d / 2.0_dbl

  do i=1, n
    if (abs(c-a(i)) + f > t) then
      if (a(i) > c) then
        a(i) = a(i) - d
      else
        a(i) = a(i) + d
      end if
    end if
  end do

end subroutine reshape_array

end module mod_cell_reshape
