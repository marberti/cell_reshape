module mod_cell_reshape

  use mod_parameters
  use mod_error
  use mod_p3d_t
  use mod_cell_t

  implicit none
  save
  private

  public :: cell_reshape

contains

subroutine cell_reshape(cin,cout,center)

  type(cell_t), intent(in)  :: cin
  type(cell_t), intent(out) :: cout
  type(p3d_t), intent(in) :: center
  character(*), parameter :: my_name = "cell_reshape"
  integer :: n
  integer :: err_n
  character(120) :: err_msg

  cout%a = cin%a
  cout%b = cin%b
  cout%c = cin%c

  n = cin%xyz%n
  cout%xyz%n = n
  allocate(cout%xyz%e(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  allocate(cout%xyz%x(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  allocate(cout%xyz%y(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  allocate(cout%xyz%z(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  cout%xyz%e = cin%xyz%e
  cout%xyz%x = cin%xyz%x
  cout%xyz%y = cin%xyz%y
  cout%xyz%z = cin%xyz%z

  call reshape_array(cout%xyz%x,n,cout%a,center%x)
  call reshape_array(cout%xyz%y,n,cout%b,center%y)
  call reshape_array(cout%xyz%z,n,cout%c,center%z)

end subroutine cell_reshape

subroutine reshape_array(a,n,d,c)

  real(dbl), dimension(:), intent(inout) :: a
  integer, intent(in) :: n
  real(dbl), intent(in) :: d
  real(dbl), intent(in) :: c
  integer :: i
  real(dbl) :: t ! threshold

  t = d / 2.0_dbl

  do i=1, n
    if (abs(c-a(i)) > t) then
      if (a(i) > c) then
        a(i) = a(i) - d
      else
        a(i) = a(i) + d
      end if
    end if
  end do

end subroutine reshape_array

end module mod_cell_reshape
