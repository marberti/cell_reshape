module mod_cell_saturate

  use mod_parameters
  use mod_error
  use mod_cell_t
  use mod_connectivity

  implicit none
  save
  private

  public :: cell_saturate

contains

subroutine cell_saturate(cin,cout)

  type(cell_t), intent(in)  :: cin
  type(cell_t), intent(out) :: cout
  character(*), parameter :: my_name = "cell_saturate"
  real(dbl), dimension(:,:), allocatable :: dm
  logical, dimension(:,:), allocatable :: lm
  integer :: n
  integer :: err_n
  character(120) :: err_msg

  n = cin%xyz%n

  allocate(dm(n,n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  allocate(lm(n,n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  ! Output is not a periodic cell, cell constants are meaningless
  cout%a = 0.0
  cout%b = 0.0
  cout%c = 0.0

  call get_connectivity_matrix(cin%xyz%e,cin%xyz%x,cin%xyz%y,cin%xyz%z,dm,lm)

  deallocate(dm,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  deallocate(lm,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

end subroutine cell_saturate

end module mod_cell_saturate
