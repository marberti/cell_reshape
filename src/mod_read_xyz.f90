module mod_read_xyz

  use mod_parameters
  use mod_error
  use mod_xyz_t
  use mod_get_field

  implicit none
  save
  private

  public :: read_xyz

contains

subroutine read_xyz(fname,xyz)

  character(*), intent(in) :: fname
  type(xyz_t), intent(out) :: xyz
  character(*), parameter :: my_name = "read_xyz"
  integer, parameter :: fnumb = 200
  character(200) :: buff
  character(100) :: field
  integer :: i
  integer :: n
  integer :: err_n
  character(120) :: err_msg

  open(unit=fnumb,file=fname,status="old",action="read",&
    iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  read(fnumb,*,iostat=err_n,iomsg=err_msg) n
  if (err_n /= 0) call error(my_name,err_msg)
  read(fnumb,*,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  xyz%n = n
  allocate(xyz%e(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  allocate(xyz%x(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  allocate(xyz%y(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  allocate(xyz%z(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  do i=1, n
    read(fnumb,"(A200)",iostat=err_n,iomsg=err_msg) buff
    if (err_n /= 0) call error(my_name,err_msg)
    call get_field(buff,field,1,err_n,err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
    read(field,"(A2)") xyz%e(i)
    call get_field(buff,field,2,err_n,err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
    read(field,*) xyz%x(i)
    call get_field(buff,field,3,err_n,err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
    read(field,*) xyz%y(i)
    call get_field(buff,field,4,err_n,err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
    read(field,*) xyz%z(i)
  end do

  close(unit=fnumb,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

end subroutine read_xyz

end module mod_read_xyz
