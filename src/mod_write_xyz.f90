module mod_write_xyz

  use mod_error
  use mod_xyz_t

  implicit none
  save
  private

  public :: write_xyz

contains

subroutine write_xyz(xyz,fname)

  use, intrinsic :: iso_fortran_env

  type(xyz_t), intent(in) :: xyz
  character(*), optional, intent(in) :: fname
  character(*), parameter :: my_name = "write_xyz"
  character(*), parameter :: fmt0 = "(1X,A2,3(1X,F11.6))"
  integer, parameter :: fnumb_par = 405
  integer :: fnumb
  integer :: i
  integer :: err_n
  character(120) :: err_msg

  if (present(fname)) then
    fnumb = fnumb_par
    open(unit=fnumb,file=fname,status="replace",action="write",&
      iostat=err_n,iomsg=err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
  else
    fnumb = OUTPUT_UNIT
  end if

  write(fnumb,*) xyz%n
  write(fnumb,*)
  do i=1, xyz%n
    write(fnumb,fmt0) xyz%e(i), xyz%x(i), xyz%y(i), xyz%z(i)
  end do

  if (present(fname)) then
    close(unit=fnumb,iostat=err_n,iomsg=err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
  end if

end subroutine write_xyz

end module mod_write_xyz
