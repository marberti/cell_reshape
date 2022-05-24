module mod_write_cell

  use mod_error
  use mod_cell_t

  implicit none
  save
  private

  public :: write_cell

contains

subroutine write_cell(cell,fname)

  use, intrinsic :: iso_fortran_env

  type(cell_t), intent(in) :: cell
  character(*), optional, intent(in) :: fname
  character(*), parameter :: my_name = "write_cell"
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

  write(fnumb,*) cell%xyz%n
  write(fnumb,*)
  do i=1, cell%xyz%n
    write(fnumb,fmt0) cell%xyz%e(i),cell%xyz%x(i),cell%xyz%y(i),cell%xyz%z(i)
  end do

  if (present(fname)) then
    close(unit=fnumb,iostat=err_n,iomsg=err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
  end if

end subroutine write_cell

end module mod_write_cell
