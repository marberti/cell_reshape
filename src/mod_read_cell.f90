module mod_read_cell

  use mod_parameters
  use mod_error
  use mod_p3d_t
  use mod_cell_t
  use mod_get_field

  implicit none
  save
  private

  public :: read_cell

contains

subroutine read_cell(fname,cell,center)

  character(*), intent(in) :: fname
  type(cell_t), intent(out) :: cell
  type(p3d_t), intent(out) :: center
  character(*), parameter :: my_name = "read_cell"
  integer, parameter :: fnumb = 200
  character(200) :: buff
  character(100) :: field
  character(8) :: i_str
  integer :: i
  integer :: n
  logical :: flag_found_center
  integer :: err_n
  character(120) :: err_msg

  flag_found_center = .false.

  open(unit=fnumb,file=fname,status="old",action="read",&
    iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  read(fnumb,*,iostat=err_n,iomsg=err_msg) n
  if (err_n /= 0) call error(my_name,err_msg)
  read(fnumb,*,iostat=err_n,iomsg=err_msg) cell%a, cell%b, cell%c
  if (err_n /= 0) call error(my_name,err_msg)

  cell%xyz%n = n
  allocate(cell%xyz%e(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  allocate(cell%xyz%x(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  allocate(cell%xyz%y(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  allocate(cell%xyz%z(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  do i=1, n
    read(fnumb,"(A200)",iostat=err_n,iomsg=err_msg) buff
    if (err_n /= 0) call error(my_name,err_msg)
    call get_field(buff,field,1,err_n,err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
    read(field,"(A2)") cell%xyz%e(i)
    call get_field(buff,field,2,err_n,err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
    read(field,*) cell%xyz%x(i)
    call get_field(buff,field,3,err_n,err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
    read(field,*) cell%xyz%y(i)
    call get_field(buff,field,4,err_n,err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
    read(field,*) cell%xyz%z(i)
    call get_field(buff,field,5,err_n,err_msg)
    if (err_n == 0) then
      if (trim(field) == "*") then
        if (flag_found_center) call error(my_name,&
          "specified more than one center in the input geometry")
        center%x = cell%xyz%x(i)
        center%y = cell%xyz%y(i)
        center%z = cell%xyz%z(i)
        flag_found_center = .true.
      else
        write(i_str,"(I8)") i+2
        i_str = adjustl(i_str)
        call error(my_name,"unexpected input on line "//trim(i_str))
      end if
    end if
  end do

  close(unit=fnumb,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  if (flag_found_center.eqv..false.) call error(my_name,&
    "no center was specified in the input geometry")

end subroutine read_cell

end module mod_read_cell
