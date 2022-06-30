program main

  use mod_error
  use mod_p3d_t
  use mod_cell_t
  use mod_read_cell
  use mod_write_cell
  use mod_cell_reshape
  use mod_cell_saturate

  implicit none

  character(*), parameter :: my_name = "main"
  integer :: i
  integer :: argc
  character(120) :: prog_name
  character(120) :: fname
  character(30) :: cmd
  type(cell_t) :: cin
  type(cell_t) :: cresh
  type(cell_t) :: csat
  type(p3d_t) :: cell_center
  logical :: flag_cell_reshape
  logical :: flag_cell_saturate

  flag_cell_reshape  = .false.
  flag_cell_saturate = .false.

  call get_command_argument(0,prog_name)
  argc = command_argument_count()

  if (argc < 1) then
    call help(prog_name)
    stop 1
  end if

  do i = 1, argc
    call get_command_argument(i,cmd)
    select case (cmd)
    case ("-h")
      call help(prog_name)
      stop
    case ("-r")
      flag_cell_reshape = .true.
    case ("-s")
      flag_cell_saturate = .true.
    case default
      if (i == argc) then
        fname = cmd
      else
        call error(my_name,"invalid argument "//trim(cmd))
      end if
    end select
  end do

  call read_cell(trim(fname),cin,cell_center)
  write(*,'(A,3(F9.4))') " Cell center:", cell_center%x, &
    cell_center%y, cell_center%z
  if (flag_cell_reshape) then
    call cell_reshape(cin,cresh,cell_center)
    write(*,*) "Reshaped."
    call write_cell(cresh,"reshaped.xyz")
  end if
  if (flag_cell_saturate) then
    if (flag_cell_reshape) then
      call cell_saturate(cresh,csat)
    else
      call cell_saturate(cin,csat)
    end if
    write(*,*) "Saturated."
    call write_cell(csat,"saturated.xyz")
  end if

contains

subroutine help(prog_name)

  character(*), intent(in) :: prog_name

  write(*,*) "usage: "//trim(prog_name)//" [OPTIONS] <FILE>"
  write(*,*)
  write(*,*) "List of OPTIONS:"
  write(*,*) "  -h      print this help and exit"
  write(*,*) "  -r      reshape the cell (always performed before saturation)"
  write(*,*) "  -s      saturate the cell (always performed after reshaping)"

end subroutine help

end program main
