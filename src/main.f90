program main

  use mod_error
  use mod_xyz_t
  use mod_read_xyz

  implicit none

  character(*), parameter :: my_name = "main"
  character(120) :: prog_name
  character(120) :: fname
  type(xyz_t) :: xyz_input
  type(xyz_t) :: xyz_output

  call get_command_argument(0,prog_name)

  if (command_argument_count() /= 1) then
    call help(prog_name)
    stop 1
  end if

  call get_command_argument(1,fname)

  call read_xyz(trim(fname),xyz_input)
!  call cell_reshape()
!  call cell_saturate()

contains

subroutine help(prog_name)

  character(*), intent(in) :: prog_name

  write(*,*) "usage: "//trim(prog_name)//" <file.xyz>"

end subroutine help

end program main
