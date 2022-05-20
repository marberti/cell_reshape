program main

  use mod_error
  use mod_p3d_t
  use mod_xyz_t
  use mod_read_xyz
  use mod_write_xyz
  use mod_cell_reshape

  implicit none

  character(*), parameter :: my_name = "main"
  character(120) :: prog_name
  character(120) :: fname
  type(xyz_t) :: xyz_input
  type(xyz_t) :: xyz_output
  type(p3d_t) :: cell_center

  call get_command_argument(0,prog_name)

  if (command_argument_count() /= 1) then
    call help(prog_name)
    stop 1
  end if

  call get_command_argument(1,fname)

  call read_xyz(trim(fname),xyz_input,cell_center)
  write(*,*) "Input:"
  call write_xyz(xyz_input)
  write(*,*) "Cell center:"
  write(*,*) cell_center%x, cell_center%y, cell_center%z
  call cell_reshape(xyz_input,xyz_output,cell_center)
!  call cell_saturate()

contains

subroutine help(prog_name)

  character(*), intent(in) :: prog_name

  write(*,*) "usage: "//trim(prog_name)//" <file.xyz>"

end subroutine help

end program main
