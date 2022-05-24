program main

  use mod_error
  use mod_p3d_t
  use mod_cell_t
  use mod_read_cell
  use mod_write_cell
  use mod_cell_reshape

  implicit none

  character(*), parameter :: my_name = "main"
  character(120) :: prog_name
  character(120) :: fname
  type(cell_t) :: cell_input
  type(cell_t) :: cell_output
  type(p3d_t) :: cell_center

  call get_command_argument(0,prog_name)

  if (command_argument_count() /= 1) then
    call help(prog_name)
    stop 1
  end if

  call get_command_argument(1,fname)

  call read_cell(trim(fname),cell_input,cell_center)
  write(*,*) "Input:"
  call write_cell(cell_input)
  write(*,*) "Cell center:"
  write(*,*) cell_center%x, cell_center%y, cell_center%z
  call cell_reshape(cell_input,cell_output,cell_center)
  write(*,*) "Reshaped."
  call write_cell(cell_output,"reshaped.xyz")
!  call cell_saturate()

contains

subroutine help(prog_name)

  character(*), intent(in) :: prog_name

  write(*,*) "usage: "//trim(prog_name)//" <file.xyz>"

end subroutine help

end program main
