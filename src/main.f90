program main

  use mod_error

  implicit none

  character(*), parameter :: my_name = "main"
  character(120) :: prog_name

  call get_command_argument(0,prog_name)

  if (command_argument_count() /= 1) then
    call help(prog_name)
    stop 1
  end if

!  call read_xyz()
!  call cell_reshape()
!  call cell_saturate()

contains

subroutine help(prog_name)

  character(*), intent(in) :: prog_name

  write(*,*) "usage: "//trim(prog_name)//" <file.xyz>"

end subroutine help

end program main
