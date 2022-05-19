module mod_error

  implicit none
  save
  public

contains

subroutine error(proc_name,proc_msg)

  use, intrinsic :: iso_fortran_env

  character(*), intent(in) :: proc_name
  character(*), intent(in) :: proc_msg

  write(ERROR_UNIT,'(1X,A,": ",A)') proc_name, proc_msg
  stop 1

end subroutine error

end module mod_error
