module mod_cell_saturate

  use mod_parameters
  use mod_error
  use mod_cell_t

  implicit none
  save
  private

  public :: cell_saturate

contains

subroutine cell_saturate(cin,cout)

  type(cell_t), intent(in)  :: cin
  type(cell_t), intent(out) :: cout
  character(*), parameter :: my_name = "cell_saturate"

  ! Output is not a periodic cell, cell constants are meaningless
  cout%a = 0.0
  cout%b = 0.0
  cout%c = 0.0

end subroutine cell_saturate

end module mod_cell_saturate
