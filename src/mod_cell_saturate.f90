module mod_cell_saturate

  use mod_parameters
  use mod_error
  use mod_xyz_t
  use mod_cell_t

  implicit none
  save
  private

  public :: cell_saturate

contains

subroutine cell_saturate(cin,gout)

  type(cell_t), intent(in) :: cin
  type(xyz_t), intent(out) :: gout
  character(*), parameter :: my_name = "cell_saturate"

end subroutine cell_saturate

end module mod_cell_saturate
