module mod_cell_reshape

  implicit none
  save
  private

  public :: cell_reshape

contains

subroutine cell_reshape(cell_in,cell_out)

  integer, intent(in)  :: cell_in
  integer, intent(out) :: cell_out

end subroutine cell_reshape

end module mod_cell_reshape
