module mod_logical

  implicit none
  save
  private

  public :: count_true
  public :: count_false

contains

integer function count_true(a)

  logical, dimension(:), intent(in) :: a
  integer :: i

  count_true = 0
  do i = 1, size(a)
    if (a(i).eqv..true.) count_true = count_true + 1
  end do

end function count_true

integer function count_false(a)

  logical, dimension(:), intent(in) :: a
  integer :: i

  count_false = 0
  do i = 1, size(a)
    if (a(i).eqv..false.) count_false = count_false + 1
  end do

end function count_false

end module mod_logical
