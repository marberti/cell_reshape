module mod_connectivity

  use mod_parameters
  use mod_error
  use mod_p3d_t
  use mod_euclidean_distance
  use mod_van_der_waals

  implicit none
  save
  private

  public :: get_connectivity_matrix

contains

subroutine get_connectivity_matrix(e,x,y,z,dm,lm)

  character(*), dimension(:), intent(in) :: e
  real(dbl), dimension(:), intent(in) :: x
  real(dbl), dimension(:), intent(in) :: y
  real(dbl), dimension(:), intent(in) :: z
  real(dbl), dimension(:,:), intent(out) :: dm
  logical, dimension(:,:), intent(out) :: lm
  character(*), parameter :: my_name = "get_connectivity_matrix"
  integer :: n
  integer :: i
  integer :: j
  type(p3d_t) :: p1
  type(p3d_t) :: p2
  real(dbl) :: distance
  logical :: connection

  n = size(e)

  if (size(x) /= n) call error(my_name,"mismatching input vectors sizes")
  if (size(y) /= n) call error(my_name,"mismatching input vectors sizes")
  if (size(z) /= n) call error(my_name,"mismatching input vectors sizes")
  if ((size(dm,1) /= n).or.(size(dm,2) /= n)) then
    call error(my_name,"mismatching dm matrix sizes")
  end if
  if ((size(lm,1) /= n).or.(size(lm,2) /= n)) then
    call error(my_name,"mismatching lm matrix sizes")
  end if

  dm = 0.0_dbl
  lm = .false.

  do i = 1, n
    do j = i+1, n
      p1%x = x(i)
      p1%y = y(i)
      p1%z = z(i)
      p2%x = x(j)
      p2%y = y(j)
      p2%z = z(j)
      distance = compute_distance_3d(p1,p2)
      connection = check_vdw_bond(e(i),e(j),distance)
      dm(i,j) = distance
      dm(j,i) = distance
      lm(i,j) = connection
      lm(j,i) = connection
    end do
  end do

end subroutine get_connectivity_matrix

end module mod_connectivity
