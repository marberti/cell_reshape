module mod_rotation

  use mod_parameters
  use mod_error

  implicit none
  save
  private

  public :: rot_matrix
  public :: rotate3d

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine rot_matrix(axis,a,m)

  integer, intent(in) :: axis
  real(dbl), intent(in) :: a ! angle
  real(dbl), dimension(3,3), intent(out) :: m
  character(*), parameter :: my_name = "rot_matrix"

  select case (axis)
  case (1)
    m(1,1) = 1.0_dbl
    m(2,1) = 0.0_dbl
    m(3,1) = 0.0_dbl
    m(1,2) = 0.0_dbl
    m(2,2) = cos(a)
    m(3,2) = sin(a)
    m(1,3) = 0.0_dbl
    m(2,3) = -sin(a)
    m(3,3) = cos(a)
  case (2)
    m(1,1) = cos(a)
    m(2,1) = 0.0_dbl
    m(3,1) = -sin(a)
    m(1,2) = 0.0_dbl
    m(2,2) = 1.0_dbl
    m(3,2) = 0.0_dbl
    m(1,3) = sin(a)
    m(2,3) = 0.0_dbl
    m(3,3) = cos(a)
  case (3)
    m(1,1) = cos(a)
    m(2,1) = sin(a)
    m(3,1) = 0.0_dbl
    m(1,2) = -sin(a)
    m(2,2) = cos(a)
    m(3,2) = 0.0_dbl
    m(1,3) = 0.0_dbl
    m(2,3) = 0.0_dbl
    m(3,3) = 1.0_dbl
  case default
    call error(my_name,"argument axis out of range")
  end select

end subroutine rot_matrix

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine rotate3d(p_in,p_out,axis,a)

  real(dbl), dimension(3), intent(in) :: p_in
  real(dbl), dimension(3), intent(out) :: p_out
  integer, intent(in) :: axis
  real(dbl), intent(in) :: a ! angle
  character(*), parameter :: my_name = "rotate3d"
  real(dbl), dimension(3,3) :: m

  if ((axis < 1).or.(axis > 3)) then
    call error(my_name,"argument axis out of range")
  end if

  call rot_matrix(axis,a,m)
  p_out = matmul(m,p_in)

end subroutine rotate3d

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mod_rotation
