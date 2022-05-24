module mod_euclidean_distance

  use mod_parameters
  use mod_p3d_t

  implicit none
  save
  private

  public :: compute_distance_3d

contains

real(dbl) function compute_distance_3d(p1,p2)

  type(p3d_t), intent(in) :: p1
  type(p3d_t), intent(in) :: p2

  compute_distance_3d = sqrt((p1%x-p2%x)**2 + &
                             (p1%y-p2%y)**2 + &
                             (p1%z-p2%z)**2)

end function compute_distance_3d

end module mod_euclidean_distance
