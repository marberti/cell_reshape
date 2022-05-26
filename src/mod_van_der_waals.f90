module mod_van_der_waals

  use mod_parameters
  use mod_periodic_table

  implicit none
  save
  private

  public :: check_vdw_bond

contains

logical function check_vdw_bond(e1,e2,d)

  character(2), intent(in) :: e1
  character(2), intent(in) :: e2
  real(dbl), intent(in) :: d
  real(dbl) :: r_e1
  real(dbl) :: r_e2

  r_e1 = pt_get_r_max(e1)
  r_e2 = pt_get_r_max(e2)

  if (d <= r_e1 + r_e2) then
    check_vdw_bond = .true.
  else
    check_vdw_bond = .false.
  end if

end function check_vdw_bond

end module mod_van_der_waals
