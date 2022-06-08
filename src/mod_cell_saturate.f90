module mod_cell_saturate

  use mod_parameters
  use mod_error
  use mod_cell_t
  use mod_periodic_table
  use mod_logical
  use mod_connectivity

  implicit none
  save
  private

  public :: cell_saturate

contains

! Public !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine cell_saturate(cin,cout)

  type(cell_t), intent(in)  :: cin
  type(cell_t), intent(out) :: cout
  character(*), parameter :: my_name = "cell_saturate"
  real(dbl), dimension(:,:), allocatable :: dm
  logical, dimension(:,:), allocatable :: lm
  integer :: i
  integer :: n
  integer :: err_n
  character(120) :: err_msg

  n = cin%xyz%n

  allocate(dm(n,n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  allocate(lm(n,n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  ! Output is not a periodic cell, cell constants are meaningless
  cout%a = 0.0
  cout%b = 0.0
  cout%c = 0.0

  call get_connectivity_matrix(cin%xyz%e,cin%xyz%x,cin%xyz%y,cin%xyz%z,dm,lm)

  do i = 1, n
    call atom_saturate(i,cin,lm)
  end do

  deallocate(dm,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  deallocate(lm,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

end subroutine cell_saturate

! Private !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine atom_saturate(i,cin,lm)

  integer, intent(in) :: i
  type(cell_t), intent(in) :: cin
  logical, dimension(:,:), intent(in) :: lm
  character(*), parameter :: my_name = "atom_saturate"
  integer :: bonds_done
  integer :: bonds_max
  real(dbl) :: dist

  bonds_max = pt_get_max_bonds(cin%xyz%e(i))
  bonds_done = count_true(lm(:,i))

  if (bonds_done >= bonds_max) return

  dist = pt_get_r_mean("H ") + pt_get_r_mean(cin%xyz%e(i))

  select case (bonds_max)
  case (1)
    call error(my_name,"bonds_max == 1 not implemented yet")
  case (2)
    call error(my_name,"bonds_max == 2 not implemented yet")
  case (3)
    call error(my_name,"bonds_max == 3 not implemented yet")
  case (4)
    call saturate_4_tetrahedral(bonds_done,dist)
  case default
    call error(my_name,"bonds_max > 4 not implemented yet")
  end select

  stop 33

end subroutine atom_saturate

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine saturate_1()

  character(*), parameter :: my_name = "saturate_1"
  call error(my_name,"not implemented yet")

end subroutine saturate_1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine saturate_2_linear()

  character(*), parameter :: my_name = "saturate_2_linear"
  call error(my_name,"not implemented yet")

end subroutine saturate_2_linear

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine saturate_2_angular()

  character(*), parameter :: my_name = "saturate_2_angular"
  call error(my_name,"not implemented yet")

end subroutine saturate_2_angular

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine saturate_3_trigonal_planar()

  character(*), parameter :: my_name = "saturate_3_trigonal_planar"
  call error(my_name,"not implemented yet")

end subroutine saturate_3_trigonal_planar

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine saturate_3_trigonal_pyramidal()

  character(*), parameter :: my_name = "saturate_3_trigonal_pyramidal"
  call error(my_name,"not implemented yet")

end subroutine saturate_3_trigonal_pyramidal

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine saturate_4_tetrahedral(bonds_done,d)

  integer, intent(in) :: bonds_done
  real(dbl), intent(in) :: d
  character(*), parameter :: my_name = "saturate_tetrahedral"
  real(dbl) :: theta
  real(dbl) :: phi

  select case (bonds_done)
  case (0)
    theta = 0.0_dbl
    phi   = 0.0_dbl
    print *, "H", d*cos(theta), d*sin(theta)*sin(phi), d*sin(theta)*cos(phi)
    theta = 1.9106332356470423_dbl
    phi   = 0.0_dbl
    print *, "H", d*cos(theta), d*sin(theta)*sin(phi), d*sin(theta)*cos(phi)
    theta = 1.9106332356470423_dbl
    phi   = 2.0943951023931953_dbl
    print *, "H", d*cos(theta), d*sin(theta)*sin(phi), d*sin(theta)*cos(phi)
    theta = 1.9106332356470423_dbl
    phi   = 4.1887902047863905_dbl
    print *, "H", d*cos(theta), d*sin(theta)*sin(phi), d*sin(theta)*cos(phi)
  case (1)
    call error(my_name,"bonds_done == 1 not implemented yet")
  case (2)
    call error(my_name,"bonds_done == 2 not implemented yet")
  case (3)
    call error(my_name,"bonds_done == 3 not implemented yet")
  end select

end subroutine saturate_4_tetrahedral

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mod_cell_saturate
