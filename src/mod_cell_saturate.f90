module mod_cell_saturate

  use mod_parameters
  use mod_error
  use mod_cell_t
  use mod_periodic_table
  use mod_logical
  use mod_connectivity
  use mod_rotation

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
  integer, dimension(:), allocatable :: saturation_hydrogens
  integer :: i
  integer :: j
  integer :: indx
  integer :: n
  integer :: an
  real(dbl), dimension(:,:), allocatable :: atoms
  real(dbl), dimension(:,:), allocatable :: h_pos
  real(dbl), dimension(3) :: translation
  real(dbl) :: alp
  real(dbl) :: bet
  real(dbl) :: gam
  integer :: err_n
  character(120) :: err_msg

  n = cin%xyz%n

  allocate(dm(n,n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  allocate(lm(n,n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  allocate(saturation_hydrogens(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  ! Output is not a periodic cell, cell constants are meaningless
  cout%a = 0.0
  cout%b = 0.0
  cout%c = 0.0

  call get_connectivity_matrix(cin%xyz%e,cin%xyz%x,cin%xyz%y,cin%xyz%z,dm,lm)
  call count_saturation_hydrogens(cin%xyz%e,lm,saturation_hydrogens)

  do i = 1, n
    if (saturation_hydrogens(i) == 0) cycle

    ! allocate h_pos
    allocate(h_pos(3,saturation_hydrogens(i)),stat=err_n,errmsg=err_msg)
    if (err_n /= 0) call error(my_name,err_msg)

    ! select atom to saturate and its neighbors
    an = count_true(lm(:,i)) + 1
    allocate(atoms(3,an),stat=err_n,errmsg=err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
    atoms(1,1) = cin%xyz%x(i)
    atoms(2,1) = cin%xyz%y(i)
    atoms(3,1) = cin%xyz%z(i)
    indx = 2
    do j = 1, n
      if (indx > an) exit
      if (lm(j,i).eqv..true.) then
        atoms(1,indx) = cin%xyz%x(j)
        atoms(2,indx) = cin%xyz%y(j)
        atoms(3,indx) = cin%xyz%z(j)
        indx = indx + 1
      end if
    end do

#ifdef DEBUG
    write(*,*) "DEBUG: unsatured atom: ",i," (H to add: ",&
      saturation_hydrogens(i),")"
    write(*,*) "DEBUG: standard orientation"
    do j = 1, an
      write(*,*) "DEBUG: ",atoms(1,j),atoms(2,j),atoms(3,j)
    end do
#endif

    ! get translation vector
    translation(1) = atoms(1,1)
    translation(2) = atoms(2,1)
    translation(3) = atoms(3,1)

    ! translate atoms (in order to get rotation angles)
    atoms(1,:) = atoms(1,:) - translation(1)
    atoms(2,:) = atoms(2,:) - translation(2)
    atoms(3,:) = atoms(3,:) - translation(3)

#ifdef DEBUG
    write(*,*) "DEBUG: after translation"
    do j = 1, an
      write(*,*) "DEBUG: ",atoms(1,j),atoms(2,j),atoms(3,j)
    end do
#endif

    ! get rotation angles
    call get_rotation_angles(atoms,alp,bet,gam)

    ! get saturation hydrogens
    call atom_saturate(cin%xyz%e(i),h_pos)

    ! rotate and translate to get the correct hydrogens' positions
    call rotate_hydrogens(h_pos,alp,bet,gam)
    h_pos(1,:) = h_pos(1,:) + translation(1)
    h_pos(2,:) = h_pos(2,:) + translation(2)
    h_pos(3,:) = h_pos(3,:) + translation(3)

    ! add hydrogens' coordinates to the appropriate structure
    !@@@
    do j = 1, size(h_pos,2)
      write(*,'("H",3(3X,F10.6))') h_pos(1,j), h_pos(2,j), h_pos(3,j)
    end do
    !@@@

    ! deallocate
    deallocate(h_pos,stat=err_n,errmsg=err_msg)
    if (err_n /= 0) call error(my_name,err_msg)

    deallocate(atoms,stat=err_n,errmsg=err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
  end do

  deallocate(dm,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  deallocate(lm,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  deallocate(saturation_hydrogens,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

end subroutine cell_saturate

! Private !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine count_saturation_hydrogens(e,lm,saturation_hydrogens)

  character(2), dimension(:), intent(in) :: e
  logical, dimension(:,:), intent(in) :: lm
  integer, dimension(:), intent(out) :: saturation_hydrogens
  integer :: n
  integer :: i
  integer :: bonds_done
  integer :: bonds_max
  integer :: bonds_to_add

  saturation_hydrogens = 0
  n = size(e)
  do i = 1, n
    bonds_max = pt_get_max_bonds(e(i))
    bonds_done = count_true(lm(:,i))
    bonds_to_add = bonds_max - bonds_done
    if (bonds_to_add > 0) saturation_hydrogens(i) = bonds_to_add
  end do

end subroutine count_saturation_hydrogens

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine get_rotation_angles(atoms,alp,bet,gam)

  real(dbl), dimension(:,:), intent(in) :: atoms
  real(dbl), intent(out) :: alp
  real(dbl), intent(out) :: bet
  real(dbl), intent(out) :: gam
  character(*), parameter :: my_name = "get_rotation_angles"
  integer :: an
  integer :: i
  real(dbl), dimension(:,:), allocatable :: atoms_in
  real(dbl), dimension(:,:), allocatable :: atoms_out
  integer :: err_n
  character(120) :: err_msg

  an = size(atoms,2)

  allocate(atoms_in(3,an),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  allocate(atoms_out(3,an),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  atoms_in = atoms

#ifdef DEBUG
  write(*,*) "DEBUG: ",my_name, " before rotation"
  do i = 1, an
    write(*,'(A,3(F10.6,3X))') " DEBUG: ",&
      atoms_in(1,i),atoms_in(2,i),atoms_in(3,i)
  end do
#endif

  ! set alp and bet
  if (an > 1) then
    ! set alp
    alp = get_angle(atoms_in(2,2),atoms_in(1,2))
    do i = 1, an
      call rotate3d(atoms_in(:,i),atoms_out(:,i),3,-alp)
    end do
    atoms_in = atoms_out

#ifdef DEBUG
    write(*,*) "DEBUG: ",my_name, " after z rotation"
    do i = 1, an
      write(*,'(A,3(F10.6,3X))') " DEBUG: ",&
        atoms_in(1,i),atoms_in(2,i),atoms_in(3,i)
    end do
#endif

    ! set bet
    bet = get_angle(atoms_in(3,2),atoms_in(1,2))
    do i = 1, an
      call rotate3d(atoms_in(:,i),atoms_out(:,i),2,bet)
    end do
    atoms_in = atoms_out

#ifdef DEBUG
    write(*,*) "DEBUG: ",my_name, " after y rotation"
    do i = 1, an
      write(*,'(A,3(F10.6,3X))') " DEBUG: ",&
        atoms_in(1,i),atoms_in(2,i),atoms_in(3,i)
    end do
#endif
  else
    alp = 0.0_dbl
    bet = 0.0_dbl
  end if

  ! set gam
  if (an > 2) then
    gam = get_angle(atoms_in(2,3),atoms_in(3,3))
    do i = 1, an
      call rotate3d(atoms_in(:,i),atoms_out(:,i),1,gam)
    end do
    atoms_in = atoms_out

#ifdef DEBUG
    write(*,*) "DEBUG: ",my_name, " after x rotation"
    do i = 1, an
      write(*,'(A,3(F10.6,3X))') " DEBUG: ",&
        atoms_in(1,i),atoms_in(2,i),atoms_in(3,i)
    end do
#endif
  else
    gam = 0.0_dbl
  end if

  ! check gam
  if ((an > 3).and.(atoms_in(2,4) < 0.0_dbl)) then
    ! rotate back
    do i = 1, an
      call rotate3d(atoms_in(:,i),atoms_out(:,i),1,-gam)
    end do
    atoms_in = atoms_out

    ! set new gam
    gam = get_angle(atoms_in(2,4),atoms_in(3,4))
    do i = 1, an
      call rotate3d(atoms_in(:,i),atoms_out(:,i),1,gam)
    end do
    atoms_in = atoms_out

#ifdef DEBUG
    write(*,*) "DEBUG: ",my_name, " after corrected x rotation"
    do i = 1, an
      write(*,'(A,3(F10.6,3X))') " DEBUG: ",&
        atoms_in(1,i),atoms_in(2,i),atoms_in(3,i)
    end do
#endif
  end if

#ifdef DEBUG
  write(*,'(A,A,A,3(3X,F9.6))') " DEBUG: ",&
    my_name," angles -> ",alp, bet, gam
#endif

  deallocate(atoms_in,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  deallocate(atoms_out,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

end subroutine get_rotation_angles

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(dbl) function get_angle(num,den)

  real(dbl), intent(in) :: num
  real(dbl), intent(in) :: den

  if (abs(den) < 1.0E-16_dbl) then
    if (abs(num) < 1.0E-16_dbl) then
      get_angle = 0.0_dbl
    else
      if (num > 0.0_dbl) then
        get_angle = pi/2.0_dbl
      else if (num < 0.0_dbl) then
        get_angle = 3.0_dbl*pi/2.0_dbl
      end if
    end if
  else
    get_angle = atan(num/den)
    if (den < 0.0_dbl) then
      get_angle = get_angle + pi
    end if
    if (get_angle < 0.0_dbl) then
      get_angle = get_angle + 2.0_dbl*pi
    end if
  end if

end function get_angle

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine atom_saturate(e,h_pos)

  character(2), intent(in) :: e
  real(dbl), dimension(:,:), intent(out) :: h_pos
  character(*), parameter :: my_name = "atom_saturate"
  integer :: bonds_max
  real(dbl) :: dist

  bonds_max = pt_get_max_bonds(e)
  dist = pt_get_r_mean("H ") + pt_get_r_mean(e)

  select case (bonds_max)
  case (1)
    call error(my_name,"bonds_max == 1 not implemented yet")
  case (2)
    call error(my_name,"bonds_max == 2 not implemented yet")
  case (3)
    call error(my_name,"bonds_max == 3 not implemented yet")
  case (4)
    call saturate_4_tetrahedral(dist,h_pos)
  case default
    call error(my_name,"bonds_max > 4 not implemented yet")
  end select

end subroutine atom_saturate

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine rotate_hydrogens(h_pos,alp,bet,gam)

  real(dbl), dimension(:,:), intent(inout) :: h_pos
  real(dbl), intent(in) :: alp
  real(dbl), intent(in) :: bet
  real(dbl), intent(in) :: gam
  real(dbl), dimension(3) :: p_rot
  integer :: i

  do i = 1, size(h_pos,2)
      call rotate3d(h_pos(:,i),p_rot,1,-gam)
      h_pos(:,i) = p_rot
      call rotate3d(h_pos(:,i),p_rot,2,-bet)
      h_pos(:,i) = p_rot
      call rotate3d(h_pos(:,i),p_rot,3,alp)
      h_pos(:,i) = p_rot
  end do

end subroutine rotate_hydrogens

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

subroutine saturate_4_tetrahedral(d,h_pos)

  real(dbl), intent(in) :: d
  real(dbl), dimension(:,:), intent(out) :: h_pos
  integer :: h_n
  real(dbl) :: theta
  real(dbl) :: phi

  h_n = size(h_pos,2)

  if (h_n >= 1) then
    theta = 1.9106332356470423_dbl
    phi   = 4.1887902047863905_dbl
    h_pos(1,1) = d*cos(theta)
    h_pos(2,1) = d*sin(theta)*sin(phi)
    h_pos(3,1) = d*sin(theta)*cos(phi)
  end if

  if (h_n >= 2) then
    theta = 1.9106332356470423_dbl
    phi   = 2.0943951023931953_dbl
    h_pos(1,2) = d*cos(theta)
    h_pos(2,2) = d*sin(theta)*sin(phi)
    h_pos(3,2) = d*sin(theta)*cos(phi)
  end if

  if (h_n >= 3) then
    theta = 1.9106332356470423_dbl
    phi   = 0.0_dbl
    h_pos(1,3) = d*cos(theta)
    h_pos(2,3) = d*sin(theta)*sin(phi)
    h_pos(3,3) = d*sin(theta)*cos(phi)
  end if

  if (h_n >= 4) then
    theta = 0.0_dbl
    phi   = 0.0_dbl
    h_pos(1,4) = d*cos(theta)
    h_pos(2,4) = d*sin(theta)*sin(phi)
    h_pos(3,4) = d*sin(theta)*cos(phi)
  end if

end subroutine saturate_4_tetrahedral

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mod_cell_saturate
