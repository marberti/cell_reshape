module mod_cell_reshape

  use mod_parameters
  use mod_error
  use mod_p3d_t
  use mod_cell_t
  use mod_logical
  use mod_connectivity

  implicit none
  save
  private

  public :: cell_reshape

contains

! Public !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine cell_reshape(cin,cout,center)

  type(cell_t), intent(in)  :: cin
  type(cell_t), intent(out) :: cout
  type(p3d_t), intent(in) :: center
  character(*), parameter :: my_name = "cell_reshape"
  real(dbl), dimension(:), allocatable :: moved_x
  real(dbl), dimension(:), allocatable :: moved_y
  real(dbl), dimension(:), allocatable :: moved_z
  real(dbl), dimension(:,:), allocatable :: dm
  logical, dimension(:,:), allocatable :: lm_old
  logical, dimension(:,:), allocatable :: lm_new
  integer :: n
  integer :: i
  character(5) :: i_str
  integer :: err_n
  character(120) :: err_msg

  n = cin%xyz%n

  ! allocation section
  allocate(moved_x(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  allocate(moved_y(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  allocate(moved_z(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  allocate(dm(n,n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  allocate(lm_old(n,n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  allocate(lm_new(n,n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  ! allocate and set cout
  cout%a = cin%a
  cout%b = cin%b
  cout%c = cin%c

  cout%xyz%n = n
  allocate(cout%xyz%e(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  allocate(cout%xyz%x(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  allocate(cout%xyz%y(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  allocate(cout%xyz%z(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  cout%xyz%e = cin%xyz%e
  cout%xyz%x = cin%xyz%x
  cout%xyz%y = cin%xyz%y
  cout%xyz%z = cin%xyz%z

  ! get old connectivity matrix (before reshape)
  call get_connectivity_matrix(                           &
    cout%xyz%e,cout%xyz%x,cout%xyz%y,cout%xyz%z,dm,lm_old &
  )

  ! reshape cout
  call reshape_array(cout%xyz%x,cout%a,center%x,moved_x)
  call reshape_array(cout%xyz%y,cout%b,center%y,moved_y)
  call reshape_array(cout%xyz%z,cout%c,center%z,moved_z)

  ! get new connectivity matrix (after reshape)
  call get_connectivity_matrix(                           &
    cout%xyz%e,cout%xyz%x,cout%xyz%y,cout%xyz%z,dm,lm_new &
  )

  ! check for isolated atoms
  do i = 1, n
    if ((count_true(lm_new(i,:)) == 0).and. &
        (count_true(lm_old(i,:)) /= 0)) then
      write(i_str,'(I5)') i
!      write(*,*) "Atom "//i_str//" became isolated after reshaping: "//&
!        "restored to its original position"
!      cout%xyz%x(i) = cout%xyz%x(i) - moved_x(i)
!      cout%xyz%y(i) = cout%xyz%y(i) - moved_y(i)
!      cout%xyz%z(i) = cout%xyz%z(i) - moved_z(i)
      write(*,'(A,3(F9.4))') " Atom "//i_str//" isolated, moved of: ",&
        moved_x(i), moved_y(i), moved_z(i)
    end if
  end do

  ! deallocation section
  deallocate(moved_x,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  deallocate(moved_y,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  deallocate(moved_z,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  deallocate(dm,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  deallocate(lm_old,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  deallocate(lm_new,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

end subroutine cell_reshape

! Private !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine reshape_array(a,d,c,moved)

  real(dbl), dimension(:), intent(inout) :: a
  real(dbl), intent(in) :: d
  real(dbl), intent(in) :: c
  real(dbl), dimension(:), intent(out) :: moved
  real(dbl) :: t ! threshold
  integer :: n
  integer :: i

  moved = 0.0_dbl
  t = d / 2.0_dbl
  n = size(a)

  do i=1, n
    if (abs(c-a(i)) > t) then
      if (a(i) > c) then
        a(i) = a(i) - d
        moved(i) = -d
      else
        a(i) = a(i) + d
        moved(i) = d
      end if
    end if
  end do

end subroutine reshape_array

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mod_cell_reshape
