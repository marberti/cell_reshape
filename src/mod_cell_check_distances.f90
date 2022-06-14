module mod_cell_check_distances

  use mod_error
  use mod_cell_t
  use mod_connectivity
  use mod_periodic_table

  implicit none
  save
  private

  public :: cell_check_distances

contains

subroutine cell_check_distances(c)

  type(cell_t), intent(in) :: c
  character(*), parameter :: my_name = "cell_check_distances"
  integer :: i
  integer :: j
  integer :: n
  character(5) :: str1
  character(5) :: str2
  real(dbl), dimension(:,:), allocatable :: rd
  real(dbl), dimension(:,:), allocatable :: td
  logical, dimension(:,:), allocatable :: lm
  real(dbl) :: mdf
  logical :: flag_found_close
  integer :: err_n
  character(120) :: err_msg

  flag_found_close = .false.
  n = c%xyz%n

  ! allocation section
  allocate(rd(n,n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  allocate(td(n,n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  allocate(lm(n,n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  ! get real distances rd
  call get_connectivity_matrix(c%xyz%e,c%xyz%x,c%xyz%y,c%xyz%z,rd,lm)

  ! get threshold distances td
  do i = 1, n
    do j = i+1, n
      if (c%xyz%e(i) == "H") then
        if (c%xyz%e(j) == "H") then
          mdf = 0.10_dbl
        else
          mdf = 0.15_dbl
        end if
      else
        mdf = 0.20_dbl
      end if
      td(i,j) = pt_get_r_mean(c%xyz%e(i)) + pt_get_r_mean(c%xyz%e(j)) - mdf
    end do
  end do

  ! check distances
  do i = 1, n
    do j = i+1, n
      if (rd(i,j) < td(i,j)) then
        flag_found_close = .true.
        write(str1,'(I5)') i
        str1 = adjustl(str1)
        write(str2,'(I5)') j
        str2 = adjustl(str2)
        write(*,'(1X,A,F7.3,A,F7.3,A)')            &
          "Distance too short (",                  &
          rd(i,j)," < ",td(i,j),                   &
          ") between atoms "//                     &
          trim(c%xyz%e(i))//trim(str1)//" and "//  &
          trim(c%xyz%e(j))//trim(str2)
      end if
    end do
  end do

  if (flag_found_close.eqv..false.) then
    write(*,*) "Atoms too close: not found"
  end if

  ! deallocation section
  deallocate(rd,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  deallocate(td,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  deallocate(lm,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

end subroutine cell_check_distances

end module mod_cell_check_distances
