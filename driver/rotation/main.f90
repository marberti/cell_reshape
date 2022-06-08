program main

  use mod_parameters
  use mod_rotation

  implicit none

  integer, parameter :: mlen = 4
  real(dbl), dimension(3,mlen) :: molecule_in
  real(dbl), dimension(3,mlen) :: molecule_out
  character(2), dimension(mlen) :: elem
  integer :: axis
  real(dbl) :: angle_deg
  real(dbl) :: angle_rad
  integer :: i

  elem(1) = "N"
  elem(2) = "H"
  elem(3) = "H"
  elem(4) = "H"

  molecule_in(1,1) =  0.000000_dbl
  molecule_in(2,1) =  0.000000_dbl
  molecule_in(3,1) =  0.000000_dbl
  molecule_in(1,2) =  0.000000_dbl
  molecule_in(2,2) =  0.000000_dbl
  molecule_in(3,2) =  1.030000_dbl
  molecule_in(1,3) =  0.971095_dbl
  molecule_in(2,3) =  0.000000_dbl
  molecule_in(3,3) = -0.343330_dbl
  molecule_in(1,4) = -0.485547_dbl
  molecule_in(2,4) = -0.840991_dbl
  molecule_in(3,4) = -0.343333_dbl

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  do axis = 1, 3
    angle_deg = 0.0_dbl
    do
      if (angle_deg > 360.0_dbl) exit

      angle_rad = deg2rad(angle_deg)

      do i = 1, mlen
        call rotate3d(molecule_in(:,i),molecule_out(:,i),axis,angle_rad)
      end do

      write(*,*) mlen
      write(*,'(1X,A,I1,A,F6.1,A)') "ROT ",axis," -> ",angle_deg," deg"
      do i = 1, mlen
        write(*,*) elem(i),molecule_out(1,i),molecule_out(2,i),molecule_out(3,i)
      end do

      angle_deg = angle_Deg + 5.0_dbl
    end do

    angle_deg = 0.0_dbl
    do
      if (angle_deg < -360.0_dbl) exit

      angle_rad = deg2rad(angle_deg)

      do i = 1, mlen
        call rotate3d(molecule_in(:,i),molecule_out(:,i),axis,angle_rad)
      end do

      write(*,*) mlen
      write(*,'(1X,A,I1,A,F6.1,A)') "ROT ",axis," -> ",angle_deg," deg"
      do i = 1, mlen
        write(*,*) elem(i),molecule_out(1,i),molecule_out(2,i),molecule_out(3,i)
      end do

      angle_deg = angle_Deg - 5.0_dbl
    end do
  end do

end program main
