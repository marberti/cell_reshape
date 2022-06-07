program main

  use mod_parameters
  use mod_rotation

  implicit none

  character(*), parameter :: my_name = "test_deg2rad"
  logical :: flag_wrong
  real(dbl) :: d
  real(dbl) :: r
  real(dbl) :: t
  real(dbl) :: q

  write(*,*) "START TEST "//my_name

  flag_wrong = .false.

  d = 45.0_dbl
  r = deg2rad(d)
  t = 0.78539816339744828_dbl
  q = abs(r-t)/t
  if (q < 1.0e-14) then
    write(*,*) "  TEST 1: OK"
  else
    write(*,*) "  TEST 1: WRONG"
    flag_wrong = .true.
  end if

  d = 90.0_dbl
  r = deg2rad(d)
  t = 1.5707963267948966_dbl
  q = abs(r-t)/t
  if (q < 1.0e-14) then
    write(*,*) "  TEST 2: OK"
  else
    write(*,*) "  TEST 2: WRONG"
    flag_wrong = .true.
  end if

  d = 180.0_dbl
  r = deg2rad(d)
  t = 3.1415926535897931_dbl
  q = abs(r-t)/t
  if (q < 1.0e-14) then
    write(*,*) "  TEST 3: OK"
  else
    write(*,*) "  TEST 3: WRONG"
    flag_wrong = .true.
  end if

  d = 217.0_dbl
  r = deg2rad(d)
  t = 3.7873644768276948_dbl
  q = abs(r-t)/t
  if (q < 1.0e-14) then
    write(*,*) "  TEST 4: OK"
  else
    write(*,*) "  TEST 4: WRONG"
    flag_wrong = .true.
  end if

  d = 360.0_dbl
  r = deg2rad(d)
  t = 6.2831853071795862_dbl
  q = abs(r-t)/t
  if (q < 1.0e-14) then
    write(*,*) "  TEST 5: OK"
  else
    write(*,*) "  TEST 5: WRONG"
    flag_wrong = .true.
  end if

  if (flag_wrong) then
    write(*,*) "TEST "//my_name//" FAILED: fix deg2rad()"
  else
    write(*,*) "TEST "//my_name//" SUCCEEDED"
  end if

end program main
