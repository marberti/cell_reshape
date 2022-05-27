module mod_periodic_table

  use mod_parameters
  use mod_error

  implicit none
  save
  private

  public :: periodic_table_len
  public :: periodic_table
  public :: pt_get_r_mean
  public :: pt_get_r_max

  type :: element_t
    character(2) :: e
    character(2) :: ef
    integer :: z
    real(dbl) :: m
    real(dbl) :: r_mean
    real(dbl) :: r_max
  end type

  integer, parameter :: periodic_table_len = 118 

  type(element_t), dimension(periodic_table_len), parameter :: &
  periodic_table = (/ &
    element_t(e="H" ,ef="H ",z=  1,m=  1.0080_dbl,r_mean=0.320_dbl,r_max=0.425_dbl),&
    element_t(e="He",ef="He",z=  2,m=  4.0026_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Li",ef="Li",z=  3,m=  6.9400_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Be",ef="Be",z=  4,m=  9.0122_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="B" ,ef="B ",z=  5,m= 10.8100_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="C" ,ef="C ",z=  6,m= 12.0110_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="N" ,ef="N ",z=  7,m= 14.0070_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="O" ,ef="O ",z=  8,m= 15.9990_dbl,r_mean=0.630_dbl,r_max=0.875_dbl),&
    element_t(e="F" ,ef="F ",z=  9,m= 18.9980_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Ne",ef="Ne",z= 10,m= 20.1800_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Na",ef="Na",z= 11,m= 22.9900_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Mg",ef="Mg",z= 12,m= 24.3050_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Al",ef="Al",z= 13,m= 26.9820_dbl,r_mean=1.260_dbl,r_max=1.545_dbl),&
    element_t(e="Si",ef="Si",z= 14,m= 28.0850_dbl,r_mean=1.160_dbl,r_max=1.395_dbl),&
    element_t(e="P" ,ef="P ",z= 15,m= 30.9740_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="S" ,ef="S ",z= 16,m= 32.0600_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Cl",ef="Cl",z= 17,m= 35.4500_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Ar",ef="Ar",z= 18,m= 39.9480_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="K" ,ef="K ",z= 19,m= 39.0980_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Ca",ef="Ca",z= 20,m= 40.0780_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Sc",ef="Sc",z= 21,m= 44.9560_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Ti",ef="Ti",z= 22,m= 47.8670_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="V" ,ef="V ",z= 23,m= 50.9420_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Cr",ef="Cr",z= 24,m= 51.9960_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Mn",ef="Mn",z= 25,m= 54.9380_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Fe",ef="Fe",z= 26,m= 55.8450_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Co",ef="Co",z= 27,m= 58.9330_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Ni",ef="Ni",z= 28,m= 58.6930_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Cu",ef="Cu",z= 29,m= 63.5460_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Zn",ef="Zn",z= 30,m= 65.3800_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Ga",ef="Ga",z= 31,m= 69.7230_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Ge",ef="Ge",z= 32,m= 72.6300_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="As",ef="As",z= 33,m= 74.9220_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Se",ef="Se",z= 34,m= 78.9710_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Br",ef="Br",z= 35,m= 79.9040_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Kr",ef="Kr",z= 36,m= 83.7980_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Rb",ef="Rb",z= 37,m= 85.4680_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Sr",ef="Sr",z= 38,m= 87.6200_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Y" ,ef="Y ",z= 39,m= 88.9060_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Zr",ef="Zr",z= 40,m= 91.2240_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Nb",ef="Nb",z= 41,m= 92.9060_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Mo",ef="Mo",z= 42,m= 95.9500_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Tc",ef="Tc",z= 43,m= 97.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Ru",ef="Ru",z= 44,m=101.0700_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Rh",ef="Rh",z= 45,m=102.9100_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Pd",ef="Pd",z= 46,m=106.4200_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Ag",ef="Ag",z= 47,m=107.8700_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Cd",ef="Cd",z= 48,m=112.4100_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="In",ef="In",z= 49,m=114.8200_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Sn",ef="Sn",z= 50,m=118.7100_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Sb",ef="Sb",z= 51,m=121.7600_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Te",ef="Te",z= 52,m=127.6000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="I" ,ef="I ",z= 53,m=126.9000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Xe",ef="Xe",z= 54,m=131.2900_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Cs",ef="Cs",z= 55,m=132.9100_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Ba",ef="Ba",z= 56,m=137.3300_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="La",ef="La",z= 57,m=138.9100_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Ce",ef="Ce",z= 58,m=140.1200_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Pr",ef="Pr",z= 59,m=140.9100_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Nd",ef="Nd",z= 60,m=144.2400_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Pm",ef="Pm",z= 61,m=145.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Sm",ef="Sm",z= 62,m=150.3600_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Eu",ef="Eu",z= 63,m=151.9600_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Gd",ef="Gd",z= 64,m=157.2500_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Tb",ef="Tb",z= 65,m=158.9300_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Dy",ef="Dy",z= 66,m=162.5000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Ho",ef="Ho",z= 67,m=164.9300_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Er",ef="Er",z= 68,m=167.2600_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Tm",ef="Tm",z= 69,m=168.9300_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Yb",ef="Yb",z= 70,m=173.0500_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Lu",ef="Lu",z= 71,m=174.9700_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Hf",ef="Hf",z= 72,m=178.4900_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Ta",ef="Ta",z= 73,m=180.9500_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="W" ,ef="W ",z= 74,m=183.8400_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Re",ef="Re",z= 75,m=186.2100_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Os",ef="Os",z= 76,m=190.2300_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Ir",ef="Ir",z= 77,m=192.2200_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Pt",ef="Pt",z= 78,m=195.0800_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Au",ef="Au",z= 79,m=196.9700_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Hg",ef="Hg",z= 80,m=200.5900_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Tl",ef="Tl",z= 81,m=204.3800_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Pb",ef="Pb",z= 82,m=207.2000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Bi",ef="Bi",z= 83,m=208.9800_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Po",ef="Po",z= 84,m=209.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="At",ef="At",z= 85,m=210.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Rn",ef="Rn",z= 86,m=222.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Fr",ef="Fr",z= 87,m=223.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Ra",ef="Ra",z= 88,m=226.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Ac",ef="Ac",z= 89,m=227.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Th",ef="Th",z= 90,m=232.0400_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Pa",ef="Pa",z= 91,m=231.0400_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="U" ,ef="U ",z= 92,m=238.0300_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Np",ef="Np",z= 93,m=237.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Pu",ef="Pu",z= 94,m=244.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Am",ef="Am",z= 95,m=243.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Cm",ef="Cm",z= 96,m=247.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Bk",ef="Bk",z= 97,m=247.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Cf",ef="Cf",z= 98,m=251.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Es",ef="Es",z= 99,m=252.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Fm",ef="Fm",z=100,m=257.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Md",ef="Md",z=101,m=258.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="No",ef="No",z=102,m=259.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Lr",ef="Lr",z=103,m=266.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Rf",ef="Rf",z=104,m=267.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Db",ef="Db",z=105,m=268.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Sg",ef="Sg",z=106,m=269.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Bh",ef="Bh",z=107,m=270.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Hs",ef="Hs",z=108,m=269.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Mt",ef="Mt",z=109,m=278.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Ds",ef="Ds",z=110,m=281.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Rg",ef="Rg",z=111,m=282.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Cn",ef="Cn",z=112,m=285.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Nh",ef="Nh",z=113,m=286.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Fl",ef="Fl",z=114,m=289.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Mc",ef="Mc",z=115,m=289.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Lv",ef="Lv",z=116,m=293.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Ts",ef="Ts",z=117,m=294.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl),&
    element_t(e="Og",ef="Og",z=118,m=294.0000_dbl,r_mean=0.000_dbl,r_max=0.000_dbl) &
  /)

contains

real(dbl) function pt_get_r_mean(e)

  character(2), intent(in) :: e
  character(*), parameter :: my_name = "pt_get_r_mean"
  integer :: i
  logical :: flag_found_e

  ! TODO remove when r_mean and r_max will be implemented for all elements
  select case (e)
  case ("H")
    continue
  case ("O")
    continue
  case ("Al")
    continue
  case ("Si")
    continue
  case default
    call error(my_name,"r_mean for element "//trim(e)//" not implemented yet")
  end select
  ! end

  flag_found_e = .false.

  do i = 1, periodic_table_len
    if (e == periodic_table(i)%e) then
      pt_get_r_mean = periodic_table(i)%r_mean
      flag_found_e = .true.
      exit
    end if
  end do

  if (.not.flag_found_e) call error(my_name,"element "//trim(e)//" not found")

end function pt_get_r_mean

real(dbl) function pt_get_r_max(e)

  character(2), intent(in) :: e
  character(*), parameter :: my_name = "pt_get_r_max"
  integer :: i
  logical :: flag_found_e

  ! TODO remove when r_mean and r_max will be implemented for all elements
  select case (e)
  case ("H")
    continue
  case ("O")
    continue
  case ("Al")
    continue
  case ("Si")
    continue
  case default
    call error(my_name,"r_max for element "//trim(e)//" not implemented yet")
  end select
  ! end

  flag_found_e = .false.

  do i = 1, periodic_table_len
    if (e == periodic_table(i)%e) then
      pt_get_r_max = periodic_table(i)%r_max
      flag_found_e = .true.
      exit
    end if
  end do

  if (.not.flag_found_e) call error(my_name,"element "//trim(e)//" not found")

end function pt_get_r_max

end module mod_periodic_table
