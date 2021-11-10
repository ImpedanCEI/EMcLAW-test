

subroutine amrex_probinit (init,name,namlen,problo,probhi) bind(c)

  implicit none

  integer, intent(in) :: init, namlen
  integer, intent(in) :: name(namlen)
  double precision, intent(in) :: problo(*), probhi(*)

  integer untin,i

end subroutine amrex_probinit


subroutine initdata(level, time, lo, hi, &
     em, em_lo, em_hi, &
     dx, prob_lo,nComp) bind(C, name="initdata")

  implicit none
  integer, intent(in) :: level, lo(3), hi(3), em_lo(3), em_hi(3), nComp
  double precision, intent(in) :: time
  double precision, intent(inout) :: em(em_lo(1):em_hi(1), &
       &                                 em_lo(2):em_hi(2), &
       &                                 em_lo(3):em_hi(3),0:nComp-1)
  double precision, intent(in) :: dx(3), prob_lo(3)

  integer          :: dm
  integer          :: i,j,k
  double precision :: x,y,z,r2

  !---declare variables for TM011
  integer          :: m, n, p, Lx, Ly, Lz
  double precision :: h_2
  real, parameter :: PI = 4.0d0*DATAN(1.0d0)
  real, parameter :: MU0 = 1 
  m=0.0
  n=1.0
  p=1.0
  Lx=1.0d0
  Ly=1.0d0
  Lz=1.0d0
  h_2 = (m*PI/Lx)**2+(n*PI/Ly)**2+(p*PI/Lz)**2
  
  !--- check for the number of dimensions
  if (em_lo(3) .eq. 0 .and. em_hi(3) .eq. 0) then
     dm = 2
     if (em_lo(2) .eq. 0 .and. em_hi(2) .eq. 0) then
        dm = 1
     endif
  else
     dm = 3
  end if

  do k=lo(3),hi(3)
     z = prob_lo(3) + (dble(k)+0.5d0) * dx(3)
     do j=lo(2),hi(2)
        y = prob_lo(2) + (dble(j)+0.5d0) * dx(2)
        do i=lo(1),hi(1)
           x = prob_lo(1) + (dble(i)+0.5d0) * dx(1)

           !--- initial conditions 1D
           if ( dm.eq.1) then
!                em(i,j,k,0) = exp(-10.0*(x+3)**2)
           endif
           !--- initial conditions 2D
           if ( dm.eq.2) then
!              r2 = ((x-0.5d0)**2 + (y-0.5d0)**2) / 0.01d0
               if (abs(x).lt.0.1) then
!                em(i,j,k) = 1.d0 !+ exp(-r2**2)
                else
!                em(i,j,k) = 0.d0
                endif
!               em(i,j,k,2) = exp(-10.0*(y+3)**2)
           endif

           !--- initial conditions 3D
           if ( dm.eq.3) then
              r2 = ((x-0.0d0)**2+(y-0.0d0)**2+(z-0.0d0)**2)/0.01d0   !implicit sphere

                 if ((abs(x).lt.0.5).and.(abs(z).lt.0.5)         &
                  & .and.(abs(y).lt.0.5)) then
               
                 !--- resonating magnetic field By, Bx, Bz
                  em(i,j,k,4) = -2.0d0/h_2*(n*PI/Ly)*(p*PI/Lz)  &
                  & *cos(m*PI/Lx*(x-Lx/2.0d0))                  &
                  & *sin(n*PI/Ly*(y-Ly/2.0d0))                  &
                  & *cos(p*PI/Lz*(z-Lz/2.0d0))*MU0 
                  em(i,j,k,3) = -2.0d0/h_2*(m*PI/Ly)*(p*PI/Lz)  &
                  & *sin(m*PI/Lx*(x-Lx/2.0d0))                  &
                  & *cos(n*PI/Ly*(y-Ly/2.0d0))                  &
                  & *cos(p*PI/Lz*(z-Lz/2.0d0))*MU0              
                  em(i,j,k,5) = cos(m*PI/Lx*(x-Lx/2.0d0))       &
                  & *cos(n*PI/Ly*(y-Ly/2.0d0))                  &
                  & *cos(p*PI/Lz*(z-Lz/2.0d0))*MU0

                 !--- constant magnetic field By, Bx, Bz
!                  em(i,j,k,4) = 1.0d0
!                  em(i,j,k,3) = 1.0d0
!                  em(i,j,k,5) = 1.0d0
                 else

                 em(i,j,k,0) = 0.d0

                 endif
           end if

        end do
     end do
  end do

end subroutine initdata

