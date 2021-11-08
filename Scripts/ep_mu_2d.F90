#include "DEFINES.H"

! ep,mu
subroutine get_ep_mu( prob_lo, lo, hi, &
&            uout, uo_lo, uo_hi, &
&            ep  , ep_lo, ep_hi, &
&            mu  , mu_lo, mu_hi, &
&            dx, nGhost, nComp, Dwave,time) bind(C, name="get_ep_mu")
  
implicit none

integer, intent(in) :: lo(2), hi(2), nGhost, nComp, Dwave
integer, intent(in) :: uo_lo(2), uo_hi(2)
integer, intent(in) :: ep_lo(2), ep_hi(2)
integer, intent(in) :: mu_lo(2), mu_hi(2)
double precision, intent(in) :: prob_lo(2), dx(2), time
double precision, intent(inout) :: uout(uo_lo(1):uo_hi(1),uo_lo(2):uo_hi(2),0:nComp-1)
! epx,epy,epz,sqrtepx,sqrtepy,sqrtepz 
! mux,muy,muz,sqrtmux,sqrtmuy,sqrtmuz 
double precision, intent(inout) :: ep(ep_lo(1):ep_hi(1),ep_lo(2):ep_hi(2),0:5)
double precision, intent(inout) :: mu(mu_lo(1):mu_hi(1),mu_lo(2):mu_hi(2),0:5)

integer :: i, j
double precision :: x,y,r2

ep = 1.0d0
mu = 1.0d0

  do    j = lo(2)-nGhost, hi(2)+nGhost
   y = prob_lo(2) + (dble(j)+0.5d0) * dx(2)

     do i = lo(1)-nGhost, hi(1)+nGhost
      x = prob_lo(1) + (dble(i)+0.5d0) * dx(1)

!        CIRCLE_2D(0.0,0.0,0.25,4.0d0,1.0d0)      

     end do

  end do

! Store in StateData
if (Dwave.eq.1) then

 do    j = lo(2), hi(2)
  do    i = lo(1), hi(1)
     uout(i,j,4) = ep(i,j,0)
     uout(i,j,5) = ep(i,j,1)
     uout(i,j,6) = mu(i,j,2)
  enddo
 enddo

else

 do    j = lo(2), hi(2)
  do    i = lo(1), hi(1)
     uout(i,j,4) = mu(i,j,0)
     uout(i,j,5) = mu(i,j,1)
     uout(i,j,6) = ep(i,j,2)
  enddo
 enddo

endif

end subroutine get_ep_mu
