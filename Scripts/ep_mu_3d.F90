#include "DEFINES.H"

! ep,mu
subroutine get_ep_mu( prob_lo, lo, hi, &
&            uout  , uo_lo, uo_hi, &
&            ep  , ep_lo, ep_hi, &
&            mu  , mu_lo, mu_hi, &
&            dx, nGhost,nComp,Dwave) bind(C, name="get_ep_mu")
  
implicit none

integer, intent(in) :: lo(3), hi(3), nGhost, nComp, Dwave
integer, intent(in) :: uo_lo(3), uo_hi(3)
integer, intent(in) :: ep_lo(3), ep_hi(3)
integer, intent(in) :: mu_lo(3), mu_hi(3)
double precision, intent(in) :: prob_lo(3), dx(3)
double precision, intent(inout) :: uout(uo_lo(1):uo_hi(1),uo_lo(2):uo_hi(2),uo_lo(3):uo_hi(3),0:nComp-1)
! epx,epy,epz,sqrtepx,sqrtepy,sqrtepz 
! mux,muy,muz,sqrtmux,sqrtmuy,sqrtmuz 
double precision, intent(inout) :: ep(ep_lo(1):ep_hi(1),ep_lo(2):ep_hi(2),ep_lo(3):ep_hi(3),0:5)
double precision, intent(inout) :: mu(mu_lo(1):mu_hi(1),mu_lo(2):mu_hi(2),mu_lo(3):mu_hi(3),0:5)

integer :: i, j, k
double precision :: x,y,z,r2

ep = 1.0d0
mu = 1.0d0

do    k = lo(3)-nGhost, hi(3)+nGhost
 z = prob_lo(3) + (dble(k)+0.5d0) * dx(3)

  do    j = lo(2)-nGhost, hi(2)+nGhost
   y = prob_lo(2) + (dble(j)+0.5d0) * dx(2)

     do i = lo(1)-nGhost, hi(1)+nGhost
      x = prob_lo(1) + (dble(i)+0.5d0) * dx(1)

!        CUBE_3D(0.4,0.6,0.4,0.6,0.4,0.6,9.0,1.0)     

     end do

  end do

end do

! Store in StateData
do    k = lo(3), hi(3)
 do    j = lo(2), hi(2)
  do    i = lo(1), hi(1)
      uout(i,j,k,8) = ep(i,j,k,0)
      uout(i,j,k,9) = ep(i,j,k,1)
      uout(i,j,k,10) = ep(i,j,k,2)
      uout(i,j,k,11) = mu(i,j,k,0)
      uout(i,j,k,12) = mu(i,j,k,1)
      uout(i,j,k,13) = mu(i,j,k,2)
  enddo
 enddo
enddo

end subroutine get_ep_mu
