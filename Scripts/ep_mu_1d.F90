
#include "DEFINES.H"

! ep,mu
subroutine get_ep_mu( prob_lo, lo, hi, &
&            uout, uo_lo, uo_hi, &
&            ep  , ep_lo, ep_hi, &
&            mu  , mu_lo, mu_hi, &
&            dx, nGhost, nComp, Dwave,time) bind(C, name="get_ep_mu")
  
implicit none

integer, intent(in) :: lo(1), hi(1), nGhost, nComp, Dwave
integer, intent(in) :: uo_lo(1), uo_hi(1)
integer, intent(in) :: ep_lo(1), ep_hi(1)
integer, intent(in) :: mu_lo(1), mu_hi(1)
double precision, intent(in) :: prob_lo(1), dx(1), time
double precision, intent(inout) :: uout(uo_lo(1):uo_hi(1),0:nComp-1)
! epx,epy,epz,sqrtepx,sqrtepy,sqrtepz 
! mux,muy,muz,sqrtmux,sqrtmuy,sqrtmuz 
double precision, intent(inout) :: ep(ep_lo(1):ep_hi(1),0:5)
double precision, intent(inout) :: mu(mu_lo(1):mu_hi(1),0:5)

integer :: i
double precision :: x

ep = 1.0d0
mu = 1.0d0

     do i = lo(1)-nGhost, hi(1)+nGhost
      x = prob_lo(1) + (dble(i)+0.5d0) * dx(1)

       if ((x.ge.0.0).and.(x.le.8.0)) then
         ep(i,0) = 2.25;                        
         ep(i,3) = sqrt(ep(i,0));                  
         ep(i,1) = 2.25;                         
         ep(i,4) = sqrt(ep(i,1));                  
         ep(i,2) = 2.25;                         
         ep(i,5) = sqrt(ep(i,2));               
         mu(i,0) = 1.0;                             
         mu(i,3) = sqrt(mu(i,0));                      
         mu(i,1) = 1.0;                            
         mu(i,4) = sqrt(mu(i,1));                      
         mu(i,2) = 1.0;                            
         mu(i,5) = sqrt(mu(i,2));                     
       endif

     end do


! Store in StateData

  do    i = lo(1), hi(1)
     uout(i,2) = ep(i,1)
     uout(i,3) = mu(i,2)
  enddo

end subroutine get_ep_mu
