#include "DEFINES.H"

! metallic materials
! to change the material location just change the if sentences in both subroutines 
! these subroutines must have the same if sentences
subroutine metallic_materials( prob_lo, lo, hi, &
&            uin, ui_lo, ui_hi, &
&            urx  , urx_lo, urx_hi, &
&            ulx  , ulx_lo, ulx_hi, &
&            dx, nGhost, nComp, Dwave) bind(C, name="metallic_materials")
  
implicit none

integer, intent(in) :: lo(1), hi(1), nGhost, nComp, Dwave
integer, intent(in) :: ui_lo(1), ui_hi(1)
integer, intent(in) :: urx_lo(1), urx_hi(1)
integer, intent(in) :: ulx_lo(1), ulx_hi(1)
double precision, intent(in) :: prob_lo(1), dx(1)
double precision, intent(in) :: uin(ui_lo(1):ui_hi(1),0:nComp-1)
double precision, intent(inout) :: urx(urx_lo(1):urx_hi(1),0:nComp-1)
double precision, intent(inout) :: ulx(ulx_lo(1):ulx_hi(1),0:nComp-1)

integer :: i
double precision :: x


     do i = lo(1)-nGhost+1, hi(1)+nGhost-1
      x = prob_lo(1) + (dble(i)+0.5d0) * dx(1)

       if((x.le.0.45).or.(x.ge.0.55)) then

!x-axis
         ulx(i+1,0) = uin(i+1,0)
         urx(i+1,0) = -uin(i+1,0)
         ulx(i,0) = -uin(i-1,0)
         urx(i,0) = uin(i-1,0)

         ulx(i+1,1) = uin(i+1,1)
         urx(i+1,1) = uin(i+1,1)
         ulx(i,1) = uin(i-1,1)
         urx(i,1) = uin(i-1,1)

        endif

      enddo

end subroutine metallic_materials


! metallic materials
subroutine fill_metallic_materials( prob_lo, lo, hi, &
&            uout, uo_lo, uo_hi, &
&            dx, nGhost, nComp, Dwave) bind(C, name="fill_metallic_materials")
  
implicit none

integer, intent(in) :: lo(1), hi(1), nGhost, nComp, Dwave
integer, intent(in) :: uo_lo(1), uo_hi(1)
double precision, intent(in) :: prob_lo(1), dx(1)
double precision, intent(inout) :: uout(uo_lo(1):uo_hi(1),0:nComp-1)

integer :: i
double precision :: x

     do i = lo(1), hi(1)
      x = prob_lo(1) + (dble(i)+0.5d0) * dx(1)

       if((x.le.0.45).or.(x.ge.0.55)) then

         uout(i,0) = 0.0d0

       endif

     end do

end subroutine fill_metallic_materials
