#include "DEFINES.H"

! metallic materials
! to change the material location just change the if sentences in both subroutines 
! these subroutines must have the same if sentences
subroutine metallic_materials( prob_lo, lo, hi, &
&            uin, ui_lo, ui_hi, &
&            urx  , urx_lo, urx_hi, &
&            ury  , ury_lo, ury_hi, &
&            urz  , urz_lo, urz_hi, &
&            ulx  , ulx_lo, ulx_hi, &
&            uly  , uly_lo, uly_hi, &
&            ulz  , ulz_lo, ulz_hi, &
&            dx, nGhost, nComp, Dwave) bind(C, name="metallic_materials")
  
implicit none

integer, intent(in) :: lo(3), hi(3), nGhost, nComp, Dwave
integer, intent(in) :: ui_lo(3), ui_hi(3)
integer, intent(in) :: urx_lo(3), urx_hi(3)
integer, intent(in) :: ury_lo(3), ury_hi(3)
integer, intent(in) :: urz_lo(3), urz_hi(3)
integer, intent(in) :: ulx_lo(3), ulx_hi(3)
integer, intent(in) :: uly_lo(3), uly_hi(3)
integer, intent(in) :: ulz_lo(3), ulz_hi(3)
double precision, intent(in) :: prob_lo(3), dx(3)
double precision, intent(in) :: uin(ui_lo(1):ui_hi(1),ui_lo(2):ui_hi(2),ui_lo(3):ui_hi(3),0:nComp-1)
double precision, intent(inout) :: urx(urx_lo(1):urx_hi(1),urx_lo(2):urx_hi(2),urx_lo(3):urx_hi(3),0:nComp-1)
double precision, intent(inout) :: ury(ury_lo(1):ury_hi(1),ury_lo(2):ury_hi(2),ury_lo(3):ury_hi(3),0:nComp-1)
double precision, intent(inout) :: urz(urz_lo(1):urz_hi(1),urz_lo(2):urz_hi(2),urz_lo(3):urz_hi(3),0:nComp-1)
double precision, intent(inout) :: ulx(ulx_lo(1):ulx_hi(1),ulx_lo(2):ulx_hi(2),ulx_lo(3):ulx_hi(3),0:nComp-1)
double precision, intent(inout) :: uly(uly_lo(1):uly_hi(1),uly_lo(2):uly_hi(2),uly_lo(3):uly_hi(3),0:nComp-1)
double precision, intent(inout) :: ulz(ulz_lo(1):ulz_hi(1),ulz_lo(2):ulz_hi(2),ulz_lo(3):ulz_hi(3),0:nComp-1)

integer :: i, j, k
double precision :: x,y,z,r2

do    k = lo(3)-nGhost+1, hi(3)+nGhost-1
 z = prob_lo(3) + (dble(k)+0.5d0) * dx(3)
  if((z.le.1.0).and.(z.ge.0.5)) then

  do    j = lo(2)-nGhost+1, hi(2)+nGhost-1
   y = prob_lo(2) + (dble(j)+0.5d0) * dx(2)
    if((y.le.1.0).and.(y.ge.0.5)) then

     do i = lo(1)-nGhost+1, hi(1)+nGhost-1
      x = prob_lo(1) + (dble(i)+0.5d0) * dx(1)
       if((x.le.1.0).and.(x.ge.0.5)) then

!x-axis
         ulx(i+1,j,k,1) = uin(i+1,j,k,1)
         urx(i+1,j,k,1) = -uin(i+1,j,k,1)
         ulx(i,j,k,1) = -uin(i-1,j,k,1)
         urx(i,j,k,1) = uin(i-1,j,k,1)

         ulx(i+1,j,k,2) = uin(i+1,j,k,2)
         urx(i+1,j,k,2) = -uin(i+1,j,k,2)
         ulx(i,j,k,2) = -uin(i-1,j,k,2)
         urx(i,j,k,2) = uin(i-1,j,k,2)

         ulx(i+1,j,k,4) = uin(i+1,j,k,4)
         urx(i+1,j,k,4) = uin(i+1,j,k,4)
         ulx(i,j,k,4) = uin(i-1,j,k,4)
         urx(i,j,k,4) = uin(i-1,j,k,4)

         ulx(i+1,j,k,5) = uin(i+1,j,k,5)
         urx(i+1,j,k,5) = uin(i+1,j,k,5)
         ulx(i,j,k,5) = uin(i-1,j,k,5)
         urx(i,j,k,5) = uin(i-1,j,k,5)

!y-axis
         uly(i,j+1,k,0) = uin(i,j+1,k,0)
         ury(i,j+1,k,0) = -uin(i,j+1,k,0)
         uly(i,j,k,0) = -uin(i,j-1,k,0)
         ury(i,j,k,0) = uin(i,j-1,k,0)

         uly(i,j+1,k,2) = uin(i,j+1,k,2)
         ury(i,j+1,k,2) = -uin(i,j+1,k,2)
         uly(i,j,k,2) = -uin(i,j-1,k,2)
         ury(i,j,k,2) = uin(i,j-1,k,2)

         uly(i,j+1,k,3) = uin(i,j+1,k,3)
         ury(i,j+1,k,3) = uin(i,j+1,k,3)
         uly(i,j,k,3) = uin(i,j-1,k,3)
         ury(i,j,k,3) = uin(i,j-1,k,3)

         uly(i,j+1,k,5) = uin(i,j+1,k,5)
         ury(i,j+1,k,5) = uin(i,j+1,k,5)
         uly(i,j,k,5) = uin(i,j-1,k,5)
         ury(i,j,k,5) = uin(i,j-1,k,5)

!z-axis
         ulz(i,j,k+1,0) = uin(i,j,k+1,0)
         urz(i,j,k+1,0) = -uin(i,j,k+1,0)
         ulz(i,j,k,0) = -uin(i,j,k-1,0)
         urz(i,j,k,0) = uin(i,j,k-1,0)

         ulz(i,j,k+1,1) = uin(i,j,k+1,1)
         urz(i,j,k+1,1) = -uin(i,j,k+1,1)
         ulz(i,j,k,1) = -uin(i,j,k-1,1)
         urz(i,j,k,1) = uin(i,j,k-1,1)

         ulz(i,j,k+1,3) = uin(i,j,k+1,3)
         urz(i,j,k+1,3) = uin(i,j,k+1,3)
         ulz(i,j,k,3) = uin(i,j,k-1,3)
         urz(i,j,k,3) = uin(i,j,k-1,3)

         ulz(i,j,k+1,4) = uin(i,j,k+1,4)
         urz(i,j,k+1,4) = uin(i,j,k+1,4)
         ulz(i,j,k,4) = uin(i,j,k-1,4)
         urz(i,j,k,4) = uin(i,j,k-1,4)

       endif
     end do
    endif
  end do
 endif
end do

end subroutine metallic_materials


! fill metallic materials
subroutine fill_metallic_materials( prob_lo, lo, hi, &
&            uout, uo_lo, uo_hi, &
&            dx, nGhost, nComp, Dwave) bind(C, name="fill_metallic_materials")
  
implicit none

integer, intent(in) :: lo(3), hi(3), nGhost, nComp, Dwave
integer, intent(in) :: uo_lo(3), uo_hi(3)
double precision, intent(in) :: prob_lo(3), dx(3)
double precision, intent(inout) :: uout(uo_lo(1):uo_hi(1),uo_lo(2):uo_hi(2),uo_lo(3):uo_hi(3),0:nComp-1)

integer :: i, j, k
double precision :: x,y,z,r2

do    k = lo(3), hi(3)
 z = prob_lo(3) + (dble(k)+0.5d0) * dx(3)
  if((z.le.1.0).and.(z.ge.0.5)) then

  do    j = lo(2), hi(2)
   y = prob_lo(2) + (dble(j)+0.5d0) * dx(2)
    if((y.le.1.0).and.(y.ge.0.5)) then

     do i = lo(1), hi(1)
      x = prob_lo(1) + (dble(i)+0.5d0) * dx(1)
       if((x.le.1.0).and.(x.ge.0.5)) then

               uout(i,j,k,0) = 0.0d0
               uout(i,j,k,1) = 0.0d0
               uout(i,j,k,2) = 0.0d0
               uout(i,j,k,3) = 0.0d0
               uout(i,j,k,4) = 0.0d0
               uout(i,j,k,5) = 0.0d0

       endif
     end do
    endif
  end do
 endif
end do

end subroutine fill_metallic_materials
