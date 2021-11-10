#include "DEFINES.H"

! metallic materials
! to change the material location just change the if sentences in both subroutines 
! these subroutines must have the same if sentences
subroutine metallic_materials( prob_lo, lo, hi, &
&            uin, ui_lo, ui_hi, &
&            urx  , urx_lo, urx_hi, &
&            ury  , ury_lo, ury_hi, &
&            ulx  , ulx_lo, ulx_hi, &
&            uly  , uly_lo, uly_hi, &
&            dx, nGhost, nComp, Dwave) bind(C, name="metallic_materials")
  
implicit none

integer, intent(in) :: lo(2), hi(2), nGhost, nComp, Dwave
integer, intent(in) :: ui_lo(2), ui_hi(2)
integer, intent(in) :: urx_lo(2), urx_hi(2)
integer, intent(in) :: ury_lo(2), ury_hi(2)
integer, intent(in) :: ulx_lo(2), ulx_hi(2)
integer, intent(in) :: uly_lo(2), uly_hi(2)
double precision, intent(in) :: prob_lo(2), dx(2)
double precision, intent(in) :: uin(ui_lo(1):ui_hi(1),ui_lo(2):ui_hi(2),0:nComp-1)
double precision, intent(inout) :: urx(urx_lo(1):urx_hi(1),urx_lo(2):urx_hi(2),0:nComp-1)
double precision, intent(inout) :: ury(ury_lo(1):ury_hi(1),ury_lo(2):ury_hi(2),0:nComp-1)
double precision, intent(inout) :: ulx(ulx_lo(1):ulx_hi(1),ulx_lo(2):ulx_hi(2),0:nComp-1)
double precision, intent(inout) :: uly(uly_lo(1):uly_hi(1),uly_lo(2):uly_hi(2),0:nComp-1)

integer :: i, j
double precision :: x,y,r2

  if (Dwave.eq.1) then ! D-wave

  do    j = lo(2)-nGhost+1, hi(2)+nGhost-1
   y = prob_lo(2) + (dble(j)+0.5d0) * dx(2)

!    if((y.le.0.6).and.(y.ge.0.4)) then

     do i = lo(1)-nGhost+1, hi(1)+nGhost-1
      x = prob_lo(1) + (dble(i)+0.5d0) * dx(1)

       r2 = ((x-0.0d0)**2 + (y-0.0d0)**2)/0.5d0
!       if((x.le.0.45).or.(x.ge.0.55)) then
       if(r2.le.0.1) then

!x-axis
         ulx(i+1,j,1) = uin(i+1,j,1)
         urx(i+1,j,1) = -uin(i+1,j,1)
         ulx(i,j,1) = -uin(i-1,j,1)
         urx(i,j,1) = uin(i-1,j,1)

         ulx(i+1,j,2) = uin(i+1,j,2)
         urx(i+1,j,2) = uin(i+1,j,2)
         ulx(i,j,2) = uin(i-1,j,2)
         urx(i,j,2) = uin(i-1,j,2)

!y-axis
         uly(i,j+1,0) = uin(i,j+1,0)
         ury(i,j+1,0) = -uin(i,j+1,0)
         uly(i,j,0) = -uin(i,j-1,0)
         ury(i,j,0) = uin(i,j-1,0)

         uly(i,j+1,2) = uin(i,j+1,2)
         ury(i,j+1,2) = uin(i,j+1,2)
         uly(i,j,2) = uin(i,j-1,2)
         ury(i,j,2) = uin(i,j-1,2)

       endif

     end do

!    endif

  end do

  else ! B-wave

  do    j = lo(2)-nGhost+1, hi(2)+nGhost-1
   y = prob_lo(2) + (dble(j)+0.5d0) * dx(2)

!    if((y.le.0.9).and.(y.ge.0.7)) then

     do i = lo(1)-nGhost+1, hi(1)+nGhost-1
      x = prob_lo(1) + (dble(i)+0.5d0) * dx(1)

       r2 = ((x-0.0d0)**2 + (y-0.0d0)**2)**2
!       if((x.le.0.9).and.(x.ge.0.7)) then
       if(r2.le.1.0) then

!x-axis
         ulx(i+1,j,1) = uin(i+1,j,1)
         urx(i+1,j,1) = uin(i+1,j,1)
         ulx(i,j,1) = uin(i-1,j,1)
         urx(i,j,1) = uin(i-1,j,1)

         ulx(i+1,j,2) = uin(i+1,j,2)
         urx(i+1,j,2) = -uin(i+1,j,2)
         ulx(i,j,2) = -uin(i-1,j,2)
         urx(i,j,2) = uin(i-1,j,2)

!y-axis
         uly(i,j+1,0) = uin(i,j+1,0)
         ury(i,j+1,0) = uin(i,j+1,0)
         uly(i,j,0) = uin(i,j-1,0)
         ury(i,j,0) = uin(i,j-1,0)

         uly(i,j+1,2) = uin(i,j+1,2)
         ury(i,j+1,2) = -uin(i,j+1,2)
         uly(i,j,2) = -uin(i,j-1,2)
         ury(i,j,2) = uin(i,j-1,2)

       endif

     end do

!    endif

  end do

  endif ! D-wave or B-wave

end subroutine metallic_materials


! metallic materials
subroutine fill_metallic_materials( prob_lo, lo, hi, &
&            uout, uo_lo, uo_hi, &
&            dx, nGhost, nComp, Dwave) bind(C, name="fill_metallic_materials")
  
implicit none

integer, intent(in) :: lo(2), hi(2), nGhost, nComp, Dwave
integer, intent(in) :: uo_lo(2), uo_hi(2)
double precision, intent(in) :: prob_lo(2), dx(2)
double precision, intent(inout) :: uout(uo_lo(1):uo_hi(1),uo_lo(2):uo_hi(2),0:nComp-1)

integer :: i, j
double precision :: x,y,r2

  if (Dwave.eq.1) then ! D-wave

  do    j = lo(2), hi(2)
   y = prob_lo(2) + (dble(j)+0.5d0) * dx(2)

    if((y.le.0.6).and.(y.ge.0.4)) then

     do i = lo(1), hi(1)
      x = prob_lo(1) + (dble(i)+0.5d0) * dx(1)

       r2 = ((x-0.5d0)**2 + (y-0.5d0)**2)/0.5d0
       if((x.le.0.45).or.(x.ge.0.55)) then
!       if(r2.le.0.1) then

         uout(i,j,0) = 0.0d0
         uout(i,j,1) = 0.0d0

       endif

     end do

    endif

  end do

  else ! B-wave

  do    j = lo(2), hi(2)
   y = prob_lo(2) + (dble(j)+0.5d0) * dx(2)

!    if((y.le.0.9).and.(y.ge.0.7)) then

     do i = lo(1), hi(1)
      x = prob_lo(1) + (dble(i)+0.5d0) * dx(1)

       r2 = ((x-0.0d0)**2 + (y-0.0d0)**2)**2
!       if((x.le.0.9).and.(x.ge.0.7)) then
       if(r2.le.1.0) then

         uout(i,j,2) = 0.0d0

       endif

     end do

!    endif

  end do

  endif ! D-wave or B-wave

end subroutine fill_metallic_materials
