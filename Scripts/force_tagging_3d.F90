#include "DEFINES.H"

! ::: -----------------------------------------------------------
! ::: This routine will tag sources 
! ::: 
! ::: INPUTS/OUTPUTS:
! ::: 
! ::: tag        <=  integer tag array
! ::: tag_lo,hi   => index extent of tag array
! ::: set         => integer value to tag cell for refinement
! ::: clear       => integer value to untag cell
! ::: lo,hi       => work region we are allowed to change
! ::: dx          => cell size
! ::: problo      => phys loc of lower left corner of prob domain
! ::: time        => problem evolution time
! ::: level       => refinement level of this array
! ::: -----------------------------------------------------------

subroutine force_tagging_error(tag,tag_lo,tag_hi, &
                       set,clear,&
                       lo,hi,&
                       dx,problo,time,level) bind(C, name="force_tagging_error")

  implicit none
  
  integer          :: lo(3),hi(3)
  integer          :: tag_lo(3),tag_hi(3)
  integer          :: tag(tag_lo(1):tag_hi(1),tag_lo(2):tag_hi(2),tag_lo(3):tag_hi(3))
  double precision :: problo(3),dx(3),time
  integer          :: level,set,clear

  integer          :: i, j, k
  double precision :: x, y, z

  ! Tag at will
  
do    k = lo(3), hi(3)
 z = problo(3) + (dble(k)+0.5d0) * dx(3)
  if((z.le.0.9).and.(z.ge.0.7)) then

  do    j = lo(2), hi(2)
   y = problo(2) + (dble(j)+0.5d0) * dx(2)
    if((y.le.0.9).and.(y.ge.0.7)) then

     do i = lo(1), hi(1)
      x = problo(1) + (dble(i)+0.5d0) * dx(1)
       if((x.le.0.9).and.(x.ge.0.7)) then

!                 tag(i,j,k) = set

       endif
     end do
    endif
  end do
 endif
end do

end subroutine force_tagging_error
