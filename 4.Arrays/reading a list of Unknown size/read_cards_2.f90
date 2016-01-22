!  read_cards_2.f90 
!
!  FUNCTIONS:
!  read_cards_2 - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: read_cards_2
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

program read_cards_2
   use myfunctions
   implicit none
   double precision, dimension(:), allocatable:: lost_card
   double precision:: card
   integer:: ios
   character(len=99) :: iom

   open(unit = 10,file='card.txt');
      do
         
         read(unit=10, fmt =*, iostat=ios, iomsg=iom) card
         if(ios < 0) exit
         if(ios > 0) then        
            print *, trim(iom)
            pause
            cycle
         end if
         CALL append(lost_card,card)
      end do
   close(unit = 10);
   print *, lost_card
   pause
   
   deallocate(lost_card)

end program read_cards_2

!subroutine append(list, element)
!
!    IMPLICIT NONE
!
!    integer :: i, isize
!    double precision, intent(in) :: element
!    double precision, dimension(:), allocatable, intent(inout) :: list
!    double precision, dimension(:), allocatable :: clist
!
!
!    if(allocated(list)) then
!        isize = size(list)
!        allocate(clist(isize+1))
!        clist(1:isize) = list(1:isize)
!        clist(isize+1) = element
!        deallocate(list)
!        call move_alloc(clist, list)
!
!    else
!        allocate(list(1))
!        list(1) = element
!    end if
!
!
!end subroutine append
