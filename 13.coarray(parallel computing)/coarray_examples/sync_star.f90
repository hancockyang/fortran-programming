!  sync_star.f90 
!
!  FUNCTIONS:
!  sync_star - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: sync_star
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

program sync_star

   implicit none
   integer, codimension[*] :: cointeger = 99
   select case (this_image())
   case (1)
      cointeger = 10
      sync images (*)
   case (2, 3)
      sync images (1)
      print *, this_image() * cointeger[1]
   case default
   ! Image 1 hangs without this:
      sync images (1)
      print *, cointeger
   end select
   pause
end program sync_star

