!  bound.f90 
!
!  FUNCTIONS:
!  bound - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: bound
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

program bound

   use bound_mod
   implicit none
   type(t_type), codimension[*] :: t
   t % p => s1
   sync all

   select case (this_image())
      case (1)
         call t%p()
         pause
         call t%s2()
         pause
         call t[2]%p()
         pause
         call t[2]%s2()
         pause
   end select
end program bound

