!  ptr_comp.f90 
!
!  FUNCTIONS:
!  ptr_comp - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: ptr_comp
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************
! A coarray may have a component that is allocatable or a pointer.
! A coarray may not be a pointer.
! If the target of a pointer is a coarray, both the pointer and the target must be on the
! same image. That is, a coarray pointer may not point to a target on a different image.
program ptr_comp

   implicit none
   real, target :: x = 1.1, y = 2.2
   type :: s_type
      real, pointer :: ptr
   end type s_type
   
   type (s_type), codimension[*] :: s
   select case (this_image())
      case (1)
         s%ptr => x
         sync images (2)
      case (2)
        
         s%ptr => y
         print *, s[1]%ptr ! 1.1
         print *, s%ptr ! 2.2
         sync images (1)
         pause
   end select
   !sync all
   !print *, s[1]%ptr 
   !print *, s[2]%ptr 
   
   
end program ptr_comp

