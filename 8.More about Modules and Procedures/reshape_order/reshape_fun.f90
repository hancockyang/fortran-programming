!  find_loc.f90 
!
!  FUNCTIONS:
!  find_loc - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: find_loc
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

program reshape_fun
   implicit none
   integer::i
   integer, dimension (3,3) :: X = reshape ([ -11, 12, -13, 21, 22, -23, 31, -32, -33 ],[ 3, 3 ] ,order = [ 2, 1])
   print "(3i5)", [(X(i,:), i=1,3)]
   pause

end program reshape_fun

