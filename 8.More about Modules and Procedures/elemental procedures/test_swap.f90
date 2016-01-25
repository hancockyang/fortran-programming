!  test_dice_roll.f90 
!
!  FUNCTIONS:
!  test_dice_roll - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: test_dice_roll
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

 program test_swap

   use myfunctions, only: swap
   implicit none
   integer, dimension(3) :: i = [1, 2, 3], j = [7, 8, 9]
   real,    dimension(3) :: a = [1.0, 2.0, 3.0], b = [7.0, 8.0, 9.0]
   call swap(i, j)
   print *, i
   print *, j
   call swap(a, b)
   print *, a
   print *, b
   pause  
 end program test_swap

