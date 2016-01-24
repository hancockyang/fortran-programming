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

 program test_random

   use myfunctions, only: random
   implicit none
   integer, parameter :: number_of_rolls = 10
   integer, dimension (number_of_rolls) ::  die_1, die_2
   real, dimension (number_of_rolls) ::  die_3, die_4
! run test 1 to 6 by 10 times for integers
   call random_seed()
   call random (die_1, 1, 6)
   call random (die_2, 1, 6)

   print "(10i2)", die_1
   print "(10i2)", die_2
   
! run test 1 to 6 by 10 times for reals
   call random_seed()
   call random (die_3, 1.0, 6.0)
   call random (die_4, 1.0, 6.0)
   print "(10f10.5)", die_3
   print "(10f10.5)", die_4
   pause
   
 end program test_random

