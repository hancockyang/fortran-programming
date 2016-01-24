!  dice_roll.f90 
!
!  FUNCTIONS:
!  dice_roll - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: dice_roll
!
!  PURPOSE:  2 dice rolls, the program to estimate the
!  probability of rolling 7 or 11 with two dice is a bit shorter than the scalar version. We
!  leave it to the reader to ponder whether it is easier or more difficult to understand than
!  the scalar version.
!
!****************************************************************************

program dice_roll
   use myfunctions, only: random_int
   implicit none
   integer, parameter :: number_of_rolls = 1000
   integer, dimension (number_of_rolls) :: dice, die_1, die_2
   integer :: wins
   call random_seed()
   call random_int (die_1, 1, 6)
   call random_int (die_2, 1, 6)
   dice = die_1 + die_2
   wins = count ((dice == 7) .or. (dice == 11))
   print "(a, f6.2)", "The percentage of rolls that are 7 or 11 is", 100.0 * real (wins) / real (number_of_rolls), "%"
   pause

end program dice_roll

