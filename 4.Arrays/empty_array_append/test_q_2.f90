!  test_q.f90 
!
!  FUNCTIONS:
!  test_q - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: test_q
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

program test_q
   implicit none
   integer, dimension(*), parameter::  a=[1 ,3,5,2, 7,10]
   integer ::  i

   integer, dimension(:), allocatable :: array
   array = [integer :: ]
   do i = 1, 6
      if( a(i) < 6 ) array = [array, a(i)]
   enddo
   print *, 'hello world'
   print *, array
end program test_q

