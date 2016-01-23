!  select_example.f90 
!
!  FUNCTIONS:
!  select_example - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: select_example
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

 program select_example

   !use select_mod
   use myfunctions
   implicit none

   ! Variables
   integer, parameter:: N = 5000000
   double precision, dimension(N)::a
   integer, parameter::k = 50
   double precision:: element
   logical::error
   integer:: start, stop1,counts_per_second
   
   
   
   call random_seed()
   call random_number(a)

   call system_clock(start, counts_per_second)
   
   call quick_select(a, k, element, error)

   ! copy data on image2 to image1 and merge
 
   call system_clock(stop1)
   call quick_sort(a)
   print *, "selecting time = ", (stop1-start)/counts_per_second, "second"
   print *, "selected value:", element, "input value: ", a(k), "if return error: ",error
   

   pause

 end program select_example

