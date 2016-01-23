!  sorting_example.f90 
!
!  FUNCTIONS:
!  sorting_example - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: sorting_example
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

program sorting_example

   use myfunctions
   implicit none

   ! Variables
   integer, parameter:: N = 5000000
   double precision, dimension(N)::a
   integer:: start, stop1,counts_per_second
   
   
   
   call random_seed()
   call random_number(a)

   call system_clock(start, counts_per_second)
   
   call quick_sort(a)

   ! copy data on image2 to image1 and merge
 
   call system_clock(stop1)
   print *, "sorting time = ", (stop1-start)/counts_per_second, "second"
   print *, "first element: ", a(1), "second element:", a(N), "if sorted: ",all(a(:N-1)<=a(2:))
   

   pause

end program sorting_example

