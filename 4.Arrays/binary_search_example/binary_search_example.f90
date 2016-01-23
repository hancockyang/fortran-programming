!  binary_search_example.f90 
!
!  FUNCTIONS:
!  binary_search_example - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: binary_search_example
!
!  PURPOSE: compare the efficient of binary search to the brute force search.
!
!****************************************************************************

program binary_search_example

 !use select_mod
   use myfunctions, only: quick_sort, binary_search, brute_force_search
   implicit none

   ! Variables
   integer, parameter:: N = 500000
   double precision, dimension(N)::a
   integer, parameter::k = 500000
   double precision:: element
   logical::found
   integer:: index
   real:: start, finish
   ! out of the list range to test if search function is robust
   element = 2.0d0
   
   call random_seed()
   call random_number(a)
   ! sort first
   call quick_sort(a)
   ! binary search
   call cpu_time(start)
   call binary_search(a, element, index, found)
   call cpu_time(finish)
   print *, "binary search time = ", finish-start, "second"
   print *, "search value:", element, "input index: ", k, "if found: ",found
   print *
   
   ! brute force search
   call cpu_time(start)
   call brute_force_search(a, element, index, found)
   call cpu_time(finish)
   print *, "brute force search time = ", finish-start, "second"
   print *, "search value:", element, "input index: ", k, "if found: ",found
   pause
end program binary_search_example

