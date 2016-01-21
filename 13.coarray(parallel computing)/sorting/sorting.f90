!  sorting.f90 
!
!  FUNCTIONS:
!   - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: hello
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

program sorting
   use sort_mod
   implicit none

   ! Variables
   integer, parameter:: N = 50000
   double precision, dimension(N), codimension[*]::a
   double precision, dimension(N):: b
   integer:: start, stop1,counts_per_second
   
   ! generate random numbers on image1
   if (this_image() == 1) then
      call random_seed()
      call random_number(a)
      b = a
      call system_clock(start, counts_per_second)
   endif
   sync all
   ! copy half to image2
!   if (this_image() == 2) then
!      a(N/4+1:N/2) = a(N/4+1:N/2)[1]
!   endif
!   if (this_image() == 3) then
!      a(N/2+1:3*N/4) = a(N/2+1:3*N/4)[1]
!   endif
!   if (this_image() == 4) then
!      a(3*N/4+1:) = a(3*N/4+1:)[1]
!   endif
   if (this_image() == 2) then
      a(N/3+1:2*N/3) = a(N/3+1:2*N/3)[1]
   endif
   if (this_image() == 3) then
      a(2*N/3+1:) = a(2*N/3+1:)[1]
   endif
   sync all
   
   ! sorting on image1 and image2
   select case (this_image())
      case(1)
         call interchange_sort(a(:N/3))
      case(2)
         call interchange_sort(a(N/3+1:2*N/3))
      case(3)
         call interchange_sort(a(2*N/3+1:))
      
   end select
   sync all

   ! copy data on image2 to image1 and merge
   if (this_image() == 1) then
      a(N/3+1:2*N/3) = a(N/3+1:2*N/3)[2]
      a(:2*N/3) = merge2(a(:N/3), a(N/3+1:2*N/3))
      a(2*N/3+1:) = a(2*N/3+1:)[3]
      a = merge2(a(:2*N/3), a(2*N/3+1:))
      call system_clock(stop1)
      print *, "For 3-image sort, time = ", (stop1-start)/counts_per_second, "second"
      print *, a(1), a(N), all(a(:N-1)<=a(2:))
      
   endif
   ! sort in 1 image
   if (this_image() == 1) then
      call system_clock(start)
      call interchange_sort(b(:N/2))
      call interchange_sort(b(N/2+1:))
      b = merge2(b(:N/2), b(N/2+1:))
      call system_clock(stop1)
      print *
      print *, "For 1-image sort, time = ", (stop1-start)/counts_per_second, "second"
      print *, b(1), b(N), all(b(:N-1)<=b(2:))
      
   endif
   sync all
   pause
   
end program sorting

