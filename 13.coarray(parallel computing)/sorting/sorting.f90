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
   ! codimension means the array (a) can be accessed by the other images
   double precision, dimension(N), codimension[*]::a
   double precision, dimension(N):: b
   real:: start, finish
   
   ! generate random numbers on image1
   if (this_image() == 1) then
      call random_seed()
      call random_number(a)
      b = a
      call cpu_time(start)
   endif
   ! need sync all beforce coarary
   sync all
   ! copy 1/3 to image2
   if (this_image() == 2) then
      ! a(N/3+1:2*N/3) on image1 is copied to image2
      a(N/3+1:2*N/3) = a(N/3+1:2*N/3)[1]
   endif
   ! copy 1/3 to image3
   if (this_image() == 3) then
      ! a(2*N/3+1:) on image1 is copied to image3
      a(2*N/3+1:) = a(2*N/3+1:)[1]
   endif
   ! need sync all after data transfer
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
    ! need sync all after sorting
   sync all

   ! copy data on image2 to image1 and merge
   if (this_image() == 1) then
      a(N/3+1:2*N/3) = a(N/3+1:2*N/3)[2]
      a(:2*N/3) = merge2(a(:N/3), a(N/3+1:2*N/3))
      a(2*N/3+1:) = a(2*N/3+1:)[3]
      a = merge2(a(:2*N/3), a(2*N/3+1:))
      call cpu_time(finish)
      print *, "For 3-image sort, time = ", finish-start, "second"
      print *, a(1), a(N), all(a(:N-1)<=a(2:))
      
   endif
   ! sort in 1 image
   if (this_image() == 1) then
      call cpu_time(start)
!      call interchange_sort(b(:N/3))
!      call interchange_sort(b(N/3+1:2*N/3))
!      call interchange_sort(b(2*N/3:))
!      b(:2*N/3) = merge2(b(:N/3), b(N/3+1:2*N/3))
!      b = merge2(b(:2*N/3), b(2*N/3+1:))
      call interchange_sort(b)
      call cpu_time(finish)
      print *
      print *, "For 1-image sort, time = ", finish-start, "second"
      print *, b(1), b(N), all(b(:N-1)<=b(2:))
      
   endif
   sync all
   pause
   
end program sorting

