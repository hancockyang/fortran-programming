!  elsewhere_example.f90 
!
!  FUNCTIONS:
!  elsewhere_example - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: elsewhere_example
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

program elsewhere_example

   implicit none
   integer, parameter :: n = 9
   integer, dimension(n,n)::key
   integer:: i,j
   real,dimension(n,n)::a
   key = 0
   call random_number(a)
   do i = 1, n
      do j = 1, n
         if (i > j) then
            ! put negative numbers below the diagonal
            a(i,j) = -a(i,j) - 2.0
         else if (i < j) then
            ! put positive numbers above the diagonal
            a(i,j) = a(i,j) + 2.0
         else
            ! put the zeros on the diagonal
            a(i,j) = 0.0
         end if
      end do
   end do
   
   where(a > 0.0) 
      key = 1
   elsewhere (a < 0)
      key = -1
   elsewhere
      key = 0
   end where
      

   
   print "(9f5.1)", (a(i,:),i=1,9)
   print *
   print "(9i5.1)", (key(i,:), i = 1,9)
   pause
end program elsewhere_example

