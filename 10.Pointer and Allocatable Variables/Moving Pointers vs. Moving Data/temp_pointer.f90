!  temp_pointer.f90 
!
!  FUNCTIONS:
!  temp_pointer - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: temp_pointer
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

program temp_pointer

   implicit none
   integer, parameter :: P = 100
   double precision, dimension(:, :), pointer:: plate
   double precision, dimension(:, :), pointer :: temp_plate, n, s, e, w,inside
   double precision, dimension(:, :), pointer :: temporary
   double precision, dimension(P-2, P-2),target :: temp
   double precision, parameter :: tolerance = 1.0e-4
   character(len=*), parameter :: plate_format = "(100f5.2)"
   double precision :: diff
   integer :: i,j, niter
   ! cpu test setup
   real:: start, finish
   ! Set up initial conditions
   allocate(plate(P,P))
   plate = 0.0
   plate(:, 1) = 1.0 ! boundary values
   plate(1, :) = [ ( dble(j)/P, j = P, 1, -1 ) ]
   inside => plate(2:P-1, 2:P-1)
         n => plate(1:P-2, 2:P-1)
         s => plate(3:P, 2:P-1)
         e => plate(2:P-1, 1:P-2)
         w => plate(2:P-1, 3:P)
   allocate(temp_plate(P,P))
   temp_plate = 0.0
   temp_plate(:, 1) = 1.0 ! boundary values
   temp_plate(1, :) = [ ( dble(j)/P, j = P, 1, -1 ) ]
   allocate(temporary(P,P))
   temporary = 0.0
   temporary(:, 1) = 1.0 ! boundary values
   temporary(1, :) = [ ( dble(j)/P, j = P, 1, -1 ) ]

   !-----------
   call cpu_time(start)

      ! Iterate
      niter = 0
   do
      temp_plate(2:P-1, 2:P-1) = (n + e + s + w) / 4.0
      diff = maxval(abs(temp_plate(2:P-1, 2:P-1)-inside))
      niter = niter + 1     
      
      temporary => plate
      plate => temp_plate
      temp_plate => temporary
      inside => plate(2:P-1, 2:P-1)
      n => plate(1:P-2, 2:P-1)
      s => plate(3:P, 2:P-1)
      e => plate(2:P-1, 1:P-2)
      w => plate(2:P-1, 3:P)

      if (diff < tolerance) exit
   end do

   call cpu_time(finish)
   print *, "Moving pointer = ", finish-start, "second"   
   print plate_format, [(plate(i, :), i = 1, P)]
   
   pause

end program temp_pointer

