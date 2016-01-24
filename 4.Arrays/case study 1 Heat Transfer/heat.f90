!  heat.f90 
!
!  FUNCTIONS:
!  heat - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: heat
!
!  PURPOSE:  the plate consists of a 10 X 10 array of
!  points. A constant source of heat with value 1.0 is applied to the left edge (column 1) of
!  the plate and heat values 1.0, 0.9, 0.8, ..., 0.2, 0.1 are applied to the points at the top of
!  the plate. No heat is applied to the other two borders. We assume that the temperature
!  in the plate assumes a steady state when the temperature at each internal point is the
!  average of the temperatures of the four points neighboring the point—the points to the
!  north, east, west, and south. Thus, the program does an iterative calculation: at each
!  step the temperature at each internal point is replaced by the average of the four surrounding
!  points. This can be done as an array operation:
!                             temp = (n + e + s + w) / 4.0
!  The associate construct is used to give short names to some of the sections of the
!  two-dimensional array named plate. This makes it easier to understand that the main
!  computational step is averaging the points to the north, east, south, and west.
!  Note also the use of parameters tolerance and plate_format in the program. The
!  size of the plate is also a parameter P so that it can be changed easily..
!
!****************************************************************************

program heat

   implicit none
   integer, parameter :: P = 10
   double precision, dimension(P, P), target :: plate
   double precision, dimension(P-2, P-2) :: temp
   double precision, parameter :: tolerance = 1.0e-4
   character(len=*), parameter :: plate_format = "(10f5.2)"
   double precision :: diff
   integer :: i,j, niter
   ! Set up initial conditions
   plate = 0
   plate(:, 1) = 1.0 ! boundary values
   plate(1, :) = [ ( dble(j)/P, j = P, 1, -1 ) ]
   ! Alias parts of the plate
   associate (inside => plate(2:P-1, 2:P-1), n => plate(1:P-2, 2:P-1), s => plate(3:P, 2:P-1),e => plate(2:P-1, 1:P-2), w => plate(2:P-1, 3:P))
      ! Iterate
      niter = 0
      do
         temp = (n + e + s + w) / 4.0
         diff = maxval(abs(temp-inside))
         niter = niter + 1
         inside = temp
         print *, niter, diff
         if (diff < tolerance) exit
      end do
   end associate
   do i = 1, min(P, 10)
      print plate_format, plate(i, :)
   enddo
   pause
end program heat

