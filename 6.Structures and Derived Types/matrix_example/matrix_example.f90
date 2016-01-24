!  matrix_example.f90 
!
!  FUNCTIONS:
!  matrix_example - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: matrix_example
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

program matrix_example
   use matrix_mode
   implicit none
   integer:: rows = 3, cols = 5, i
   type(matrix_type):: matrix
   matrix%rows = rows 
   matrix%cols = cols
   call matrix%initial()
   print *, shape(matrix%values)
   print "(5f5.2)", [(matrix%values(i,:), i = 1, rows)]
   pause


end program matrix_example

