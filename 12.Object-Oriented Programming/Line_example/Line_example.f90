!  Line_example.f90 
!
!  FUNCTIONS:
!  Line_example - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: Line_example
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

program Line_example
   
   use line_mod

   implicit none
   ! define polymorphic
   class(line_type), allocatable :: line
   class(vector_type1), allocatable :: line1
   class(vector_type2), allocatable :: line2   
   class(line_type), allocatable :: fancy_line
   ! use type
   type(vector_type1) :: line3
   type(painted_line_type) :: line4
! line_type
!--------------------------------------------------------   
!   line = line_type(1.1, 2.2, 4.4, 5.5) fortran 2008
   allocate (line, source=line_type(1.1, 2.2, 4.4, 5.5))
   ! it can call it inheritant
   print *, 'line type:'
   print *, line%x1
!-----------------------------------------------------------   
! the extend type   
   allocate (line1, source=vector_type1(1.1, 2.2, 4.4, 5.5, 2))
   print *, 'extend type:'
   print *, line1%x1
!-----------------------------------------------------------   
! the line is inheritant from this type
   allocate (line2, source=vector_type2(line,2))
   print *, 'inheritant type:'
   print *, line2%line%x1
!-----------------------------------------------------------   
! the extend extend type   
   allocate (fancy_line, source=fancy_line_type(0.0, 0.0, 0.0, 1.1, 0, 0, 0, 100))
   print *, 'extend extend type:'
   print *, fancy_line%x1
!------------------------------------------------------------
! use type
   line3 = vector_type1(1.1, 2.2, 4.4, 5.5, 2)
   print *, 'type:'
   print *, line3%x1

!--------------------------------------------------------------
! type bound procedure
   print *, "length"
   print *, line%length()
!-----------------------------------------------------------
! procedure pointer component
   line4%fp => add
   print*,line4%fp(2.1,2.1)
   pause

end program Line_example

