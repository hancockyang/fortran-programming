!  integrate.f90 
!
!  FUNCTIONS:
!  integrate - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: integrate
!
!  PURPOSE:  1.Show the recursive function, interface (dummy function), and module usage in FORTRAN.
!            2.Trapezoid (1,2 area) and Simpson's approximation (in the modulu)
!
!****************************************************************************

 program integrate
   use function_module
   use integral_module
   use math_module

   implicit none
   
   double precision:: x_min, x_max, tolerance
   double precision:: answer
   
   x_min = -4.0d0
   x_max =  4.0d0
   tolerance = 1.0d-2
   ! function: f = exp(-x^2)
   ! the numerical integration has: 
   ! f: function
   ! x_min: starting point
   ! x_max: end point
   ! tolerance
   answer = integral_trap(f, x_min, x_max, tolerance)
   print "(a, f11.6)", "The trapzoid integral is approximately : ", answer
   answer = integral_simpson(f, x_min, x_max, tolerance)
   print "(a, f11.6)", "The simpson integral is approximately  : ", answer
   print "(a, f11.6)", "The exact answer is                    : ", sqrt(pi)
   pause

 ! Variables

 
 

 end program integrate

