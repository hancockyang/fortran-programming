program integral
! calculates a trapezoidal approximation to an area using n trapezoids. n is read from the input file.
! The region is bounded by lines x = a, y = 0, x = b, and the curve y = sin(x). a and b also are read from the input file
   implicit none
   real:: a, b, h, total
   integer:: i, n
   print *, "Input data n:"
   read *, n
   print *, " n = ", n
   print *, "Input data a, b:"
   read *, a, b
   print *, "a = ", a
   print *, "b = ", b
   
   h = (b - a)/n
! caculate the total (f(a)/2 + f(a+h) + f(a+2h) + ... + f(b-h) + f(b)/2)*h
! Do the firts and last terms first
   total  = 0.5*(sin(a) + sin(b))
   do i = 1, n-1
      total  = total + sin(a + i*h)
   enddo
   
   print *, "Trapezoidal approximation to the area = ", h*total
   pause
end program integral