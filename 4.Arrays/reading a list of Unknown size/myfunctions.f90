module myfunctions
   implicit none
   
   double precision :: pi  = 4.0d0*ATAN(1.0d0)
   private
   public :: integral_trap, integral_simpson, append
   contains
   !------------------------------------------Numerical integral---------------------------------
   !---------------------------------------------------------------------------------------------
      !-------------------------------------trapzoid--------------------------------------------
      recursive function integral_trap(f, a, b, tolerance) result(integral_result)
      ! divide the trapzoid area to two parts and evaluate the accuracy
      ! f: function
      ! a: starting point
      ! b: end point
         intrinsic :: abs
         interface
            function f(x) result(f_result)
               double precision, intent(in)::x
               double precision:: f_result
            end function f
         end interface
         double precision, intent(in):: a, b, tolerance
         double precision:: integral_result
         double precision:: h, mid
         double precision:: one_trapezoid_area, two_trapezoid_area
         double precision:: left_area, right_area
         
         h = b - a
         mid = (a + b)/2.0d0
         one_trapezoid_area = h*(f(a) + f(b)) / 2.0d0
         two_trapezoid_area = h/2.0d0*(f(a) + f(mid)) / 2.0d0 + h/2.0d0*(f(mid) + f(b)) / 2.0d0
         ! the error analysis is shown in page 113-114
         if (abs(one_trapezoid_area - two_trapezoid_area) < 3.0d0*tolerance) then
            integral_result = two_trapezoid_area
         else
            left_area = integral_trap(f, a, mid, tolerance/2.0d0)
            right_area = integral_trap(f, mid, b, tolerance/2.0d0)
            integral_result = left_area + right_area
         end if
      end function integral_trap
      !-------------------------------------simpson---------------------------------------------
      recursive function integral_simpson(f, a, b, tolerance) result(integral_result)
         intrinsic :: abs
         interface
            function f(x) result(f_result)
               double precision, intent(in)::x
               double precision:: f_result
            end function f
         end interface
         double precision, intent(in):: a, b, tolerance
         double precision:: integral_result
         double precision:: h, mid, mid1, mid2
         double precision:: one_simpson_area, two_simpson_area
         double precision:: left_area, right_area
         
         h = (b - a)/2.0d0
         mid = (a + b)/2.0d0
         one_simpson_area = h*(f(a) + 4.0d0*f(mid) + f(b)) / 3.0d0
         
         mid1 = (a + mid)/2.0d0
         mid2 = (mid + b)/2.0d0
         two_simpson_area = h*(f(a) + 4.0d0*f(mid1) + f(mid)) / 6.0d0 + h*(f(mid) + 4.0d0*f(mid2) + f(b)) / 6.0d0
         
         if (abs(one_simpson_area - two_simpson_area) < 15.0d0*tolerance) then
            integral_result = two_simpson_area
         else
            left_area = integral_simpson(f, a, mid, tolerance/2)
            right_area = integral_simpson(f, mid, b, tolerance/2)
            integral_result = left_area + right_area
         end if
      end function integral_simpson
   !------------------------------------------end of numerical integral-------------------------------------
   !------------------------------------------append subroutin, do the same as in python--------------------
   subroutine append(list, element)

      IMPLICIT NONE

      integer :: i, isize
      double precision, intent(in) :: element
      double precision, dimension(:), allocatable, intent(inout) :: list
      double precision, dimension(:), allocatable :: clist


       if(allocated(list)) then
           isize = size(list)
           allocate(clist(isize+1))
           clist(1:isize) = list(1:isize)
           clist(isize+1) = element
           deallocate(list)
           call move_alloc(clist, list)

       else
           allocate(list(1))
           list(1) = element
       end if


   end subroutine append
   !--------------------------------------------end of append subroutine---------------------------------------
end module myfunctions