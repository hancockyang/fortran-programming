module myfunctions
   implicit none
   
   double precision :: pi  = 4.0d0*ATAN(1.0d0)
   private
   public :: integral_trap, integral_simpson, append, quick_sort, merge2
   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%PUBLIC FUNCTIONS or SUBROUTINEs%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   ! 1. integral_trap(f, a, b, tolerance)    : function, trapzoid numberical integration, double precision
   !  f        : function
   !  a,b      : lower limit and upper limit
   !  tolerance: the error tolerance
   !---------------------------------------------------------------------------------------------------------------------------------------------------------------------
   ! 2. integral_simpson(f, a, b, tolerance) : function, simpson numberical integration, double precision
   !  f        : function
   !  a,b      : lower limit and upper limit
   !  tolerance: the error tolerance
   !---------------------------------------------------------------------------------------------------------------------------------------------------------------------
   ! 3. append(list, element)                : subroutine, append element to the end of the list, double precision
   !  list     : list to be appended
   !  element  : appedn to the list 
   !----------------------------------------------------------------------------------------------------------------------------------------------------------------------
   ! 4. quick_sort(list)                     : soubroutine, sorting a list. small list uses interchage function, large uses recursive sorting, double precision
   !  list     : list to be sorted
   !----------------------------------------------------------------------------------------------------------------------------------------------------------------------
   ! 5. merge2(a,b)                          : function, merge two list together, double precision
   !  a, b     : lists to be merged
   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
   contains
   !*************************************Numerical integral****************************************
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
   !*************************************************end of numerical integral**********************************************
   !**********************************************append subroutin, do the same as in python********************************
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
   !*************************************end of append subroutine*******************************
   !**************************************sorting function*************************************
   recursive subroutine quick_sort(list)

      double precision, dimension(:), intent(inout):: list
      
      integer:: i, j, n
      double precision:: chosen, temp
      integer, parameter:: max_simple_sort_size = 6
      
      n = size(list)
      if (n <= max_simple_sort_size) then
         ! use interchange sort for small lists
         call interchange_sort(list)
      else
         ! use partition ("quick") sort
         chosen = list(n/2)
         
         i = 0
         j = n + 1
         
         do
            ! scan list from left end
            ! until element >= chosen is found
            do
               i = i + 1
               if(list(i) >= chosen) exit
            enddo
            ! scan list from right end
            ! until element <= chosen is found
            do
               j = j - 1
               if (list(j) <= chosen) exit
            enddo
            if (i < j) then
               ! swap two out of place elements
               temp = list(i)
               list(i) = list(j)
               list(j) = temp
            else if (i == j) then
               i = i + 1
               exit
            else
               exit
            endif
         enddo
         
         if (1 < j) call quick_sort(list(:j))
         if (i < n) call quick_sort(list(i:))
      endif ! test for small array
   end subroutine quick_sort
!-----------------------------------------------------------------   
   subroutine interchange_sort(list)
      double precision, dimension(:), intent(inout):: list
      integer:: i, j
      double precision:: temp
      
      do i = 1, size(list) - 1
         do j = i + 1, size(list)
            if (list(i) > list(j)) then
               temp = list(i)
               list(i) = list(j)
               list(j) = temp
            endif
         enddo
      enddo
   end subroutine interchange_sort
!------------------------------------------------------------  
! Here is the function that merges two sorted arrays. At each point in the merging process,
! the first elements of each sorted list are compared and the smaller one is selected for 
! inclusion as the next element in the merged list. The process is made a little more complicated
! by handling the merge after one of the lists is exhausted. 
   function merge2(a,b) result(m)
      
      double precision, dimension(:), intent(in):: a, b
      double precision, dimension(size(a) + size(b)) :: m
      integer:: ka, kb, km
      
      ka = 1; kb = 1; km = 1
      
      do
         if (ka > size(a)) then
            m(km:) = b(kb:)
            return
         else if (kb > size(b)) then
            m(km:) = a(ka:)
            return
         else if (a(ka) < b(kb)) then
            m(km) = a(ka)
            km = km + 1; ka = ka + 1
         else
            m(km) = b(kb)
            km = km + 1; kb = kb + 1
         endif
      enddo
   end function merge2
end module myfunctions