module myfunctions
   implicit none
   
   double precision :: pi  = 4.0d0*ATAN(1.0d0)
   
   public :: integral_trap, integral_simpson, append, quick_sort, merge2, quick_select, binary_search, brute_force_search, random, linspace
   private :: random_int,random_real
   
   interface random
      module procedure random_int, random_real
   end interface
   
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
   ! 3. append(list, element)                          : subroutine, append element to the end of the list, double precision
   !  list     : list to be appended
   !  element  : appedn to the list 
   !----------------------------------------------------------------------------------------------------------------------------------------------------------------------
   ! 4. quick_sort(list)                               : soubroutine, sorting a list. small list uses interchage function, large uses recursive sorting, double precision
   !  list     : list to be sorted
   !----------------------------------------------------------------------------------------------------------------------------------------------------------------------
   ! 5. merge2(a,b)                                    : function, merge two list together, double precision
   !  a, b     : lists to be merged
   !----------------------------------------------------------------------------------------------------------------------------------------------------------------------
   ! 6. quick_select(list, k, element, error)          : subroutine, select the k-th smallest element from list without sorting it, double precision
   !  list     : list to be selected from
   !  k        : k-th smallest
   !  element  : returned value 
   !  error    : F for no error return
   !----------------------------------------------------------------------------------------------------------------------------------------------------------------------
   ! 7. binary_search(list, element, index, found)     : subroutine, binary search  element from list after sorting it, double precision
   !  list     : list to be searched from
   !  element  : value we if in the list 
   !  index    : index of the 
   !  found    : if the elment in the list
   !----------------------------------------------------------------------------------------------------------------------------------------------------------------------
   ! 8. brute_force_search(list, element, index, found): subroutine, brute force search  element from list after sorting it, double precision
   !  list     : list to be searched from
   !  element  : value we if in the list 
   !  index    : index of the 
   !  found    : if the elment in the list
   !----------------------------------------------------------------------------------------------------------------------------------------------------------------------
   ! 9. random_int(output, low, high)                   : subroutine, run random test size(output) times. earch time has the result in the range from low to high, integer
   !  output   : result list
   !  low      : lower limit
   !  high     : upper limit
   !----------------------------------------------------------------------------------------------------------------------------------------------------------------------
   !10. random_real(output, low, high)                  : subroutine, run random test size(output) times. earch time has the result in the range from low to high, real
   !  output   : result list
   !  low      : lower limit
   !  high     : upper limit
   !----------------------------------------------------------------------------------------------------------------------------------------------------------------------
   ! 9 and 10 are generized to random function (integer and real)
   !----------------------------------------------------------------------------------------------------------------------------------------------------------------------
   !11. inspace(x, x_start, x_end, x_len)               : subroutine, similar to matlab inspace function, divide a range from x_start to x_end by x_len pieces, double precision
   !  x        : returned array
   !  x_start  : starting point
   !  x_end    : end point
   !  x_len    : segment number
   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
   contains
   !*************************************Numerical integral****************************************
   !---------------------------------------------------------------------------------------------
      !-------------------------------------trapzoid--------------------------------------------
      recursive function integral_trap(f, a, b, tolerance) result(integral_result)
   !  Mathematicians tell us that the error E(h) in approximating the area of the almost
   !  rectangular region with top boundary y = f(x) by the area of one trapezoid is approximately
   !  -1/12f''(c)h3, where h is the width of the trapezoid, and c is some x value in the
   !  interval, whose exact location may not be known, but which matters little because for
   !  reasonable functions f’’(x) varies little over a small interval of width h. The dependence
   !  of E(h) on h^3 shows why the error drops rapidly as h decreases, and the dependence of
   !  E(h) on f’’(c) shows why the error is smaller when f??(x) is smaller, at places such as
   !  near inflection points (where the tangent line crosses the curve) where f’’(x) = 0. If the
   !  same region is approximated by the sum of the areas of two trapezoids, each of width
   !  h/2, the error in each of them is approximately -1/12f’’(c1)(h/2)^3, or 1/8E(h), if we assume
   !  f’’(x) changes little over such a small interval so that f’’(c) ~ f’’(c1). Since there are
   !  two trapezoids, the total error E(h/2) is approximately E(h)/4. If T(h) and T(h/2) are the
   !  two trapezoidal approximations and I is the exact integral, we have approximately
   !     T(h/2) = (I - E(h/2) - (I - E(h)) = – E(h/2) + E(h) = E(h/2) + 4*E(h/2)
   !            = 3*E(h/2)
   ! This formula provides a way to check whether the trapezoidal approximations are better
   ! than a specified error tolerance. Since
   !     E(h/2) = |1/3*T(h/2)– T(h)|
   ! approximately, the two-trapezoid approximation is sufficiently accurate if
   !     1/3*|T(h/2)– T(h)| < tolerance
   ! If not, then the error tolerance is split in two, and the adaptive trapezoidal function integral
   ! is called again to approximate the area of each half of the region to within half
   ! of the original error tolerance. Thus, only regions where the approximation error is still
   ! large are further subdivided.
         IMPLICIT NONE
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
      ! same as trapzoid integral, only the error is -90/90*f''''(c)*h^5
         IMPLICIT NONE
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
   ! allocate a new list as the element append to the ord list, and create temp list (clist) to pass the ord array to the newly
   ! created array
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
   ! As might be expected, the quick sort is a bit more complicated. It is a divide-andconquer
   ! algorithm like binary search. To sort a list of numbers, an arbitrary number
   ! (such as the first, last, or middle one) is chosen from the list. All the remaining numbers
   ! in turn are compared with the chosen number; the ones smaller are collected in a
   ! “smaller” set and the ones larger are collected in a “larger” set. The whole list is sorted
   ! by sorting the “smaller” set, following them with all numbers equal to the chosen
   ! number, and following them with the sorted list of “larger” numbers. Note that sorting
   ! the “smaller” and “larger” lists involves using the quick sort routine recursively
      IMPLICIT NONE
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
   ! One of the simplest ways to sort this is to compare every number in the list with every
   ! other number in the list and swap them if they are out of order. As with the previous
   ! examples in this chapter, the sorting is done with a subroutine so that it can be put in
   ! a module and be used by many programs.
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
!********************************end of sort function*******************************************
!**********************************merge2 function************************************************
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
   !********************************************end of merg2 function***************************
   !***********************************************quick_select subroutine***********************
   recursive subroutine quick_select(list, k, element, error)
   ! A common problem is to find the median of a list of numbers, that is, the one that
   ! would be in the middle of the list if the list were in order. One way to do this is to sort
   ! the list and look at the element in the middle, but this is quite inefficient. The best sorting
   ! algorithms require nlog2(n) steps to sort n numbers, whereas the median of n numbers
   ! can be found in n steps
   ! A good algorithm to select the kth element is similar to the quick-sort algorithm.
   ! Arbitrarily pick one of the numbers in the list. As with the quick sort, separate the
   ! numbers into three collections: the numbers smaller than the chosen number, the numbers
   ! equal to the chosen number, and the numbers larger than the chosen number.
   ! Suppose the size of each of these collections is s, e, and l, respectively. If k d s, the number
   ! we are looking for is in the collection of smaller numbers, and, in fact, is the kth
   ! number in that collection in order; this number can be found by applying the same selection
   ! algorithm recursively to the list of smaller numbers. If s k ds+e, then the number
   ! chosen is the one we are looking for and the search is complete. If s + e < k, the
   ! number we are looking for is in the collection of larger numbers; it is, in fact, the one in
   ! position k -s -e in that list in order, so it can be found by recursively calling the selection
   ! procedure.

      double precision, dimension(:), intent(in):: list
      integer, intent(in):: k
      double precision, intent(out):: element
      logical, intent(out):: error
      double precision, dimension(:), allocatable:: smaller, larger
      integer:: i, n, number_smaller, number_equal, number_larger
      double precision:: chosen
      !integer, parameter:: max_simple_sort_size = 6
      
      n = size(list)
      if (n <= 1) then
         error = .not.  (n == 1 .and. k == 1)
         if (error) then
            element = 0.0d0
         else
            element = list(1)
         end if
      else
         allocate (smaller(n), larger(n))
         chosen = list(1)
         number_smaller = 0
         number_equal   = 1
         number_larger  = 0
         
         do i = 2, n
            if (list(i) < chosen) then
               number_smaller = number_smaller + 1
               smaller(number_smaller) = list(i)
            else if (list(i) == chosen) then
               number_equal = number_equal + 1
            else
               number_larger = number_larger + 1
               larger(number_larger) = list(i)
            end if
         end do
         
         if (k <= number_smaller) then
            call quick_select(smaller(1:number_smaller), k, element, error)
         else if (k <= number_smaller + number_equal) then
            element = chosen
            error = .false.
         else
            call quick_select(larger(1:number_larger), k - number_smaller - number_equal, element, error)
         end if
         
         deallocate(smaller, larger)
      endif ! test for small array
   end subroutine quick_select

!*********************************************end of quick_select subroutine****************************
!***********************************************binary_search subroutine********************************

   subroutine binary_search(list, element, index, found)
   ! If the search must be performed on a traditional computer by making one comparison
   ! at a time, the search can be made more efficient by maintaining the list in the order
   ! of increasing card number. As soon as one canceled card number examined in the
   ! search is too large, all subsequent ones will also be too large, so the search can be abandoned
   ! early
      implicit none
      double precision, dimension(:), intent(in):: list
      double precision, intent(in):: element
      integer, intent(out):: index
      logical, intent(out):: found
      integer:: first, half, last
      
      first = 1
      last  = size(list)
      do
         if (first == last) exit
         half = (first + last)/2
         if (element <= list(half)) then
            last = half
         else
            first = half + 1
         end if
      enddo
      index = first
      found = (element == list(index))
   end subroutine binary_search
!***********************************************end of binary_search subroutine********************************
!*******************************************brute_force_search subroutine********************************
   subroutine brute_force_search(list, element, index, found)
      implicit none
      double precision, dimension(:), intent(in):: list
      double precision, intent(in):: element
      integer, intent(out):: index
      logical, intent(out):: found
      integer::  last, i
      
     
      last  = size(list)
      do i = 1, last
         if (element == list(i)) then
            index = i
            exit
         end if
      enddo
      
      found = (element == list(index))
   end subroutine brute_force_search
!***********************************************end of brute_force_search subroutine********************************
!***********************************************random_int subroutine***********************************************
   subroutine random_int(output, low, high)
   ! run random test size(output) times. earch time has the result in the range from low to high
      implicit none
      integer, dimension(:), intent(out) :: output
      integer, intent(in) :: low, high
      double precision, dimension(:), allocatable :: uniform_random_value
      
      allocate (uniform_random_value(size(output)))
      ! this gives 0 to 1 random numbers
      call random_number(uniform_random_value)
      ! this scale it to [low, high]
      output = int((high - low + 1) * uniform_random_value + low)
      deallocate (uniform_random_value)
   end subroutine random_int
!***********************************************end of random_int subroutine***********************************************
!***********************************************random_real subroutine***********************************************
   subroutine random_real(output, low, high)
   ! run random test size(output) times. earch time has the result in the range from low to high
      implicit none
      real, dimension(:), intent(out) :: output
      real, intent(in) :: low, high
      real, dimension(:), allocatable :: uniform_random_value
      
      allocate (uniform_random_value(size(output)))
      ! this gives 0 to 1 random numbers
      call random_number(uniform_random_value)
      ! this scale it to [low, high]
      output = ((high - low + 1) * uniform_random_value + low)
      deallocate (uniform_random_value)
   end subroutine random_real
!***********************************************end of random_int subroutine***********************************************
!***********************************************linspace subroutine***********************************************
   subroutine linspace(x, x_start, x_end, x_len)
      integer:: x_len, i
      double precision, dimension(x_len),intent(inout)::x
      double precision:: x_start, x_end, dx
      dx = (x_end - x_start) / (x_len - 1)
      x(1:x_len) = [(x_start + ((i - 1)*dx), i = 1, x_len)]
   end subroutine linspace
!***********************************************end of linspace subroutine***********************************************
end module myfunctions