module select_mod
   
   implicit none
   public :: quick_select
   
contains
!-------------------------------------------------------   
   recursive subroutine quick_select(list, k, element, error)
      ! the algorithm is disscused in p148
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
!-----------------------------------------------------------------   
!   subroutine interchange_sort(list)
!      double precision, dimension(:), intent(inout):: list
!      integer:: i, j
!      double precision:: temp
!      
!      do i = 1, size(list) - 1
!         do j = i + 1, size(list)
!            if (list(i) > list(j)) then
!               temp = list(i)
!               list(i) = list(j)
!               list(j) = temp
!            endif
!         enddo
!      enddo
!   end subroutine interchange_sort
!------------------------------------------------------------  

end module select_mod