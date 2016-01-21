module sort_mod
   
   implicit none
   public :: quick_sort, interchange_sort
   
contains
!-------------------------------------------------------   
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
end module sort_mod