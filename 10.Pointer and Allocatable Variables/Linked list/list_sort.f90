!  list_sort.f90 
!
!  FUNCTIONS:
!  list_sort - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: list_sort
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

program list_sort

   use sorted_integer_lists_module
   implicit none
   
   type(sorted_list), pointer :: list => null()
   logical :: found
   integer :: n
   integer, dimension(9), parameter :: numbers = [ 4, 6, 3, 8, 7, 9, 2, 1, 5 ]
   
   do n = 1, size(numbers)
      call insert(list, numbers(n))
   end do
   print *, "Sorted list"
   call print_list(list)
   
   do n = 1, size(numbers)
      if (modulo(numbers(n), 2) /= 0) then
         call delete1(list, numbers(n), found)
         if (.not. found) then
            print *, numbers(n), "not found in list"
         end if
      end if
   end do
   
   print *; print *
   print *, "List with odd numbers deleted"
   call print_list(list)
   deallocate(list)
   print *; print *
   print *, "Is list empty?", is_empty(list)
   pause
end program list_sort

