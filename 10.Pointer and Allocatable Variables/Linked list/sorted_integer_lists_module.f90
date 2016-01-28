module sorted_integer_lists_module
   implicit none
   private
   
   type, public :: sorted_list
      private
      integer :: value1
      type(sorted_list), pointer :: next => null()
      contains
      ! the final subroutine will be excuted automatically and we don't need to call the function
         final:: empty
   end type sorted_list
   
   public:: is_empty, insert, delete1, print_list
   
   
   
   contains
      recursive subroutine insert(list, number)
      
         type(sorted_list), pointer, intent(in out) :: list
         
         integer, intent(in) :: number
         type(sorted_list), pointer :: temp
         
         if (is_empty(list)) then
            allocate (list)
            list%value1 = number
         ! the inserted number is less, then temp is alias of current cell
         ! create a new cell by allocate(cell)
         ! the cell is filled by number and pointer: temp pointer to the cell has value larger than number (next)
         else if (number <= list%value1) then
            temp => list
            allocate (list)
            list = sorted_list(number, temp)
         else
         ! the inserted number is greater, then call the recursive
            call insert(list%next, number)
         end if
      end subroutine insert
      
      function is_empty(list) result(is_empty_result)
         type(sorted_list), pointer, intent(in) :: list
         
         logical :: is_empty_result
         ! test if the list is associated with any other data
         is_empty_result = .not. associated(list)
      end function is_empty
      
      recursive subroutine delete1(list, number, found)
         type(sorted_list), pointer, intent(in out) :: list
         integer, intent(in) :: number
         logical, intent(out) :: found
         type(sorted_list), pointer :: temp
         
         if (is_empty(list)) then
            found = .false.
         else if (list%value1 == number) then
            ! Delete node pointed to by list, this list is the pointer of previous list%next pointer
            temp => list
            list => list%next
            temp%next => null()
            deallocate(temp)
            found = .true.
         
         else
         ! if it is not the desired number call the recursive
            call delete1(list%next, number, found)
         end if
      end subroutine delete1
      
      recursive subroutine print_list(list)
         type(sorted_list), pointer, intent(in) :: list
         if (associated(list)) then
            write (unit=*, fmt="(tr1, i0)", advance="no") list%value1
            call print_list(list%next)
         end if
      end subroutine print_list
      
      recursive subroutine empty(list)
         type(sorted_list), intent(in out) :: list
         if (associated(list%next)) then
            deallocate (list%next)
         end if
      end subroutine empty
end module sorted_integer_lists_module