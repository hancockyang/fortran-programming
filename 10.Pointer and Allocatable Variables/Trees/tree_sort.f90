!  tree_sort.f90 
!
!  FUNCTIONS:
!  tree_sort - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: tree_sort
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

program tree_sort

! Sorts a list of integers by building
! a tree, sorted in infix order.
! This sort has expected behavior n log n,
! but worst case (input is sorted) n ** 2.
   use tree_module
   implicit none
   ! Start with an empty tree
   type(tree_type), pointer :: tree
   integer :: number, ios, n
   integer, dimension(9), parameter :: numbers =[ 4, 6, 3, 8, 7, 9, 2, 1, 5 ]
   
   
   do n = 1, size(numbers)
      call insert(tree, numbers(n))
   end do
   call print_tree(tree)
   pause

end program tree_sort

