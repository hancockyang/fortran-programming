module tree_module
   implicit none
   public :: insert, print_tree
   type, public :: tree_type
      integer :: value1
      type(tree_type), pointer :: left, right
   end type tree_type
   
   
   contains
   
      recursive subroutine insert(tree, number)
         type(tree_type), pointer, intent(in out) :: tree
         integer, intent(in) :: number
         ! If (sub)tree is empty, put number at root
         if (.not. associated(tree)) then
            allocate (tree)
            tree%value1 = number
            nullify (tree%left)
            nullify (tree%right)
         ! Otherwise, insert into correct subtree
         else if (number < tree%value1) then
            call insert(tree%left, number)
         else
            call insert(tree%right, number)
         end if
      end subroutine insert
      
      recursive subroutine print_tree(tree)
         ! Print tree in infix order
         type(tree_type), pointer :: tree
         if (associated(tree)) then
            call print_tree(tree % left)
            print *, tree % value1
            call print_tree(tree % right)
         end if
      end subroutine print_tree
      
   end module tree_module