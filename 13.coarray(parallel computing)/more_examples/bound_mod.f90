module bound_mod
   implicit none
   private
   type, public :: t_type
      procedure(s1), pointer, nopass :: p
      contains
         procedure, nopass :: s2
   end type t_type
   public :: s1, s2
   contains
   
      subroutine s1()
         print *, 1.1
      end subroutine s1
      
      subroutine s2()
         print *, 2.2
      end subroutine s2
end module bound_mod