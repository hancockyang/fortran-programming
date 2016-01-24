module line_mod
   implicit none
   private
   ! line connected to
   type, public :: line
      real :: x1, y1, x2, y2
   ! contains function length   
      contains
         procedure :: length
   end type line
   
   interface
   ! function defined in module
      module function length (l)
      
         class(line), intent(in) :: l
         real :: length
         
      end function length
      
   end interface
   
end module line_mod

