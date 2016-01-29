module line_mod

   type, public :: line_type
      real :: x1, y1, x2, y2
   ! type-bound procedures
   contains
      procedure, public :: length
   end type line_type
!---------------------inherited from line_type-----------------------
   type, public, extends(line_type) :: painted_line_type
      !integer :: r, g, b ! Values each 0-100
      ! Procedure Pointer Components
      procedure (f), pointer, nopass :: fp => null()
   end type painted_line_type

   type, public, extends(line_type) :: vector_type1
      integer :: direction ! 0 not directed, 1 toward (x1, y1) or 2
   ! Procedure Pointer Components
      !procedure (f), pointer, nopass :: fp => null()
   end type vector_type1
!---------------------------------------------------------------------
!-----------------------------inherit line_type-----------------------
   type, public :: vector_type2
      type(line_type) :: line
      integer :: direction
      !procedure (f), pointer, nopass :: fp => null()
   end type vector_type2
!------------------------------extend vector_type1------------------
   type, public, extends(vector_type1) :: fancy_line_type
      integer :: r, g, b
      !procedure (f), pointer, nopass :: fp => null()
   end type fancy_line_type
!-----------------------------------------------------------
   interface
      function f(x,y) result(f_result)
         real, intent(in)::x,y
         real:: f_result
      end function f
   end interface
   
   contains
      function length(ab) result(length_result)
         class(line_type), intent(in) :: ab
         length_result = sqrt( (ab%x1-ab%x2)**2 + (ab%y1-ab%y2)**2 )
      end function length
      
      function add(a,b) result(c)
         real, intent(in):: a,b
         real:: c
         c = a + b
      end function add
end module line_mod