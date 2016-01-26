!  pointer_example.f90 
!
!  FUNCTIONS:
!  pointer_example - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: pointer_example
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

program pointer_example

   implicit none
   real, target, dimension(4) :: a = [ 1, 2, 3, 4 ]
   real, pointer, dimension(:) :: p, q
   real, pointer, dimension(:, :) :: matrix
   real, pointer, dimension(:) :: diagonal, base
   real:: results
   !---------------Procedure pointers-----------------------------------
   interface
      function f(x,y) result(f_result)
         real, intent(in)::x,y
         real:: f_result
      end function f
   end interface
   procedure (f), pointer :: fff => null()
    !---------------arrays of procedure pointers-----------------------------------
   type :: proc_type
      procedure (f), pointer, nopass :: ptr_to_f => null()
   end type proc_type
   type (proc_type), dimension(:), allocatable :: ap
   integer:: n=3, i
   
   
   !----------------associat-------------------------------------
   p => a(1:3)
   q => a(2:4)
   ! point p and q are asscoiated
   print *, "if associated: "
   print *, associated(p, q)
   print *
   !---------------------pointer remapping---------------------
   allocate (base(n*n))
   base = [2.3, 3.6 , 5.2, 6.3, 9.0, 8.5, 6.5, 1.2, 3.7]
   matrix(1:n, 1:n) => base
   ! diagonal
   diagonal => base(::n+1) ! start from 1(:) to the end(:) by increment n+1
   print *, "Diagonal: "
   print "(3f5.1)", diagonal
   print *, "Matrix: "
   print "(3f5.1)", [ (matrix(i,:),i=1,n) ]
   print *
   
   !---------------Procedure pointers-----------------------------------
   fff => add
   results = fff(0.24,3.0)
   print *, 'printing 0.24 + 3.0'
   print *, 'using procedure pointer:', results ! prints cos(0.24)
   print *, 'using intrisc function :', 0.24 + 3.0
   print *

   !---------------arrays of procedure pointers-----------------------------------
   allocate (ap(n))
   ap(1)%ptr_to_f => add
   ap(2)%ptr_to_f => sub
   ap(3)%ptr_to_f => times
   print *, 'printing 0.24 + 3.0'
   print *, 'using procedure pointer:', ap(1)%ptr_to_f(0.24,3.0) 
   print *, 'printing 0.24 - 3.0'
   print *, 'using procedure pointer:', ap(2)%ptr_to_f(0.24,3.0)
   print *, 'printing 0.24 * 3.0'
   print *, 'using procedure pointer:', ap(3)%ptr_to_f(0.24,3.0) 
   pause
   
   
   
   contains
      function add(a,b) result(c)
         real, intent(in):: a,b
         real:: c
         c = a + b
      end function add
      
      function sub(a,b) result(c)
         real, intent(in):: a,b
         real:: c
         c = a - b
      end function sub
      
      function times(a,b) result(c)
         real, intent(in):: a,b
         real:: c
         c = a * b
      end function times
end program pointer_example

