module matrix_mode
   type,public :: matrix_type
      integer:: rows, cols
      
      double precision, allocatable, dimension(:,:):: values
      
      contains
         procedure :: initial
      
   end type matrix_type
   
   contains
      subroutine initial(matrix)
         class(matrix_type),intent(inout):: matrix
         
         
         allocate(matrix%values(matrix%rows,matrix%cols))
         
         matrix%values = 0.0d0
         
      end subroutine initial
   
end module matrix_mode