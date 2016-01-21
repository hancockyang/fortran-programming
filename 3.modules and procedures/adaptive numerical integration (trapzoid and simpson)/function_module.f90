module function_module
   implicit none
   private
   public::f
   
   contains
      function f(x) result(f_result)
         double precision, intent(in):: x
         double precision:: f_result
         
         f_result = exp(-x**2)
      end function f
end module function_module