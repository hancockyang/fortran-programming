module student_mod   
   implicit none
   
   
   ! address type, contains stree, city, state, zipcode
   type, public :: address_type
      character(len=30) :: number, street, city
      character(len=2) :: state
      character(len=30) :: zip_code
   end type address_type
   ! phone type, contains area_code and number
   type, public :: phone_type
       character(len=30) :: area_code, number
   end type phone_type
   
   ! grade type, contains math, english, physics, chemistry, history, average(optional)
   ! contains procedure mean to caculate the mean of the grade
   type, public :: grade_type
      real :: math, english, physics, chemistry, history
      real :: average = 0.0 
      contains
         procedure:: mean
   end type grade_type
   
  
   ! student type, contains name, address type, phone type, grade type and remarks    
   ! contains procedure report 
   type, public:: student
      character(len=40) :: name
      type(address_type)::address
      type(phone_type)::phone
      type(grade_type)::grade
      character(len=100) :: remarks
      contains
         procedure:: report
   end type student
    
   contains
      subroutine mean(output) 
         implicit none
         class(grade_type),intent(inout)::output       
         output%average = (output%math + output%english + output%physics + output%chemistry + output%history) / 5.0d0
      end subroutine mean

      subroutine report(output) 
         implicit none
         class(student),intent(in)::output 
         character(len=*), parameter :: write_format = "(5a12)",  convert_format = '(f11.2)'
         character(len=40) :: math,english,physics,chemistry,history 
         character ::str*1
         str = '|'
         write(math,convert_format) output%grade%math
         write(english,convert_format) output%grade%english
         write(physics,convert_format) output%grade%physics
         write(chemistry,convert_format) output%grade%chemistry
         write(history,convert_format) output%grade%history
         
         math      = trim(math)//str
         english   = trim(english)//str
         physics   = trim(physics)//str
         chemistry = trim(chemistry)//str
         history   = trim(history)//str
         
         print *,"name: "//output%name
         print *,"Address: "//trim(output%address%number )//" "//trim(output%address%street)//" "//trim(output%address%city)//", "//trim(output%address%state)//" "//trim(output%address%zip_code)
         print *,"Phone No: "//"("//trim(output%phone%area_code)//")"//trim(output%phone%number)
         print *,"------------------------------------------------------------------"
         print write_format,"Math|", "English|", "Physics|", "Chemistry|", "History|"
         print *,"------------------------------------------------------------------"
         print write_format,math,english,physics,chemistry,history
         print *,"------------------------------------------------------------------"
         print "(a,f10.2)","Average: ",output%grade%average
      end subroutine report
end module student_mod