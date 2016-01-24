!  student_record.f90 
!
!  FUNCTIONS:
!  student_record - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: student_record
!
!  PURPOSE:  demo of derived type and output formatting.
!
!****************************************************************************

program student_record
   use student_mod
   implicit none
   type (student):: joan
  
   
   joan%phone   = phone_type("505", "2750800")
   joan%address = address_type("360","Huntington Ave", "Boston", "MA", "02115")
   joan%grade   = grade_type( 97.0, 98.0, 55.0, 69.0, 75.0) 
   call joan%grade%mean()
   joan%name = "Joan Doe"
   call joan%report()
     
   pause

end program student_record

