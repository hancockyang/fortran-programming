!  test_q.f90 
!
!  FUNCTIONS:
!  test_q - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: test_q
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

program test_q

   use vehicle_module
   use v_q_module
   implicit none
   class(vehicle_type), allocatable :: v
   type(q_type) :: q
   logical :: f
   call q%empty()
   print *, "Is Q empty?", q%is_empty()
   allocate (v, source=car_type(2000.0, 4, "C-1455", .false.))
   print *, "Inserting car C-1455"
   call q%insert(v)
   deallocate (v)
   print *, "Is Q empty?", q%is_empty()
   print *, "Printing Q:"
   call q%print_licenses()
   print *
   allocate (v, source=bus_type(9000.0, 6, "B-6700", 70))
   print *, "Inserting bus B-6700"
   call q%insert(v)
   deallocate (v)
   allocate (v, source=truck_type(9000.0, 18, "T-8800", 20000.00))
   print *, "Inserting truck T-8800"
   call q%insert(v)
   deallocate (v)
   allocate (v, source=bus_type(8000.0, 6, "B-6701", 70))
   print *, "Inserting bus B-6701"
   call q%insert(v)
   deallocate (v)
   print *, "Printing Q:"
   call q%print_licenses()
   print *
   print *, "Removing first vehicle in Q:"
   call q%remove(v, f)
   print *, "Found:", f, trim(v%license)
   print *, "Printing Q:"
   call q%print_licenses()
   print *
   print *, "Removing all vehicles from Q:"
   call q%empty()
   print *, "Printing Q:"
   call q%print_licenses()
   call q%remove(v, f)
   print *,f
end program test_q

