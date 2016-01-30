module vehicle_module
   implicit none
   private
   
   type, public :: vehicle_type
      real :: weight
      integer :: number_of_wheels
      character(len=9) :: license
   end type vehicle_type
   
   type, public, extends(vehicle_type) :: car_type
      logical :: is_a_taxi
   end type car_type
   
   type, public, extends(vehicle_type) :: truck_type
      real :: capacity
   end type truck_type
   
   type, public, extends(vehicle_type) :: bus_type
      integer :: passengers
   end type bus_type
   
end module vehicle_module