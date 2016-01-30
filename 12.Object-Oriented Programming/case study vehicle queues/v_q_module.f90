module v_q_module
   use vehicle_module
   implicit none
   private
   
   type :: node_type
      class(vehicle_type), allocatable :: v
   end type node_type
   
   type, public :: q_type
      private
      type(node_type), dimension(:), allocatable :: vehicles
      
      contains
         procedure :: empty
         procedure :: is_empty
         procedure :: insert
         procedure :: remove
         procedure :: print_licenses
   end type q_type
   
   contains
      subroutine empty(q)
         class(q_type), intent(out) :: q
         q%vehicles = [ node_type:: ]
      end subroutine empty
      
      function is_empty(q) result(is_empty_result)
         class(q_type), intent(in) :: q
         logical :: is_empty_result
         is_empty_result = (size(q%vehicles) == 0)
      end function is_empty
      
      subroutine insert(q, dv)
         class(q_type), intent(in out) :: q
         class(vehicle_type), intent(in), allocatable :: dv
         q%vehicles = [ q%vehicles, node_type(dv) ]
      end subroutine insert
      
      subroutine remove(q, first, found)
         class(q_type), intent(in out) :: q
         type(vehicle_type), allocatable, intent(out) :: first
         logical, intent(out) :: found
         found = .not. is_empty(q)
         if (.not. found) return ! Q is empty
         first = q%vehicles(1)%v
         q%vehicles = q%vehicles(2:)
      end subroutine remove
      
      
      subroutine print_licenses(q)
         class(q_type), intent(in) :: q
         integer :: n
         do n = 1, size(q%vehicles)
            select type (temp_v=>q%vehicles(n)%v)
               type is (car_type)
                  write (unit=*, fmt="(a9)", advance="no") "Car:"
               type is (bus_type)
                  write (unit=*, fmt="(a9)", advance="no") "Bus:"
               type is (truck_type)
                  write (unit=*, fmt="(a9)", advance="no") "Truck:"
               class default
                  write (unit=*, fmt="(a9)", advance="no") "Vehicle:"
            end select
            print *, trim(q%vehicles(n)%v%license)
         end do
      end subroutine print_licenses
      
end module v_q_module