!  heat4.f90 
!
!  FUNCTIONS:
!  heat4 - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: heat4
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

program heat4
   use heat_xfer_mod
   implicit none

   integer :: start, stop1, counts_per_second
   integer :: line
   real, dimension(:,:), allocatable, target :: a
   real, dimension(:,:), allocatable :: temp
   call set_boundary_conditions()
   sync all
   call system_clock(start, counts_per_second)
   call initialize_quadrants()
   sync all
   call heat_xfer()
   sync all

!   if (this_image() == 1) then
!      call system_clock(stop1)
!      print *, "For 4-image solution, time = ", &
!      (stop1 - start) / counts_per_second, " seconds"
!      allocate (plate(0:P+1,0:P+1), stat = alloc_stat)
!      if (alloc_stat > 0) then
!         print *, "Allocation of plate failed"
!      stop
!      end if
!      plate(0:Q, 0:Q) = quad(0:Q, 0:Q ) [1,1] ! NW
!      plate(Q+1:, 0:Q) = quad(1:Q+1, 0:Q ) [2,1] ! SW
!      plate(0:Q, Q+1:) = quad(0:Q, 1:Q+1) [1,2] ! NE
!      plate(Q+1:, Q+1:) = quad(1:Q+1, 1:Q+1) [2,2] ! SE
!      print *
!      print *, "Number of iterations (4 images):", n_iter !MPI *100
!      ! call print_plate(plate) ! Uncomment for debugging
!   end if
!
!   if (this_image() == 1) then
!      allocate (a(0:P+1,0:P+1), temp(P,P), &
!      stat = alloc_stat)
!      if (alloc_stat > 0) then
!         print *, "Allocation of a or temp failed"
!         stop
!      end if
!      a = 0
!      a(0, :) = top
!      a(:, 0) = left
!      a(:, P+1) = right
!      a(P+1:, 0) = bottom
!      call system_clock(start)
!      n_iter = 0
!      associate ( &
!         interior => a(1:P, 1:P), &
!         n => a(0:P-1, 1:P ), &
!         s => a(2:P+1, 1:P ), &
!         w => a(1:P, 0:P-1), &
!         e => a(1:P, 2:P+1))
!         call system_clock(start)
!         n_iter = 0
!         do
!            temp = (n + e + w + s) / 4
!            n_iter = n_iter + 1
!            diff = maxval(abs(temp - interior))
!            interior = temp
!            if (diff < tolerance) exit
!         end do
!      end associate
!      call system_clock(stop1)
!      print *
!      print *, "For 1-image solution, time = ", &
!      (stop1 - start) / counts_per_second, " seconds"
!      diff = maxval(abs(plate(1:P, 1:P) - a(1:P, 1:P)))
!      print *
!      print *, "Number of iterations (1 image):", n_iter
!      ! call print_plate(a) ! Uncomment to see values for small plate
!      print *
!      print *, "Max difference between methods:", diff
!   end if
   pause
   sync all
end program heat4

