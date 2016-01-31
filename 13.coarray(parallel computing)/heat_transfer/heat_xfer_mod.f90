module heat_xfer_mod
! A parameter P represents the size of the plate (also Q = P/2 will be used in the coarray version).
!
! In order to speed up the computation, the plate is divided into quadrants. The iterations
! needed to solve the heat transfer problem are carried out on each quadrant simultaneously
! on different images.
!
! Although the computations for each quadrant can be executed independently of
! the other quadrants, some parts of the border of each quadrant are cells in an adjacent
! quadrant. This is illustrated by looking at the lower-left quadrant (the southwest quadrant).
! The northern border of this quadrant consists of cells in the northwest quadrant
! and the eastern border consists of cells in the southeast quadrant. Thus, the values
! along the southern border of the northwest quadrant must be copied to the image processing
! the southwest quadrant. 
!
! Similar declarations are provided for the four-image case, except that there is a
! coarray quad representing each of four quadrants of the plate and there is a coarray
! scalar diff that keeps track of how the process is converging. Most of this code is in a
! module.
   implicit none
   private
   
   integer, public, parameter :: P = 100, Q = P/2
   real, public, parameter :: tolerance = 1.0e-5
   real, public, dimension(:, :), allocatable :: plate
   real, public, dimension(:, :), codimension[:,:],allocatable,target :: quad
   real, public, dimension(:, :), allocatable :: temp_interior
   real, public, codimension[*] :: diff
   enum, bind(C)
      enumerator :: NW=1, SW, NE, SE
   end enum
   
   real, public, pointer, dimension(:,:) :: n, e, s, w, interior
   real, public, allocatable, dimension(:) :: top, bottom, left, right
   integer :: j, image
   integer, public :: n_iter = 0, alloc_stat
   integer, public, parameter :: chunk = 100
   
   public :: set_boundary_conditions, initialize_quadrants, print_plate , heat_xfer
   
   contains
   ! The module procedure set_boundary_conditions allocates the arrays top, left,
   ! right, and bottom and gives them values. A different way to write this code would be
   ! to make these parameters
      subroutine set_boundary_conditions ()
         ! boundaries are outside of (1:P,1:P) region
         allocate (top (0:P+1), bottom(0:P+1), left(0:P+1), right (0:P+1),stat = alloc_stat)
         if (alloc_stat > 0) then
            print *, "Allocation of boundary failed on image", this_image()
            stop
         end if
         top = [ 1.0, ( real(j)/P, j = P, 0, -1) ]
         left = 1.0
         right = 0.0
         bottom = 0.0
      end subroutine set_boundary_conditions
   
   ! Another procedure in the module allocates the arrays for each quadrant of the
   ! plate. Remember that this same code will be executed on each image. Q is half of P.
      subroutine initialize_quadrants ()
         ! set coarray quad:
         ! [1, 1] NW: image 1
         ! [2, 1] SW: image 2
         ! [1, 2] NE: image 3
         ! [2, 2] SE: image 4
         ! (0:Q+1, 0:Q+1) to include boundaries and interactive area between other coarray
         allocate (quad(0:Q+1, 0:Q+1) [2,*], stat = alloc_stat)
         
         if (alloc_stat > 0) then
            print *, "Allocation of quadrant failed on image", this_image()
            stop
         end if
         ! interior temp of each quad
         allocate (temp_interior(1:Q, 1:Q), stat = alloc_stat)
         if (alloc_stat > 0) then
            print *, "Allocation of temp interior failed on image", this_image()
            stop
         end if
   ! Next, the boundary values are set for each quadrant. Note that NW, SW, NE, and
   ! SE are simply parameters with values 1, 2, 3, and 4, declared using enumerators (1.2).
   ! The parameter names help to understand the code a little bit better.
   
         quad = 0.0
         select case (this_image())
            case(NW) ! northwest
               quad(:,0) = left(:Q+1)
               quad(0,:) = top(:Q+1)
            case(SW) ! southwest
               quad(:,0) = left(Q:)
               quad(Q+1,:) = bottom(:Q+1)
            case(NE) ! northeast
               quad(Q+1,:) = right(:Q+1)
               quad(0,:) = top(Q:)
            case(SE) ! southeast
               quad(Q+1,:) = right(Q:)
               quad(Q+1,:) = bottom(Q:)
         end select
      end subroutine initialize_quadrants
   
   ! The heat transfer computation itself consists of updating the quadrant boundaries
   ! and averaging the temperature at each point in the interior. The loop repeats until
   ! there is convergence. This code must use cosubscripts, rather than image numbers.
      
      subroutine heat_xfer()
         heat_xfer_loop: do
         ! Update interior quadrant boundaries
         ! Plate boundaries have not changed
            select case (this_image())
               case(NW)
                  quad(Q+1, 1:Q) = quad(1, 1:Q)[2,1] ! S shared board with south
                  quad(1:Q, Q+1) = quad(1:Q, 1)[1,2] ! E shared board with east
               case(SW)
                  quad(0, 1:Q)   = quad(Q ,1:Q)[1,1] ! N shared board with north
                  quad(1:Q, Q+1) = quad(1:Q, 1)[2,2] ! E shared board with east
               case(NE)
                  quad(Q+1, 1:Q) = quad(1, 1:Q)[2,2] ! S shared board with south
                  quad(1:Q, 0 )  = quad(1:Q, Q)[1,1] ! W shared board with west
               case(SE)
                  quad(0, 1:Q)   = quad(Q ,1:Q)[1,2] ! N shared board with north
                  quad(1:Q, 0 )  = quad(1:Q, Q)[2,1] ! W shared board with west
            end select
            ! For the southwest quadrant, values are copied from the image to the north into its
            ! top boundary, and values are copied from the image to the east into its eastern boundary.
            ! The associate construct is used when updating the cells in order to make the code
            ! a little more readable.
            sync all
            ! this is used to reach convergence in each quad
!            associate ( &
!               interior => quad(1:Q, 1:Q), &
!               n => quad(0:Q-1, 1:Q ), &
!               s => quad(2:Q+1, 1:Q ), &
!               e => quad(1:Q , 2:Q+1), &
!               w => quad(1:Q , 0:Q-1))
!               temp_interior = (n + e + s + w) / 4
!               diff = maxval(abs(interior - temp_interior))
!               interior = temp_interior
!            end associate
            
            temp_interior = (quad(1:Q , 2:Q+1) + quad(0:Q-1, 1:Q ) + quad(2:Q+1, 1:Q ) + quad(1:Q , 0:Q-1)) / 4
            diff = maxval(abs(quad(1:Q, 1:Q) - temp_interior))
            quad(1:Q, 1:Q) = temp_interior
            
            sync all
            !Then image 1 checks the maximum of the iteration differences
            !on the four images and exits the loop if there is convergence.
            if (this_image() == 1) then
               n_iter = n_iter + 1
               do image = 2, num_images()
                  diff = max (diff, diff[image])
               end do
            end if
            sync all
            if (diff[1] < tolerance) exit heat_xfer_loop
         end do heat_xfer_loop
      end subroutine heat_xfer
      
      subroutine print_plate(x)
         real, dimension(:,:), intent(in) :: x
         integer :: line
         print *
         do line = 1, size(x, 2)
            print "(1000f5.2)", x(line, :)
         end do
      end subroutine print_plate
      
end module heat_xfer_mod