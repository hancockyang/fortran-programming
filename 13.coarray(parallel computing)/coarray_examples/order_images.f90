!  order_images.f90 
!
!  FUNCTIONS:
!  order_images - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: order_images
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************
!To understand how this program works, it is helpful to think first about the execution
!by image 1, then image 2, and so forth. For image 1, the first if statement is false,
!so the two assignment statements are executed next. They set p(1) to 1 on image 1 and
!increment k on image 1 to 2. Then it issues a sync with image 2, so image 1 waits at this
!point until image 2 syncs with it.
!Meanwhile, image 2 executes the statement that syncs it with image 1, so it waits
!until image 1 has executed the assignment statements described above and syncs with
!image 2. Then image 1 can continue to the sync all statement and wait for the other
!images to reach that point. Image 2 executes the two assignment statements, setting
!p(2) to 2 and k to 3 (both on image 1 only).
!The execution on images 3 and 4 is similar. After all images reach the sync all
!statement, image 1 prints the four values of the array p, which are 1, 2, 3, and 4.
program order_images

   implicit none
   integer :: me, n_i
   integer, codimension[*] :: k = 1
   integer, dimension(4), codimension[*] :: p
   me = this_image()
   n_i = num_images()
   if (me > 1) sync images (me - 1)
   p(k[1])[1] = me
   k[1] = k[1] + 1
   if (me < n_i) sync images (me + 1)
   sync all
   if (this_image() == 1) print *, p
   pause

end program order_images

