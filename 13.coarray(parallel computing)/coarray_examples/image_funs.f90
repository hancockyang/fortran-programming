!  image_funs.f90 
!
!  FUNCTIONS:
!  image_funs - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: image_funs
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

program image_funs

   implicit none
   real, codimension[0:1, *] :: C
   if (this_image() == 2) then
      print *, "num_images =", num_images()
      print *, "lower cobounds of C", lcobound(C)
      print *, "upper cobounds of C", ucobound(C)
      print *, "cosubscripts of C on image 2", this_image(C)
   endif
end program image_funs

!Because there are four images and C is declared with codimensions [0:1, *], C is represented on the four images as follows:
!C[0, 1] is on image 1
!C[1, 1] is on image 2, hence the cosubscripts of C on image 2 are 1 and 1
!C[0, 2] is on image 3
!C[1, 2] is on image 4