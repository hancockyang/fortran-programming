program submod
   use line_mod
   implicit none
   type (line) :: line_1
      line_1 = line(0, 0, 1, 1)
   print * line_1%length
end program submod