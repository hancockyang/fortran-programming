submodule (line_mod) line_length_mod

   contains
      module procedure length
      
         length = sqrt((l%x2-l%x1)**2 + l%y2-l%y1)**2)
         
      end procedure length
      
end submodule line_length_mod