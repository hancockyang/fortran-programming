# fortran-programming
## Linked list (working in gfortran not in ifort)
1. Recursive List Processing
To view the list as a recursive data structure as described above, a node should consist of a value and another object, next, of type sorted_list. An object of type
sorted_list is a pointer.
2. Inserting a Number
Let us first do the subroutine that inserts a number into a list. The recursive version is
deceptively simple. First, if the list is empty, another list must be created and the number
placed in its value field. Then the next field is made empty because there are no
other elements in the list.
If the list is not empty, the number to be inserted must be compared with the first
value in the list. If it is smaller or equal, it must be inserted as the first value of the list.
Again, a new first node is created and the number placed in its value field. However,
this time the next field of this new first node is set equal to the original list before the
insertion because all other numbers in the list follow this new number.
A temporary variable of type list is necessary to prevent losing the reference to the
list when a new first node is allocated. Notice that pointer assignment (=>) is used for
the variables of type sorted_list.
These are the nonrecursive base cases of the recursive subroutine insert. The only
other remaining case is when the number to be inserted is greater than the first element
of the list. In this case, the insertion is completed by a recursive call to insert the
number in the rest of the list.
3. Determining if a List is Empty
The function that determines if a list is empty is straightforward. Recall that a pointer
is not associated if it is null.
4. Deleting a Number
The subroutine to delete a number from a list, if it is there, is quite similar to the subroutine
to insert. There are two special nonrecursive cases. If the list is empty, the number
cannot be deleted from it, so found is set to false. If the number is the first number
in the list, deleting it may be accomplished by making list start with its second element
(if any) using the statement.
Also, it is a good idea to deallocate the space for the deleted node to avoid unreferenced
storage.
The first of these must be done before list is reassigned, and the others
afterward. temp%next must be set to null, so that when temp is deallocated, the final
subroutine empty does not deallocate the entire remainder of the list.
In case the list is not empty, but the desired number is not its first element, the
number is deleted by a recursive call to delete it from the rest of list. 
5. Whenever a list of type sorted_list is deallocated, the final subroutine empty is called
with the list as its argument. The subroutine deletes the node pointed to by the argument
list and recursively deletes the list referenced by its next component.