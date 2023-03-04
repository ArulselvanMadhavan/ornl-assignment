program main
   use ornl_assignment, only: extract_digits, remove_duplicates
   use stdlib_string_type, only: string_type, assignment(=), write (formatted), char
   use stdlib_hashmap_wrappers, only: fnv_1_hasher, key_type, set
   use stdlib_hashmaps, only: chaining_hashmap_type, int_index
   implicit none
   character(len=5) :: id
   character(len=:), allocatable :: id_clean
   type(string_type) :: xs(3)
   type(string_type), allocatable :: ys(:)
   type(chaining_hashmap_type) :: map

   integer :: i
   ! Problem 1
   id = ".7324"
   id_clean = extract_digits(id)

   ! Problem 2
   xs(1) = "arul"
   xs(2) = "arul"
   xs(3) = "a"
   call remove_duplicates(xs, ys)
   print *, xs
   print *, ys
   print *, char(xs(1))
end program main
