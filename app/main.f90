program main
   use ornl_assignment, only: extract_digits
   ! use stdlib_string_type, only: string_type, assignment(=)
   implicit none
   character(len=5) :: id
   character(len=:), allocatable :: id_clean
   integer :: count
   id = ".7324"
   id_clean = extract_digits(id)
   print *, count
   print *, len(id_clean)
end program main
