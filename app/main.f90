program main
  use ornl_assignment, only: say_hello
  use stdlib_string_type, only: string_type, assignment(=)
  implicit none
  type(string_type) :: akeys
  akeys = "7199"
  call say_hello(akeys)
end program main
