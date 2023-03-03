module map_functions
use iso_fortran_env, only:int8, int16, int32, int64, real32, real64, real128
  
  implicit none
  public
  contains
  pure logical function is_number(x) result(res)
    character, intent(in) :: x
    res = .true.
  end function is_number

pure integer(int8) function xpowx_i1(x) result(res)
  integer(int8), intent(in) :: x
  res = x**x
end function xpowx_i1

end module map_functions

module ornl_assignment
  use functional
  use map_functions
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello(akeys)
    implicit none
    character(len=*), intent(in) :: akeys
    akeys = map(xpowx_i1, akeys)
    ! do n = 0, len(akeys)
    !    print *, "Key:", akeys(n)
    ! end do
    print *, "Submission by: ", len(akeys)
  end subroutine say_hello
end module ornl_assignment
