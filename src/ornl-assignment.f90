! module map_functions
! use iso_fortran_env, only:int8, int16, int32, int64, real32, real64, real128
! use stdlib_string_type
!   implicit none
!   public
!   contains
!   pure logical function is_number(x) result(res)
!     character, intent(in) :: x
!     res = .true.
!   end function is_number

! pure integer(int8) function xpowx_i1(x) result(res)
!   integer(int8), intent(in) :: x
!   res = x**x
! end function xpowx_i1

! end module map_functions

module ornl_assignment
   implicit none
   private

   public :: say_hello
contains
   subroutine say_hello(string)
      use stdlib_string_type, only: string_type, assignment(=), write (formatted)
      use stdlib_strings, only: padr
      implicit none
      type(string_type), intent(in) :: string

      ! string = "right pad this string"
! string <-- "right pad this string"

      print '(dt)', padr(string, 25, "$") ! "right pad this string$$$$"

      !string = padr(string, 25)
      print '(dt)', string
! string <-- "right pad this string    "
   end subroutine say_hello
end module ornl_assignment
