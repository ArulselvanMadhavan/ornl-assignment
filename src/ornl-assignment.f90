module ornl_assignment
   use stdlib_ascii, only: is_digit
   implicit none
   public :: extract_digits
contains
   pure function extract_digits(id) result(id_clean)
      character(len=*), intent(in) :: id
      character(len=len(id)) :: numbers
      character(len=:), allocatable :: id_clean
      integer :: i, pos
      integer :: count
      count = 0
      pos = 1
      do i = 1, len(id)
         if (is_digit(id(i:i))) then
            pos = count + 1
            numbers(pos:pos) = id(i:i)
            count = count + 1
         end if
      end do
      id_clean = numbers(1:count)
   end function extract_digits
end module ornl_assignment
