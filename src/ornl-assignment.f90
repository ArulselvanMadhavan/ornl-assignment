module ornl_assignment
   use stdlib_ascii, only: is_digit
   implicit none
   public :: extract_digits
   public :: remove_duplicates
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
            count = pos
         end if
      end do
      id_clean = numbers(1:count)
   end function extract_digits

   subroutine remove_duplicates(xs, ys)
      ! Imports
      use stdlib_string_type, only: string_type, assignment(=), char, write (formatted)
      use stdlib_hashmaps, only: chaining_hashmap_type
      use stdlib_hashmap_wrappers, only: fnv_1_hasher, set, key_type
      ! Declarations
      type(chaining_hashmap_type) :: map
      type(string_type), intent(in) :: xs(:)
      type(string_type), allocatable :: temp(:)
      type(string_type), allocatable, intent(out) :: ys(:)
      type(key_type)      :: key
      logical             :: conflict
      integer :: i, count, pos
      ! Init
      count = 0
      allocate (temp, source=xs)
      call map%init(fnv_1_hasher, slots_bits=8)
      ! main logic
      do i = 1, size(xs)
         call set(key, char(xs(i)))
         call map%map_entry(key, conflict=conflict)
         if (.not. conflict) then
            pos = count + 1
            temp(pos) = xs(i)
            count = pos
         end if
      end do

      ys = temp(1:count)
   end subroutine remove_duplicates

end module ornl_assignment
