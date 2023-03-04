module ornl_assignment
   use stdlib_ascii, only: is_digit
   use stdlib_string_type, only: string_type, assignment(=), char, write (formatted)
   use stdlib_hashmaps, only: open_hashmap_type, default_bits, max_bits
   use stdlib_hashmap_wrappers, only: fnv_1_hasher, set, key_type

   implicit none
   real, parameter :: load_factor = 0.5625

   public :: remove_duplicates, filter_unique, extract_digits
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

   subroutine filter_unique(unique_words, xs, ys)
      ! Declarations
      type(open_hashmap_type) :: map
      character(len=*), intent(in) :: xs(:), unique_words(:)
      character(len=len(xs(1))), allocatable :: temp(:)
      character(len=*), allocatable, intent(out) :: ys(:)
      type(key_type)      :: key
      logical :: is_present, conflict
      integer :: i, count, slot_bits
      count = 0
      allocate (temp, source=xs)
      slot_bits = exponent(size(unique_words)/load_factor)
      slot_bits = max(default_bits, slot_bits)
      ! print *, "Filt begin:", slot_bits, 2**slot_bits, size(unique_words)
      call map%init(fnv_1_hasher, slots_bits=slot_bits)
      do i = 1, size(unique_words)
         call set(key, unique_words(i))
         call map%map_entry(key, conflict=conflict)
      end do
      ! print *, "Filt done:"
      do i = 1, size(xs)
         call set(key, xs(i))
         call map%key_test(key, is_present)
         if (.not. is_present) then
            count = count + 1
            temp(count) = xs(i)
         end if
      end do
      ! print *, "filt lookup done"
      ys = temp(1:count)
   end subroutine filter_unique

   subroutine remove_duplicates(xs, ys)
      ! Declarations
      type(open_hashmap_type) :: map
      character(len=*), intent(in) :: xs(:)
      character(len=len(xs(1))), allocatable :: temp(:)
      character(len=*), allocatable, intent(out) :: ys(:)
      type(key_type)      :: key
      logical             :: conflict
      integer :: i, count, pos, slot_bits

      ! Init
      count = 0
      allocate (temp, source=xs)
      slot_bits = exponent((size(xs)/load_factor))
      slot_bits = max(default_bits, slot_bits)
      ! print *, "Rem begin:", slot_bits, 2**slot_bits, size(xs)
      call map%init(fnv_1_hasher, slots_bits=slot_bits)
      ! main logic
      do i = 1, size(xs)

         call set(key, xs(i))
         call map%map_entry(key, conflict=conflict)
         if (.not. conflict) then
            pos = count + 1
            temp(pos) = xs(i)
            count = pos
         end if
      end do
      ! print *, "Rem end:", slot_bits, 2**slot_bits, size(xs)
      ys = temp(1:count)
   end subroutine remove_duplicates

end module ornl_assignment
