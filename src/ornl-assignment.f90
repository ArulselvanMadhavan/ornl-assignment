module ornl_assignment
   use stdlib_ascii, only: is_digit
   use stdlib_string_type, only: string_type, assignment(=), char, write (formatted)
   use stdlib_hashmaps, only: open_hashmap_type, default_bits, max_bits
   use stdlib_hashmap_wrappers, only: fnv_1_hasher, set, key_type, other_type, get

   implicit none
   real, parameter :: load_factor = 0.5625

   public :: remove_duplicates, filter_unique, extract_digits, to_integer, lookup_ids
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

   pure function get_digits(num) result(digits)
      use iso_fortran_env, only: int8
      integer, intent(in) :: num
      integer(int8), allocatable :: digits(:)
      integer :: num_digits, ix, rem
      if (num == 0) then
         allocate (digits(1))
         digits(1) = 0
      else
         num_digits = floor(log10(real(num)) + 1)
         allocate (digits(num_digits))

         rem = num
         do ix = 1, num_digits
            digits(ix) = rem - (rem/10)*10  ! Take advantage of integer division
            rem = rem/10
         end do
      end if

   end function get_digits

   subroutine lookup_ids(specialty_ids, specialty_names, ids, specialties)
      use stdlib_hashmaps, only: chaining_hashmap_type

      integer, intent(in) :: specialty_ids(:)
      character(len=*), intent(in) :: ids(:)
      character(len=len(ids(1))), allocatable :: ids_digits(:)
      type(string_type), intent(out) :: specialties(:)
      type(string_type), allocatable :: data_str
      type(string_type), intent(in) :: specialty_names(size(specialty_ids))
      type(key_type)              :: key
      type(other_type)            :: other
      type(chaining_hashmap_type) :: map
      logical :: is_present, conflict
      integer :: i, slot_bits
      class(*), allocatable :: data
      character(len=len(ids(1))), allocatable :: unique_ids(:)
      integer, allocatable :: unique_ids_int(:)

      ! Init hashmap
      slot_bits = exponent(size(specialty_ids)/load_factor)
      slot_bits = max(default_bits, slot_bits)
      call map%init(fnv_1_hasher, slots_bits=slot_bits)

      ! Fill the map with (id, specialty)
      do i = 1, size(specialty_ids)
         call set(key, get_digits(specialty_ids(i)))
         allocate (data, source=char(specialty_names(i)))
         call set(other, data)
         call map%map_entry(key, other, conflict=conflict)
         deallocate (data)
      end do

      ! Build unique ids
      allocate (ids_digits(size(ids)))
      do i = 1, size(ids)
         ids_digits(i) = extract_digits(ids(i))
      end do

      ! Remove duplicates
      call remove_duplicates(ids_digits, unique_ids)

      ! Build int ids
      unique_ids_int = to_integer(unique_ids)
      do i = 1, size(unique_ids_int)
         call set(key, get_digits(unique_ids_int(i)))
         call map%key_test(key, is_present)
         if (is_present) then
            call map%get_other_data(key, other)
            call get(other, data)
            select type (data)
            type is (character(len=*))
               specialties(i) = data
            class default
               specialties(i) = "N/A"
            end select
         else
            specialties(i) = "N/A"
         end if
      end do
   end subroutine lookup_ids

   pure elemental function to_integer(digits) result(number)
      character(len=*), intent(in) :: digits
      integer :: number, stat
      read (digits, *, iostat=stat) number
   end function to_integer

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

end module ornl_assignment
