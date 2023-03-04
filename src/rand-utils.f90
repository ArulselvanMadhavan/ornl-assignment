module rand_utils
   implicit none
   public :: random_word
contains

   subroutine random_word(word, digits_only_word)
      use stdlib_random, only: dist_rand
      use stdlib_kinds, only: int8, sp
      integer(int8) :: zero_ascii, nine_ascii, digit_range, ascii_code, sample
      character(len=*), intent(inout) :: word
      character(len=len(word)) :: digits
      character(len=:), allocatable, intent(out) :: digits_only_word
      integer :: i, count, pos
      real(sp) :: sample_fp32
      ! Init
      zero_ascii = 48
      nine_ascii = 57
      digit_range = nine_ascii - zero_ascii + 1
      count = 0

      ! Word gen
      do i = 1, len(word)
         sample = dist_rand(1_int8)

         ! Positive range
         if (sample < 0) then
            sample = sample + 128
         end if
         ! Insert a digit about 50% of the time
         if (sample < 128/2) then
            pos = count + 1
            sample_fp32 = sample/128.0
            ascii_code = zero_ascii + floor(digit_range*sample_fp32)
            count = count + 1
            digits(pos:pos) = achar(ascii_code)
         else
            ascii_code = sample
         end if

         word(i:i) = achar(ascii_code)
      end do
      digits_only_word = digits(1:count)
   end subroutine random_word
end module rand_utils

