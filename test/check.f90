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

module test_suite2
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use ornl_assignment, only: remove_duplicates
   implicit none
   private
   public :: collect_suite
contains
!> Collect all exported unit tests
   subroutine collect_suite(testsuite)
      !> Collection of tests
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [ &
                  new_unittest("rm_duplicate_string(list_size=2)", test_rm_dupl_str_2), &
                  new_unittest("rm_duplicate_string(list_size=10)", test_rm_dupl_str_10), &
                  new_unittest("rm_duplicate_string(list_size=100)", test_rm_dupl_str_100) &
                  ]
   end subroutine collect_suite

   subroutine test_rm_dupl_str_2(error)
      use stdlib_string_type, only: string_type
      type(error_type), allocatable, intent(out) :: error
      integer, parameter :: size = 2
      type(string_type) :: words(size)
      call test_rm_duplicate_string(words, error)
   end subroutine test_rm_dupl_str_2

   subroutine test_rm_dupl_str_10(error)
      use stdlib_string_type, only: string_type
      type(error_type), allocatable, intent(out) :: error
      integer, parameter :: size = 10
      type(string_type) :: words(size)
      call test_rm_duplicate_string(words, error)
   end subroutine test_rm_dupl_str_10

   subroutine test_rm_dupl_str_100(error)
      use stdlib_string_type, only: string_type
      type(error_type), allocatable, intent(out) :: error
      integer, parameter :: size = 100
      type(string_type) :: words(size)
      call test_rm_duplicate_string(words, error)
   end subroutine test_rm_dupl_str_100

   subroutine test_rm_duplicate_string(words, error)
      use rand_utils, only: random_word
      use stdlib_string_type, only: string_type, assignment(=), write (formatted), operator(==)
      type(error_type), allocatable, intent(out) :: error
      integer, parameter :: word_size = 5
      type(string_type), intent(inout) :: words(:)
      type(string_type), allocatable :: unique_words(:)
      integer i
      character(len=word_size) :: word
      character(len=:), allocatable :: digits_only_word
      integer half
      logical is_equal
      half = size(words)/2

      do i = 1, half
         call random_word(word, digits_only_word)
         words(i) = word
      end do
      words(half + 1:) = words(1:half)
      call remove_duplicates(words, unique_words)
      call check(error, size(unique_words), half)
      if (allocated(error)) return
      do i = 1, size(unique_words)
         call check(error, words(i) == unique_words(i))
         if (allocated(error)) return
      end do
   end subroutine test_rm_duplicate_string
end module test_suite2

! module test_suite1
!    use testdrive, only: new_unittest, unittest_type, error_type, check
!    use ornl_assignment, only: extract_digits
!    implicit none
!    private

!    public :: collect_suite1

! contains

! !> Collect all exported unit tests
!    subroutine collect_suite1(testsuite)
!       !> Collection of tests
!       type(unittest_type), allocatable, intent(out) :: testsuite(:)

!       testsuite = [new_unittest("random_string(length=0)", test_random_0), &
!                    new_unittest("random_string(length=5)", test_random_5), &
!                    new_unittest("random_string(length=50)", test_random_50), &
!                    new_unittest("random_string(length=500)", test_random_500) &
!                    ]

!    end subroutine collect_suite1

!    subroutine test_random_5(error)
!       type(error_type), allocatable, intent(out) :: error
!       call test_random_string(error, 5)
!    end subroutine test_random_5

!    subroutine test_random_0(error)
!       type(error_type), allocatable, intent(out) :: error
!       call test_random_string(error, 0)
!    end subroutine test_random_0

!    subroutine test_random_50(error)
!       type(error_type), allocatable, intent(out) :: error
!       call test_random_string(error, 50)
!    end subroutine test_random_50

!    subroutine test_random_500(error)
!       type(error_type), allocatable, intent(out) :: error
!       call test_random_string(error, 500)
!    end subroutine test_random_500

!    subroutine test_random_string(error, size)
!       type(error_type), allocatable, intent(out) :: error
!       integer, intent(in) :: size
!       integer, parameter :: total_tests = 2
!       character(len=size) :: word
!       character(len=:), allocatable :: digits_only_word
!       call random_word(word, digits_only_word)
!       call check(error, extract_digits(word), digits_only_word)
!       if (allocated(error)) return
!    end subroutine test_random_string

! end module test_suite1

program check
   use, intrinsic :: iso_fortran_env, only: error_unit
   use testdrive, only: run_testsuite, new_testsuite, testsuite_type
   ! use test_suite1, only: collect_suite1
   use test_suite2, only: collect_suite
   implicit none
   integer :: stat, is
   type(testsuite_type), allocatable :: testsuites(:)
   character(len=*), parameter :: fmt = '("#", *(1x, a))'

   stat = 0

   testsuites = [ &
                ! new_testsuite("suite1", collect_suite1), &
                new_testsuite("suite2", collect_suite) &
                ]

   do is = 1, size(testsuites)
      write (error_unit, fmt) "Testing:", testsuites(is)%name
      call run_testsuite(testsuites(is)%collect, error_unit, stat)
   end do

   if (stat > 0) then
      write (error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
      error stop
   end if
end program check
