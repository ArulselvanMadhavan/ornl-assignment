module test_suite1
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use ornl_assignment, only: extract_digits
   implicit none
   private

   public :: collect_suite1

contains

!> Collect all exported unit tests
   subroutine collect_suite1(testsuite)
      !> Collection of tests
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [new_unittest("random_string(length=0)", test_random_0), &
                   new_unittest("random_string(length=5)", test_random_5), &
                   new_unittest("random_string(length=50)", test_random_50), &
                   new_unittest("random_string(length=500)", test_random_500) &
                   ]

   end subroutine collect_suite1

   subroutine test_random_5(error)
      type(error_type), allocatable, intent(out) :: error
      call test_random_string(error, 5)
   end subroutine test_random_5

   subroutine test_random_0(error)
      type(error_type), allocatable, intent(out) :: error
      call test_random_string(error, 0)
   end subroutine test_random_0

   subroutine test_random_50(error)
      type(error_type), allocatable, intent(out) :: error
      call test_random_string(error, 50)
   end subroutine test_random_50

   subroutine test_random_500(error)
      type(error_type), allocatable, intent(out) :: error
      call test_random_string(error, 500)
   end subroutine test_random_500

   subroutine test_random_string(error, size)
      use rand_utils, only: random_word
      type(error_type), allocatable, intent(out) :: error
      integer, intent(in) :: size
      integer, parameter :: total_tests = 2
      character(len=size) :: word
      character(len=:), allocatable :: digits_only_word
      call random_word(word, digits_only_word)
      call check(error, extract_digits(word), digits_only_word)
      if (allocated(error)) return
   end subroutine test_random_string

end module test_suite1

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
      integer, parameter :: word_size = 5
      character(len=word_size) :: words(size)
      call test_rm_duplicate_string(words, error)
   end subroutine test_rm_dupl_str_2

   subroutine test_rm_dupl_str_10(error)
      use stdlib_string_type, only: string_type
      type(error_type), allocatable, intent(out) :: error
      integer, parameter :: size = 10
      integer, parameter :: word_size = 5
      character(len=word_size) :: words(size)
      call test_rm_duplicate_string(words, error)
   end subroutine test_rm_dupl_str_10

   subroutine test_rm_dupl_str_100(error)
      use stdlib_string_type, only: string_type
      type(error_type), allocatable, intent(out) :: error
      integer, parameter :: size = 100
      integer, parameter :: word_size = 5
      character(len=word_size) :: words(size)
      call test_rm_duplicate_string(words, error)
   end subroutine test_rm_dupl_str_100

   subroutine test_rm_duplicate_string(words, error)
      use rand_utils, only: random_word
      use stdlib_string_type, only: string_type, assignment(=), write (formatted), operator(==)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), intent(inout) :: words(:)
      character(len=len(words(1))), allocatable :: unique_words(:)
      integer i
      character(len=len(words(1))) :: word
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

module test_suite3
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use ornl_assignment, only: to_integer
   implicit none
   private
   public :: collect_suite3
contains
!> Collect all exported unit tests
   subroutine collect_suite3(testsuite)
      !> Collection of tests
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [ &
                  new_unittest("specialties_lookup", test_lookup) &
                  ]
   end subroutine collect_suite3

   subroutine test_lookup(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=4) :: ids(3)
      integer :: res(3), expected(3), i
      ids = ["2234", "1234", "1124"]
      expected = [2234, 1234, 1124]
      res = to_integer(ids)
      do i = 1, size(res)
         call check(error, res(i), expected(i))
         if (allocated(error)) return
      end do
      
   end subroutine test_lookup

end module test_suite3

program check
   use, intrinsic :: iso_fortran_env, only: error_unit
   use testdrive, only: run_testsuite, new_testsuite, testsuite_type
   use test_suite1, only: collect_suite1
   use test_suite2, only: collect_suite
   use test_suite3, only: collect_suite3
   implicit none
   integer :: stat, is
   type(testsuite_type), allocatable :: testsuites(:)
   character(len=*), parameter :: fmt = '("#", *(1x, a))'

   stat = 0

   testsuites = [ &
                new_testsuite("extract_digits_suite", collect_suite1), &
                new_testsuite("remove_duplicates_suite", collect_suite), &
                new_testsuite("specialties_lookup", collect_suite3) &
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
