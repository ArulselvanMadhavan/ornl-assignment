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

      testsuite = [ new_unittest("valid", test_valid) ]

   end subroutine collect_suite1

   subroutine test_valid(error)
      type(error_type), allocatable, intent(out) :: error
      call check(error, extract_digits(".7324"), "7324")
      if (allocated(error)) return
   end subroutine test_valid

end module test_suite1

program check
   use, intrinsic :: iso_fortran_env, only: error_unit
   use testdrive, only: run_testsuite, new_testsuite, testsuite_type
   use test_suite1, only: collect_suite1
   implicit none
   integer :: stat, is
   type(testsuite_type), allocatable :: testsuites(:)
   character(len=*), parameter :: fmt = '("#", *(1x, a))'

   stat = 0

   testsuites = [ &
                new_testsuite("suite1", collect_suite1) &
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
