program main
   use ornl_assignment, only: extract_digits, remove_duplicates
   use parallel_work, only: rm_duplicates_par
   use stdlib_random, only: random_seed
   implicit none
   character(len=*), parameter :: VERSION = '1.0'
   character(len=32)           :: arg
   integer                     :: i, problem_size, stat
   integer :: n_images, image_id
   integer :: seed_put, seed_get

   do i = 1, command_argument_count()
      call get_command_argument(i, arg)
      select case (arg)
      case ('-v', '--version')
         print '(2a)', 'version ', VERSION
         stop

      case ('-h', '--help')
         call print_help()
         stop

      case default
         select case (arg(1:3))
         case ('-p:')
            read (arg(4:), '(I32)', iostat=stat) problem_size
         case default
            print '(2a, /)', 'unrecognised command-line option: ', arg
            call print_help()
            stop
         end select
      end select
   end do

   ! integer, parameter :: problem_sizes(2) = [600000, 6000000]
   n_images = num_images()
   image_id = this_image()

   seed_put = 1234567
   call random_seed(seed_put + image_id, seed_get)
   ! do i = 1, size(problem_sizes)
   call rm_duplicates_par(problem_size/n_images, image_id, n_images)
   ! end do
contains
   subroutine print_help()
      print '(a, /)', 'command-line options:'
      print '(a)', '  -v, --version     print version information and exit'
      print '(a, /)', '  -h, --help        print usage information and exit'
   end subroutine print_help
end program main
