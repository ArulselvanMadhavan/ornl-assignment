program main
   use ornl_assignment, only: extract_digits, remove_duplicates
   use parallel_work, only: rm_duplicates_par
   use stdlib_random, only: random_seed
   implicit none
   integer, parameter :: problem_sizes(2) = [64000, 128000]
   integer :: i, n_images, image_id
   integer :: seed_put, seed_get

   n_images = num_images()
   image_id = this_image()

   seed_put = 1234567
   call random_seed(seed_put + image_id, seed_get)
   do i = 1, size(problem_sizes)
      call rm_duplicates_par(problem_sizes(i)/n_images, image_id, n_images)
   end do
end program main
