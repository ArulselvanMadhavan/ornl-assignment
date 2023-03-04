program main
   use ornl_assignment, only: extract_digits, remove_duplicates
   use parallel_work, only: rm_duplicates_par
   implicit none
   integer, parameter :: problem_sizes(1) = [10]
   integer :: i, n_images, image_id
   n_images = num_images()
   image_id = this_image()

   do i = 1, size(problem_sizes)
      call rm_duplicates_par(problem_sizes(i), image_id, n_images)
   end do
end program main
