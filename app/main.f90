program main
   use ornl_assignment, only: extract_digits, remove_duplicates
   use parallel_work, only: rm_duplicates_par
   use rand_utils, only: random_word
   use stdlib_string_type, only: string_type, assignment(=), write (formatted), char
   use stdlib_strings, only: to_string
   use stdlib_hashmap_wrappers, only: fnv_1_hasher, key_type, set
   use stdlib_hashmaps, only: chaining_hashmap_type, int_index
   implicit none
   integer, parameter :: problem_sizes(1) = [10]
   integer, parameter :: word_len = 4
   integer :: i, w, n_images, chunk, image_id, from, to, neighbor
   character(len=word_len) :: word
   character(len=:), allocatable :: digits_in_word
   type(string_type), allocatable, dimension(:), codimension[:] :: words
   integer, dimension(10) :: word_lens[*]
   integer :: stat
   integer :: ending[*]
   n_images = num_images()
   image_id = this_image()

   do i = 1, size(problem_sizes)
      call rm_duplicates_par(problem_sizes(i))
      ! problem_size = problem_sizes(i)
      ! from = (image_id - 1)*problem_size + 1
      ! to = image_id*problem_size
      ! allocate (words(from:to) [*], stat=stat)
      ! do w = 0, problem_size - 1
      !    call random_word(word, digits_in_word)
      !    words(from + w) = to_string(image_id)//"_"//word
      !    word_lens(modulo(from + w, 10) + 1) = image_id
      ! end do
      ! neighbor = image_id + 2*modulo(image_id, 2) - 1
      ! print *, problem_size, from, to, image_id, n_images, neighbor
      ! ending = to
      ! sync all

      ! ! if (image_id == 1) then
      ! !    do w = 1, n_images
      ! !       from = (w - 1)*problem_size + 1
      ! !       to = w*problem_size
      ! !       print *, "Last print:", from, to, w, words(:)[w]
      ! !    end do
      ! ! end if
      ! print *, image_id, neighbor, word_lens(:)[neighbor], ending[image_id], ending[neighbor]
      ! sync all
      ! deallocate (words, stat=stat)
   end do
end program main
