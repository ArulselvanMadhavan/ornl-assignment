module parallel_work
   use rand_utils, only: random_word
   use stdlib_string_type, only: string_type, assignment(=), write (formatted), char
   use stdlib_strings, only: to_string
   implicit none
   private
   public :: rm_duplicates_par
contains
   subroutine rm_duplicates_par(problem_size)
      integer, intent(in) :: problem_size
      integer, parameter :: word_len = 4
      integer :: i, n_images, image_id, from, to, neighbor
      character(len=word_len) :: word
      character(len=:), allocatable :: digits_in_word
      character(len=word_len + 3), allocatable :: words(:) [:]
      integer, allocatable :: word_lens(:) [:]
      n_images = num_images()
      image_id = this_image()
      from = (image_id - 1)*problem_size + 1
      to = image_id*problem_size
      neighbor = image_id + 2*modulo(image_id, 2) - 1

      allocate (words(from:to) [*])
      allocate (word_lens(from:to) [*])
      do i = 0, problem_size - 1
         call random_word(word, digits_in_word)
         words(from + i) = to_string(image_id, '(I2)')//"_"//word
         word_lens(from + i) = image_id
      end do

      if (image_id == 1) then
         do i = 1, n_images
            from = (i - 1)*problem_size + 1
            to = i*problem_size
            print *, "Last print:", from, to, i, words(:) [i], word_lens(:) [i]
         end do
      end if
   end subroutine rm_duplicates_par

end module parallel_work
