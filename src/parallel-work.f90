module parallel_work
   use rand_utils, only: random_word
   use stdlib_string_type, only: string_type, assignment(=), write (formatted), char
   use stdlib_strings, only: to_string
   implicit none
   private
   public :: rm_duplicates_par
contains
   subroutine rm_duplicates_par(problem_size, image_id, n_images)
      integer, intent(in) :: problem_size, image_id, n_images
      integer, parameter :: word_len = 4
      integer :: i, from, to
      character(len=word_len) :: word
      character(len=:), allocatable :: digits_in_word
      character(len=word_len + 3), allocatable :: words(:) [:]

      from = (image_id - 1)*problem_size + 1
      to = image_id*problem_size
      allocate (words(from:to) [*])

      do i = 0, problem_size - 1
         call random_word(word, digits_in_word)
         words(from + i) = to_string(image_id, '(I2)')//"_"//word
      end do

      if (image_id == 1) then
         do i = 1, n_images
            from = (i - 1)*problem_size + 1
            to = i*problem_size
            print *, "Last print:", from, to, i, words(:) [i]
         end do
      end if
   end subroutine rm_duplicates_par

end module parallel_work
