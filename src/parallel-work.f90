module parallel_work
   use rand_utils, only: random_word
   ! use stdlib_string_type, only: string_type,
   ! use stdlib_strings, only: to_string
   use ornl_assignment, only: remove_duplicates, filter_unique
   implicit none
   private
   public :: rm_duplicates_par
contains
   subroutine rm_duplicates_par(problem_size, image_id, n_images)
      integer, intent(in) :: problem_size, image_id, n_images
      integer, parameter :: word_len = 4
      integer :: i, to, iter, word_max
      integer, allocatable :: unique_len[:], from[:]
      character(len=word_len) :: word
      character(len=:), allocatable :: digits_in_word
      character(len=word_len + 2) :: temp_word
      character(len=len(temp_word)), allocatable :: words(:) [:]
      character(len=len(temp_word)), allocatable :: unique_words(:)
      character(len=len(temp_word)), allocatable :: buffer(:)
      character(len=*), parameter :: fmt = '(I1.1,A1,A4)'
      allocate (from[*])
      from = (image_id - 1)*problem_size + 1
      to = image_id*problem_size
      allocate (words(from:to) [*])
      allocate (unique_words(problem_size))
      allocate (unique_len[*])

      ! Create words
      do i = 1, problem_size
         call random_word(word, digits_in_word)
         write (temp_word, fmt) image_id, "_", word
         words(from + i - 1) = temp_word
      end do
      ! insert duplicates for test
      do i = 1, image_id
         write (words(from + i - 1), fmt) i, "_", "arul"
      end do
      ! rm duplicates on each image - passing unique words between them
      do iter = 1, n_images
         if (iter == 1) then
            call remove_duplicates(words, unique_words)
            unique_len = size(unique_words)
         else
            ! iter - range[2, n_images]
            if (iter <= image_id) then
               buffer = words(:) [iter - 1]
               buffer = buffer(:unique_len[iter - 1])
               ! print *, "Iter debug", iter, image_id, from[iter - 1], unique_len, buffer
               call filter_unique(buffer, words(from:from + unique_len - 1), unique_words)
               unique_len = size(unique_words)
               words(from:from + unique_len - 1) = unique_words
            end if

         end if
         sync all

         if (image_id == 1) then
            buffer = words(:) [iter]
            buffer = buffer(:unique_len[iter])
            print *, "Unique:", iter, buffer(:4), size(buffer), (i, i=2, n_images)
         end if

      end do
   end subroutine rm_duplicates_par

end module parallel_work
