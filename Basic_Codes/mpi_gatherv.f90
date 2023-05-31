! MPI parallelized version to demonstrate usage of MPI_GATHERV
! reference link: https://rookiehpc.github.io/mpi/docs/mpi_gatherv/index.html
!> Illustrates how to use the variable version of a gather.
!> Every MPI process begins with a value, the MPI process 0 will gather
!> all these values and print them. The example is designed to cover all cases:
!> - Different displacements
!> - Different receive counts
!> It can be visualised as follows:
!> This application is meant to be run with 3 processes.
!>
!> +-----------+ +-----------+ +-------------------+ 
!> | Process 0 | | Process 1 | |     Process 2     |
!> +-+-------+-+ +-+-------+-+ +-+-------+-------+-+
!>   | Value |     | Value |     | Value | Value |
!>   |  100  |     |  101  |     |  102  |  103  |
!>   +-------+     +-------+     +-------+-------+
!>      |                |            |     |
!>      |                |            |     |
!>      |                |            |     |
!>      |                |            |     |
!>      |                |            |     |
!>      |                |            |     |
!>   +-----+-----+-----+-----+-----+-----+-----+
!>   | 100 |  0  |  0  | 101 |  0  | 102 | 103 |
!>   +-----+-----+-----+-----+-----+-----+-----+
!>   |                Process 0                |
!>   +-----------------------+-----+-----+-----+
program mpi_gatherv_tutorial

  implicit none
  include "mpif.h"

  integer :: ierr, nprocs, my_id
  integer :: root_rank = 0, my_value_count, i
  integer :: count(0:2)
  integer :: displacement(0:2)
  integer :: buffer(0:6) = 0
  integer, dimension(:), allocatable :: my_values

  call MPI_INIT(ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
  if (nprocs .ne. 3) then
     write(*, '(A)') "This application is meant to be run with 3 MPI processes"
     call MPI_ABORT(MPI_COMM_WORLD, 0, ierr)
  end if
  call MPI_COMM_RANK(MPI_COMM_WORLD, my_id, ierr)

  select case(my_id)
  case (0)
     my_value_count = 1
     allocate (my_values(0:my_value_count-1))
     my_values(0) = 100
     count = (/ 1, 1, 2 /)
     displacement = (/ 0, 3, 5 /)

     write(*, '(A, I0, A, I0)') "Process ", my_id, ", my_value = ", my_values(0)

  case(1)
     my_value_count = 1
     allocate (my_values(0:my_value_count-1))
     my_values(0) = 101

     write(*, '(A, I0, A, I0)') "Process ", my_id, ", my_value = ", my_values(0)

  case(2)
     my_value_count = 2
     allocate (my_values(0:my_value_count-1))
     my_values = (/ 102, 103 /)
     
     write(*, '(A, I0, A, I0, A, I0)') "Process ", my_id, ", my_value = ", my_values(0), ", ",my_values(1)
  end select

  call MPI_GATHERV(my_values, my_value_count, MPI_INTEGER, buffer, count, displacement, MPI_INTEGER, &
       root_rank, MPI_COMM_WORLD, ierr)

  deallocate(my_values)

  if (my_id .eq. root_rank) then
     write(*, '(A, I0, A)', advance = 'NO') "Values gathered in the buffer on process ", my_id, ":"
     do i = 0, 6
        write(*, '(A, I0)', advance = 'NO') " ", buffer(i)
     end do
     write(*, '(A)') ""
  end if
  
  call MPI_FINALIZE(ierr)
  
end program mpi_gatherv_tutorial
