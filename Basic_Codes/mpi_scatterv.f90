! MPI parallelized version to demonstrate usage of MPI_SCATTERV
! reference link: https://rookiehpc.github.io/mpi/docs/mpi_scatterv/index.html
!> Illustrates how to use the variable version of a scatter.
!> A process is designed as root and begins with a buffer containig all
!> values, and prints them. It then dispatches these values to all the processes
!> in the same communicator. Other process just receive the dispatched value(s)
!> meant for them. Finally, everybody prints the value received. This
!> application is designed to cover all cases:
!> - Different send counts
!> - Different displacements
!> This application is meant to be run with 3 processes.
!>
!>       +-----------------------------------------+
!>       |                Process 0                |
!>       +-----+-----+-----+-----+-----+-----+-----+
!>       | 100 |  0  | 101 | 102 |  0  |  0  | 103 |
!>       +-----+-----+-----+-----+-----+-----+-----+
!>         |            |     |                |
!>         |            |     |                |
!>         |            |     |                |
!>         |            |     |                |
!>         |            |     |                |
!>         |            |     |                |
!> +-----------+ +-------------------+ +-----------+
!> | Process 0 | |    Process 1      | | Process 2 |
!> +-+-------+-+ +-+-------+-------+-+ +-+-------+-+
!>   | Value |     | Value | Value |     | Value |
!>   |  100  |     |  101  |  102  |     |  103  |
!>   +-------+     +-------+-------+     +-------+
program mpi_scatterv_tutorial

  implicit none
  include "mpif.h"

  integer :: ierr, nprocs, my_id
  integer :: root_rank = 0, my_value_count, i
  integer :: count(0:2), displacement(0:2), buffer(0:6)
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
     buffer = (/ 100, 0, 101, 102, 0, 0, 103 /)
     count = (/ 1, 2, 1 /)
     displacement = (/ 0, 2, 6 /)

     write(*, '(A, I0, A)', advance = 'NO') "Values gathered in the buffer on root process ", my_id, ":"
     do i = 0, 6
        write(*, '(A, I0)', advance = 'NO') " ", buffer(i)
     end do
     write(*, '(A)') ""

  case (1)
     my_value_count = 2
     allocate (my_values(0:my_value_count-1))
  case(2)
     my_value_count = 1
     allocate (my_values(0:my_value_count-1))
  end select

  call MPI_SCATTERV(buffer, count, displacement, MPI_INTEGER, my_values, my_value_count, MPI_INTEGER, &
       root_rank, MPI_COMM_WORLD, ierr)

  write(*, '(A, I0, A)', advance = 'NO') "Process ", my_id, " received values: "
  do i = 0, my_value_count-1
     write(*, '(I0, A)', advance = 'NO') my_values(i), " "
  end do
  write(*, '(A)') ""

  deallocate (my_values)
  
  call MPI_FINALIZE(ierr)
  
end program mpi_scatterv_tutorial
