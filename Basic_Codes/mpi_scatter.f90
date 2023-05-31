! MPI parallelized version to demonstrate usage of MPI_SCATTER
! reference link: https://rookiehpc.github.io/mpi/docs/mpi_scatter/index.html
!> Illustrates how to use a scatter.
!> This application is meant to be run with 4 processes. Process 0 is
!> designed as root and begins with a buffer containing all values, and prints
!> them. It then dispatches these values to all the processes in the same
!> communicator. Other processes just receive the dispatched value meant for 
!> them. Finally, everybody prints the value received.
!>
!>                +-----------------------+
!>                |       Process 0       |
!>                +-----+-----+-----+-----+
!>                |  0  | 100 | 200 | 300 |
!>                +-----+-----+-----+-----+
!>                 /      |       |      \
!>                /       |       |       \
!>               /        |       |        \
!>              /         |       |         \
!>             /          |       |          \
!>            /           |       |           \
!> +-----------+ +-----------+ +-----------+ +-----------+
!> | Process 0 | | Process 1 | | Process 2 | | Process 3 |
!> +-+-------+-+ +-+-------+-+ +-+-------+-+ +-+-------+-+ 
!>   | Value |     | Value |     | Value |     | Value |   
!>   |   0   |     |  100  |     |  200  |     |  300  |   
!>   +-------+     +-------+     +-------+     +-------+   
program mpi_scatter_tutorial

  implicit none
  include "mpif.h"

  integer :: ierr, nprocs, my_id
  integer :: root_rank = 0, my_value
  integer :: buffer(0:3)

  call MPI_INIT(ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
  if (nprocs .ne. 4) then
     write(*, '(A)') "This application is meant to be run with 4 MPI processes"
     call MPI_ABORT(MPI_COMM_WORLD, 0, ierr)
  end if
  call MPI_COMM_RANK(MPI_COMM_WORLD, my_id, ierr)

  if (my_id .eq. root_rank) then
     buffer = (/ 0, 100, 200, 300 /)
     write(*, '(A, I0, A, A, I0, A, I0, A, I0, A, I0, A)') "Values to scatter from process ", my_id, ":", &
          " ", buffer(0), ", ", buffer(1), ", ", buffer(2), ", ", buffer(3), "."
  end if

  call MPI_SCATTER(buffer, 1, MPI_INTEGER, my_value, 1, MPI_INTEGER, root_rank, MPI_COMM_WORLD, ierr)

  write(*,'(A, I0, A, I0)' ) "Process ", my_id, " received value = ", my_value

  call MPI_FINALIZE(ierr)
  
end program mpi_scatter_tutorial
