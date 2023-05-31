! MPI parallelized version to demonstrate usage of MPI_ALLREDUCE
! reference link: https://rookiehpc.github.io/mpi/docs/mpi_allreduce/index.html
!> Illustrates how to use an all-reduce.
!> This application consists of a sum all-reduction every MPI process
!> sends its rank for reduction before the sum of these ranks is stored in the
!> receive buffer of each MPI process. It can be visualised as follows:
!>
!> +-----------+ +-----------+ +-----------+ +-----------+
!> | Process 0 | | Process 1 | | Process 2 | | Process 3 |
!> +-+-------+-+ +-+-------+-+ +-+-------+-+ +-+-------+-+
!>   | Value |     | Value |     | Value |     | Value |
!>   |   0   |     |   1   |     |   2   |     |   3   |
!>   +-------+     +----+--+     +--+----+     +-------+
!>            \         |           |         /
!>             \        |           |        /
!>              \       |           |       /
!>               \      |           |      /
!>                +-----+-----+-----+-----+
!>                            |
!>                        +---+---+
!>                        |  SUM  |
!>                        +---+---+
!>                        |   6   |
!>                        +-------+
!>                            |
!>                +-----+-----+-----+-----+
!>               /      |           |      \
!>              /       |           |       \
!>             /        |           |        \
!>            /         |           |         \
!>   +-------+     +----+--+     +--+----+     +-------+  
!>   |   6   |     |   6   |     |   6   |     |   6   |  
!> +-+-------+-+ +-+-------+-+ +-+-------+-+ +-+-------+-+
!> | Process 0 | | Process 1 | | Process 2 | | Process 3 |
!> +-----------+ +-----------+ +-----------+ +-----------+
program mpi_allreduce_tutorial

  implicit none
  include "mpif.h"

  integer :: ierr, nprocs, my_id
  integer :: reduction_result = 0

  call MPI_INIT(ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
  if (nprocs .ne. 4) then
     write(*, '(A)') "This application is meant to be run with 4 MPI processes"
     call MPI_ABORT(MPI_COMM_WORLD, 0, ierr)
  end if
  call MPI_COMM_RANK(MPI_COMM_WORLD, my_id, ierr)

  ! each MPI process sends its rank value to reduction variable and all MPI process will collect the result
  call MPI_ALLREDUCE(my_id, reduction_result, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierr)

  write(*, '(A, I0, A, I0, A)') "MPI process = ", my_id, ", The sum of all ranks is ", reduction_result, "."
  
  call MPI_FINALIZE(ierr)
  
end program mpi_allreduce_tutorial
