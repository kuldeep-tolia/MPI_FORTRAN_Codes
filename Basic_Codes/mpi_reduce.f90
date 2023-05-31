! MPI parallelized version to demonstrate usage of MPI_REDUCE
! reference link: https://rookiehpc.github.io/mpi/docs/mpi_reduce/index.html
!> Illustrates how to use a reduce.
!> This application consists of a sum reduction every MPI process
!> sends its rank for reduction before the sum of these ranks is stored in the
!> root MPI process. It can be visualised as follows, with MPI process 0 as
!> root:
!>
!> +-----------+ +-----------+ +-----------+ +-----------+
!> | Process 0 | | Process 1 | | Process 2 | | Process 3 |
!> +-+-------+-+ +-+-------+-+ +-+-------+-+ +-+-------+-+
!>   | Value |     | Value |     | Value |     | Value |
!>   |   0   |     |   1   |     |   2   |     |   3   |
!>   +-------+     +-------+     +-------+     +-------+
!>            \         |           |         /
!>             \        |           |        /
!>              \       |           |       /
!>               \      |           |      /
!>                +-----+-----+-----+-----+
!>                            |
!>                        +---+---+
!>                        |  SUM  |
!>                        +---+---+
!>                            |
!>                        +---+---+
!>                        |   6   |
!>                      +-+-------+-+
!>                      | Process 0 |
!>                      +-----------+
program mpi_reduce_tutorial

  implicit none
  include "mpif.h"

  integer :: ierr, nprocs, my_id
  integer :: reduction_result = 0, root_rank = 0

  call MPI_INIT(ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
  if (nprocs .ne. 4) then
     write(*, '(A)') "This application is meant to be run with 4 MPI processes"
     call MPI_ABORT(MPI_COMM_WORLD, 0, ierr)
  end if
  call MPI_COMM_RANK(MPI_COMM_WORLD, my_id, ierr)

  ! each MPI process sends its rank value to reduction variable and root-rank MPI process will collect the result
  call MPI_REDUCE(my_id, reduction_result, 1, MPI_INTEGER, MPI_SUM, root_rank, MPI_COMM_WORLD, ierr)

  if (my_id .eq. root_rank) then
     write(*, '(A, I0, A)') "The sum of ranks is ", reduction_result, "."
  end if

  call MPI_FINALIZE(ierr)
  
end program mpi_reduce_tutorial
