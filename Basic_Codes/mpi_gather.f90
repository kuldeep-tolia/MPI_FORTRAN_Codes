! MPI parallelized version to demonstrate usage of MPI_GATHER
! reference link: https://rookiehpc.github.io/mpi/docs/mpi_gather/index.html
!> Illustrates how to use a gather.
!> This application is meant to be run with 4 MPI processes. Every MPI
!> process begins with a value, then MPI process 0 is picked to gather all these
!> values and print them. It can be visualised as follows:
!>
!> +-----------+ +-----------+ +-----------+ +-----------+
!> | Process 0 | | Process 1 | | Process 2 | | Process 3 |
!> +-+-------+-+ +-+-------+-+ +-+-------+-+ +-+-------+-+ 
!>   | Value |     | Value |     | Value |     | Value |   
!>   |   0   |     |  100  |     |  200  |     |  300  |   
!>   +-------+     +-------+     +-------+     +-------+   
!>            \            |     |            /
!>             \           |     |           /
!>              \          |     |          /
!>               \         |     |         /
!>                \        |     |        /
!>                 \       |     |       /
!>                +-----+-----+-----+-----+
!>                |  0  | 100 | 200 | 300 |
!>                +-----+-----+-----+-----+
!>                |       Process 0       |
!>                +-----------------------+
program mpi_gather_tutorial

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

  my_value = my_id * 100
  write(*,'(A, I0, A, I0)' ) "Process ", my_id, " has my_value = ", my_value
  
  if (my_id .eq. root_rank) then
     call MPI_GATHER(my_value, 1, MPI_INTEGER, buffer, 1, MPI_INTEGER, root_rank, MPI_COMM_WORLD, ierr)
     write(*, '(A, I0, A, I0, A, I0, A, I0, A, I0, A)') "Values collected on process ", my_id, ": ", &
          buffer(0), ", ", buffer(1), ", ", buffer(2), ", ", buffer(3), "."
  else
     call MPI_GATHER(my_value, 1, MPI_INTEGER, buffer, 0, MPI_INTEGER, root_rank, MPI_COMM_WORLD, ierr)
  end if

  call MPI_FINALIZE(ierr)
  
end program mpi_gather_tutorial
