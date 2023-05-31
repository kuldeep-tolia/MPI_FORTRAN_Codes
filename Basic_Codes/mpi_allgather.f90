! MPI parallelized version to demonstrate usage of MPI_ALLGATHER
! reference link: https://rookiehpc.github.io/mpi/docs/mpi_allgather/index.html
!> Illustrates how to use an allgather.
!> This application is meant to be run with 3 MPI processes. Every MPI
!> process begins with a value, then every MPI process collects the entirety of
!> the data gathered and prints them. It can be visualised as follows:
!>
!> +-----------+  +-----------+  +-----------+
!> | Process 0 |  | Process 1 |  | Process 2 |
!> +-+-------+-+  +-+-------+-+  +-+-------+-+
!>   | Value |      | Value |      | Value |
!>   |   0   |      |  100  |      |  200  |
!>   +-------+      +-------+      +-------+
!>       |________      |      ________|
!>                |     |     | 
!>             +-----+-----+-----+
!>             |  0  | 100 | 200 |
!>             +-----+-----+-----+
!>             |   Each process  |
!>             +-----------------+
program mpi_allgather_tutorial

  implicit none
  include "mpif.h"

  integer :: ierr, nprocs, my_id
  integer :: my_value
  integer :: buffer(0:2)

  call MPI_INIT(ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
  if (nprocs .ne. 3) then
     write(*, '(A)') "This application is meant to be run with 3 MPI processes"
     call MPI_ABORT(MPI_COMM_WORLD, 0, ierr)
  end if
  call MPI_COMM_RANK(MPI_COMM_WORLD, my_id, ierr)

  my_value = my_id * 100
  write(*,'(A, I0, A, I0)' ) "Process ", my_id, " has my_value = ", my_value

  call MPI_ALLGATHER(my_value, 1, MPI_INTEGER, buffer, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr)
  write(*, '(A, I0, A, I0, A, I0, A, I0, A)') "Values collected on process ", my_id, ": ", &
          buffer(0), ", ", buffer(1), ", ", buffer(2), "."
  
  call MPI_FINALIZE(ierr)
  
end program mpi_allgather_tutorial
