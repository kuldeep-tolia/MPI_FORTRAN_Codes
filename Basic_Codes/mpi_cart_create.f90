! MPI parallelized version to demonstrate usage of MPI_CART_CREATE
! reference link: https://rookiehpc.github.io/mpi/docs/mpi_cart_create/index.html
!> Illustrates how to create a communicator representing a 2D torus topology.
program mpi_cart_create_tutorial

  implicit none
  include "mpif.h"

  integer :: ierr, nprocs, my_id
  integer :: dims(0:1), my_coord(0:1)
  logical :: periodic(0:1), reorder
  integer :: new_comm

  call MPI_INIT(ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
  dims = (/ 0, 0 /)               ! ask MPI to decompose nrpocs in 2D cartesian grid
  call MPI_DIMS_CREATE(nprocs, 2, dims, ierr)

  periodic = (/ .TRUE., .TRUE. /) ! both dimensions are periodic
  reorder = .TRUE.                !let MPI decide to assign arbitary ranks if it deems it necessary

  call MPI_CART_CREATE(MPI_COMM_WORLD, 2, dims, periodic, reorder, new_comm, ierr) ! create a new communicator for given 2D torus topology
  call MPI_COMM_RANK(new_comm, my_id, ierr)
  call MPI_CART_COORDS(new_comm, my_id, 2, my_coord, ierr) ! get coordinates of my_id in new communicator

  ! print my location in 2D torus topology
  write(*, '(A, I0, A, I0, A, I0, A)') "MPI process ", my_id, " is located at (", my_coord(0), ", ", my_coord(1), ")."
  
  call MPI_FINALIZE(ierr)
  
end program mpi_cart_create_tutorial
