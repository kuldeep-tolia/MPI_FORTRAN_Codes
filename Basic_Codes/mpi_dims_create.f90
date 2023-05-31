! MPI parallelized version to demonstrate usage of MPI_DIMS_CREATE
! reference link: https://rookiehpc.github.io/mpi/docs/mpi_dims_create/index.html
!> Illustrate how to decompose MPI processes in a cartesian grid.
!> This application is to be run with 12 processes. It attempts to
!> decompose these 12 MPI processes in 3 dimensions (i.e: a cube).
program mpi_dims_create_tutorial

  implicit none
  include "mpif.h"

  integer :: ierr, nprocs, my_id
  integer :: dims(0:2)

  call MPI_INIT(ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
  if (nprocs .ne. 12) then
     write(*, '(A)') "This application is meant to be run with 12 MPI processes"
     call MPI_ABORT(MPI_COMM_WORLD, 0, ierr)
  end if
  call MPI_COMM_RANK(MPI_COMM_WORLD, my_id, ierr)

  if (my_id .eq. 0) then
     write(*, '(A, I0, A)') "Attempting to decompose ", nprocs, " processes into 3 dimensions:"

     ! give total freedom to MPI and auto-allocate
     dims = (/ 0, 0, 0 /)
     write(*, '(A, I0, A, I0, A, I0, A)', advance = 'NO') "Restriction (", dims(0), ", ", dims(1), ",", dims(2), &
          ") gave following decomposition: "
     call MPI_DIMS_CREATE(nprocs, 3, dims, ierr)
     write(*, '(A, I0, A, I0, A, I0, A)') "(", dims(0), ", ", dims(1), ", ", dims(2), ")"

     ! give restriction of 6 processes in 1st dimension
     dims = (/ 6, 0, 0 /)
     write(*, '(A, I0, A, I0, A, I0, A)', advance = 'NO') "Restriction (", dims(0), ", ", dims(1), ",", dims(2), &
          ") gave following decomposition: "
     call MPI_DIMS_CREATE(nprocs, 3, dims, ierr)
     write(*, '(A, I0, A, I0, A, I0, A)') "(", dims(0), ", ", dims(1), ", ", dims(2), ")"

     ! give restriction of 4 processes in 2nd dimension and 1 process in 3rd dimension
     dims = (/ 0, 4, 1 /)
     write(*, '(A, I0, A, I0, A, I0, A)', advance = 'NO') "Restriction (", dims(0), ", ", dims(1), ",", dims(2), &
          ") gave following decomposition: "
     call MPI_DIMS_CREATE(nprocs, 3, dims, ierr)
     write(*, '(A, I0, A, I0, A, I0, A)') "(", dims(0), ", ", dims(1), ", ", dims(2), ")"
  end if
  
  call MPI_FINALIZE(ierr)
  
end program mpi_dims_create_tutorial
