! MPI parallelized version to compute first derivative using explicit 4th order central difference scheme

program main

  implicit none
  include "mpif.h"

  integer, parameter :: n = 10000000  ! set the number of grid points
  integer :: my_id, nprocs, ierr, status(MPI_STATUS_SIZE)
  integer :: i, l, local_n, cimin, cimax
  
  double precision :: xmin, xmax, x, temp, dx
  double precision, dimension(:), allocatable :: local_f, local_df, global_df
  double precision :: start_t, end_t

  character(50) :: filename

  xmin = 0.0d0
  xmax = 3.0d0
  dx = (xmax - xmin) / (n - 1)
  
  start_t = MPI_WTIME()
  call MPI_INIT(ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, my_id, ierr)

  local_n = (n - 1) / nprocs
  cimin = -2                    ! 4th order accuracte CDS requires 2 ghost points
  cimax = local_n + 2

  if (my_id .eq. 0) cimin = 0
  if (my_id .eq. nprocs-1) cimax = local_n + MOD((n-1), nprocs) ! assign extra remaining points to last process id

  ! allocate memory
  allocate (local_f(cimin:cimax), local_df(cimin:cimax))

  do i = cimin, cimax
     local_f(i) = sin(5.0d0 * dx * (my_id * local_n + i))
  end do

  ! calculating local first derivative in each process
  local_df(cimin) = (-3.0d0 * local_f(cimin) + 4.0d0 * local_f(cimin+1) - local_f(cimin+2)) / (2.0d0 * dx) ! computing derivative at left boundary using 2nd order scheme
  local_df(cimin+1) = (-3.0d0 * local_f(cimin+1) + 4.0d0 * local_f(cimin+2) - local_f(cimin+3)) / (2.0d0 * dx)
  local_df(cimax-1) = (3.0d0 * local_f(cimax-1) - 4.0d0 * local_f(cimax-2) + local_f(cimax-3)) / (2.0d0 * dx) ! computing derivative at right boundary using 2nd order scheme
  local_df(cimax) = (3.0d0 * local_f(cimax) - 4.0d0 * local_f(cimax-1) + local_f(cimax-2)) / (2.0d0 * dx)

  do i = cimin+2, cimax-2       ! computing derivative at interior points using 4th order scheme
     local_df(i) = (-local_f(i+2) + 8.0d0 * local_f(i+1) - 8.0d0 * local_f(i-1) + local_f(i-2)) / (12.0d0 * dx)
  end do
  
  if (my_id .eq. nprocs-1) then ! gathering local derivative data on last process
     allocate (global_df(0:n-1))

     do l = 0, nprocs-2
        call MPI_RECV(global_df(l*local_n:(l+1)*local_n-1), local_n, MPI_DOUBLE_PRECISION, &
             l, l*101, MPI_COMM_WORLD, status, ierr)
     end do

     global_df(my_id*local_n:n-1) = local_df(0:cimax) ! concatenating the last process derivative data

     write(filename, '(A, I6, A)') "first_derivative_N", n, ".txt"
     open(UNIT = 11, FILE = filename)
     do i = 0, n-1
        x = i * dx
        write(11, *) x, 5.0d0 * cos(5.0d0 * x), global_df(i)
     end do

     close(11)

     deallocate (global_df)
  else
     call MPI_SEND(local_df(0:local_n-1), local_n, MPI_DOUBLE_PRECISION, nprocs-1, my_id*101, MPI_COMM_WORLD, ierr)
  end if

  ! deallocate memory
  deallocate (local_f, local_df)
     
  call MPI_FINALIZE(ierr)
  end_t = MPI_WTIME()
  
  if (my_id .eq. nprocs-1) write(*, '(A, F9.6, A, I3)') "Program running time = ", end_t - start_t, &
       " seconds with processes used = ", nprocs
    
end program main
