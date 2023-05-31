! MPI parallelized version to compute first derivative using explicit 2nd order central difference scheme

program main

  implicit none
  include "mpif.h"

  integer :: my_id, nprocs, ierr, status(MPI_STATUS_SIZE)
  integer :: i, nx, local_n, local_xs, local_xe, local_l
  integer, dimension(:), allocatable :: counts, displacements

  double precision, parameter :: dx = 1.0d-3 ! set the delta-x
  double precision :: xmin, xmax, x, temp
  double precision, dimension(:), allocatable :: local_U, local_dU, global_dU
  double precision :: start_t, end_t

  character(50) :: filename

  xmin = -1.0d0
  xmax = 1.0d0
  
  call MPI_INIT(ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, my_id, ierr)

  start_t = MPI_WTIME()
  nx = int((xmax - xmin) / dx)
  local_n = nx / nprocs

  ! assigning local start/end points for each process
  if (my_id .ne. nprocs-1) then
     local_xs = my_id * local_n
     local_xe = local_xs + local_n - 1
  else
     local_xs = my_id * local_n
     local_xe = local_xs + local_n
  end if

  ! calculate local array lengths
  local_l = local_xe - local_xs

  ! allocate memory
  allocate (local_U(0:local_l+2), local_dU(0:local_l))
  allocate (counts(0:nprocs-1), displacements(0:nprocs-1))

  ! initialize arrays
  local_U = 0.0d0
  local_dU = 0.0d0

  ! calculate local-U_i before calculating derivates
  do i = 1, local_l+1
     x = xmin + (local_xs + i - 1) * dx
     local_U(i) = x * tan(x)
  end do

  ! NOTE: since 2nd order CDS is implemented, it will require only one ghost point to store the boundary U value
  ! communicating to left neighbour
  if (my_id .ne. 0) call MPI_SEND(local_U(1), 1, MPI_DOUBLE_PRECISION, my_id-1, 100, MPI_COMM_WORLD, ierr)
  if (my_id .ne. nprocs-1) call MPI_RECV(local_U(local_l+2), 1, MPI_DOUBLE_PRECISION, my_id+1, 100, &
       MPI_COMM_WORLD, status, ierr)
   
  ! communicating to right neighbour
  if (my_id .ne. nprocs-1) call MPI_SEND(local_U(local_l+1), 1, MPI_DOUBLE_PRECISION, my_id+1, 200, MPI_COMM_WORLD, ierr)
  if (my_id .ne. 0) call MPI_RECV (local_U(0), 1, MPI_DOUBLE_PRECISION, my_id-1, 200, MPI_COMM_WORLD, status, ierr)

  ! calculating local first derivative in each process
  do i = 0, local_l
     if ((my_id .eq. 0) .AND. (i .eq. 0)) then ! left boundary point
        local_dU(i) = (-3.0d0 * local_U(i+1) + 4.0d0 * local_U(i+2) - local_U(i+3)) / (2.0d0 * dx)
     else if ((my_id .eq. nprocs-1) .AND. (i .eq. local_l)) then ! right boundary point
        local_dU(i) = (3.0d0 * local_U(i+1) - 4.0d0 * local_U(i) + local_U(i-1)) / (2.0d0 * dx)
     else                       ! interior points (in global sense)
        local_dU(i) = (local_U(i+2) - local_U(i)) / (2.0d0 * dx)
     end if
  end do

  ! gathering lengths of each array from each process
  call MPI_GATHER(local_l+1, 1, MPI_INTEGER, counts, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    
  if (my_id .eq. 0) then
     allocate (global_dU(0:nx))

     ! define the displacements
     do i = 0, nprocs-1
        displacements(i) = i * (local_l + 1)
     end do
  end if

  ! gathering locally computed first derivatives from each process into root process
  call MPI_GATHERV(local_dU, local_l+1, MPI_DOUBLE_PRECISION, global_dU, counts, &
       displacements, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)

  end_t = MPI_WTIME()

  ! writing results in output file
  if (my_id .eq. 0) then
     write(filename, '(A, F6.4, A)') "first_derivative_dx_", dx, ".txt"
     open(UNIT = 11, FILE = filename)
     do i = 0, nx
        x = xmin + i * dx
        write(11, *) x, tan(x) + x / cos(x)**2, global_dU(i)
     end do
     close (11)
     deallocate (global_dU)
  end if

  call MPI_BARRIER(MPI_COMM_WORLD, ierr)
  
  ! deallocate memory
  deallocate (local_U, local_dU)
  deallocate (counts, displacements)
  
  call MPI_FINALIZE(ierr)
  if (my_id .eq. 0) write(*, '(A, F8.6, A, I3)') "Program running time = ", end_t - start_t, &
       " seconds with processes used = ", nprocs
    
end program main
