! MPI parallelized version of matrix(nxn) multiplication using Canon's algorithm
! Assumptions:
! A and B matrices are square matrices and the number of processors used should be a square number.

program main

  implicit none
  include "mpif.h"

  integer, parameter :: N = 64  ! size of global matrix
  integer :: my_id, nprocs, ierr, status(MPI_STATUS_SIZE), canon_comm
  integer :: i, j, k, phase, local_n
  integer :: dims(0:1)
  logical :: periods(0:1)
  logical :: reorder
  integer :: left, right, up, down
  
  double precision, dimension(:, :), allocatable :: local_A, local_B, local_C, buffer, global_C, temp
  double precision :: start_t, end_t

  call MPI_INIT(ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, my_id, ierr)

  ! initialize new communicator
  dims(0) = 0                   ! allowing MPI to auto-allocate the process in each direction
  dims(1) = 0
  call MPI_DIMS_CREATE(nprocs, 2, dims, ierr)
  
  if (dims(0) .ne. dims(1)) then
     if (my_id .eq. 0) print*, "The number of processes must be a square number"
     call MPI_FINALIZE(ierr)
     STOP
  end if

  periods(0) = .TRUE.                ! setting periodicity in each direction for wraparound
  periods(1) = .TRUE.
  reorder = .TRUE.

  local_n = N / dims(0)
  
  ! allocate memory
  allocate (local_A(0:local_n-1, 0:local_n-1), local_B(0:local_n-1, 0:local_n-1))
  allocate (local_C(0:local_n-1, 0:local_n-1), buffer(0:local_n-1, 0:local_n-1))
  allocate (temp(0:local_n-1, 0:local_n-1))

  ! populate matrices
  local_A = 1.0d0               ! matrix-A is initialized to 1.0 for sanity check, user can input their desired matrix
  local_B = 1.0d0               ! matrix-B is initialized to 1.0 for sanity check, user can input their desired matrix
  local_C = 0.0d0               ! matrix-C is initialized to 0.0
  buffer = 0.0d0
  temp = 0.0d0

  ! creating new communicator
  call MPI_CART_CREATE(MPI_COMM_WORLD, 2, dims, periods, reorder, canon_comm, ierr)

  ! obtain ranks of neighbouring processes
  call MPI_CART_SHIFT(canon_comm, 0, 1, left, right, ierr)
  call MPI_CART_SHIFT(canon_comm, 1, 1, up, down, ierr)

  start_t = MPI_WTIME()
  do phase = 0, dims(0)-1
     do j = 0, local_n-1        ! perform local block matrix multiplication
        do i = 0, local_n-1
           do k = 0, local_n-1
              local_C(i, j) = local_C(i, j) + local_A(i, k) * local_B(k, j)
           end do
        end do
     end do
     if (phase .eq. (dims(0)-1)) exit

     ! communication calls
     call MPI_SENDRECV(local_A, local_n*local_n, MPI_DOUBLE_PRECISION, left, 100, buffer, local_n*local_n, MPI_DOUBLE_PRECISION, &
          right, 100, canon_comm, status, ierr)
     temp = buffer
     buffer = local_A
     local_A = temp
     call MPI_SENDRECV(local_B, local_n*local_n, MPI_DOUBLE_PRECISION, up, 200, buffer, local_n*local_n, MPI_DOUBLE_PRECISION, &
           down, 200, canon_comm, status, ierr)
     temp = buffer
     buffer = local_B
     local_B = temp
  end do

  call MPI_BARRIER(canon_comm, ierr)
  end_t = MPI_WTIME()

  if (my_id .eq. 0) then        ! user can turn off/on the comment for printing the obtained global matrix for smaller matrices
     allocate (global_C(0:N-1, 0:N-1))
     call MPI_GATHER(local_C, local_n*local_n, MPI_DOUBLE_PRECISION, global_C, local_n*local_n, MPI_DOUBLE_PRECISION, 0, &
          canon_comm, ierr)
  else
     call MPI_GATHER(local_C, local_n*local_n, MPI_DOUBLE_PRECISION, global_C, local_n*local_n, MPI_DOUBLE_PRECISION, 0, &
          canon_comm, ierr)
  end if

  call MPI_BARRIER(canon_comm, ierr)

  if (my_id .eq. 0) then
     print*, "---------Printing global matrix-C---------"
     write(*, *) global_C(:, :)
     deallocate (global_C)
  end if
    
  ! deallocate memory
  deallocate (local_A, local_B)
  deallocate (local_C, buffer)
  deallocate (temp)
  
  call MPI_FINALIZE(ierr)

  if (my_id .eq. 0) write(*, '(A, F8.6, A, I3)') "Program running time = ", end_t - start_t, &
       " seconds with processes used = ", nprocs
    
end program main
