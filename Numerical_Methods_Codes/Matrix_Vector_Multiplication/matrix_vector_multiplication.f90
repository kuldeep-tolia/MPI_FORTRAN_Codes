! MPI parallelized version of matrix(mxn) - vector(nx1) multiplication
program matvecMultiplication

  implicit none
  include "mpif.h"

  double precision, dimension(:, :), allocatable :: local_A
  double precision, dimension(:), allocatable :: local_x, local_b
  double precision :: start_time, end_time
  integer :: my_id, nprocs, status(MPI_STATUS_SIZE), ierr, comm
  integer :: m, local_m, n, local_n, i, j
	
  m = 10240	                ! number of rows
  n = 10240                     ! number of columns
	
  start_time = MPI_WTIME()
  call MPI_INIT(ierr)
  comm = MPI_COMM_WORLD
  call MPI_COMM_SIZE(comm, nprocs, ierr)
  call MPI_COMM_RANK(comm, my_id, ierr)
	
  local_m = m / nprocs            ! m > 0 and should be evenly divisible by nprocs
  local_n = n / nprocs            ! n > 0 and should be evenly divisible by nprocs
!  call read_dimensions(m, local_m, n, local_n, my_id, nprocs, comm)		! user can uncomment this procedure if wish to 
  call allocate_memory(local_A, local_x, local_b, local_m, n, local_n, comm)
  call populate_matrices(local_A, local_x, m, local_m, n, local_n, my_id, comm)
  call matvec_multiply(local_A, local_x, local_b, local_m, local_n, n, comm)

  ! printing random element of the obtained vector from each process
  ! as the values were initialized to 1.0, the obtained vector-b should be equal to #columns, i.e., n
  write(*, 10) "Sanity check printing local_b values: b(", local_m/2, ") = ", local_b(local_m/2), " from process = ", my_id
10 format (A, I0, A, F10.3, A, I0)
  	
  call MPI_FINALIZE(ierr)
  end_time = MPI_WTIME()

  if (my_id .eq. 0) then
     write(*, 11) "Program running time = ", end_time-start_time, " with processes used = ", nprocs
11   format (A, F10.7, A, I0)
  end if
  	
  call deallocate_memory(local_A, local_x, local_b, local_m, n, local_n, comm)
	
contains
	
  subroutine read_dimensions(m, local_m, n, local_n, my_id, nprocs, comm)
    implicit none

    integer :: my_id, nprocs, ierr, comm
    integer :: m, local_m, n, local_n
	
    if (my_id .eq. 0) then
       write(*, '(A)') "Enter number of rows: "	        ! m should be evenly divisible by nprocs and m > 0
       read(*, *) m
       write(*, '(A)') "Enter number of columns: "	! n should be evenly divisible by nprocs and n > 0
       read(*, *) n
    end if
		
    call MPI_BCAST(m, 1, MPI_INTEGER, 0, comm, ierr)
    call MPI_BCAST(n, 1, MPI_INTEGER, 0, comm, ierr)
	
    if ((m .le. 0) .OR. (n .le. 0) .OR. (MOD(m, nprocs) .ne. 0) .OR. (MOD(n, nprocs) .ne. 0)) then	! check for any input errors
       if (my_id .eq. 0) write(*, '(A)') "Please enter correct dimension or number of MPI processes. Exiting!!"
       call MPI_FINALIZE(ierr)
       STOP
    end if
		
    local_m = m / nprocs
    local_n = n / nprocs
	
  end subroutine read_dimensions
	
  subroutine allocate_memory(local_A, local_x, local_b, local_m, n, local_n, comm)
    implicit none

    double precision, dimension(:, :), allocatable :: local_A
    double precision, dimension(:), allocatable :: local_x, local_b
    integer :: local_m, n, local_n, comm
    
    allocate (local_A(0:local_m-1, 0:n-1))
    allocate (local_x(0:n-1), local_b(0:local_m-1))
	
  end subroutine allocate_memory

  subroutine deallocate_memory(local_A, local_x, local_b, local_m, n, local_n, comm)
    implicit none

    double precision, dimension(:, :), allocatable :: local_A
    double precision, dimension(:), allocatable :: local_x, local_b
    integer :: local_m, n, local_n, comm
	
    deallocate (local_A)
    deallocate (local_x, local_b)
	
  end subroutine deallocate_memory
	
  subroutine populate_matrices(local_A, local_x, m, local_m, n, local_n, my_id, comm)
    implicit none

    integer :: m, local_m, n, local_n, my_id, comm, ierr
    double precision :: local_A(0:local_m-1, 0:n-1)
    double precision :: local_x(0:local_n-1)
    double precision, dimension(:, :), allocatable :: matA
    double precision, dimension(:), allocatable :: vec
    
    if (my_id .eq. 0) then
       allocate (matA(0:m-1, 0:n-1))
       allocate (vec(0:n-1))
   
       matA = 1.0d0                 ! for simplicity and sanity check, it is kept as 1.0
       vec = 1.0d0
     
       call MPI_SCATTER(matA, local_m*n, MPI_DOUBLE_PRECISION, local_A, local_m*n, MPI_DOUBLE_PRECISION, 0, comm, ierr)
       call MPI_SCATTER(vec, local_n, MPI_DOUBLE_PRECISION, local_x, local_n, MPI_DOUBLE_PRECISION, 0, comm, ierr)
		
       deallocate(matA)
       deallocate(vec)
    else 
       call MPI_SCATTER(matA, local_m*n, MPI_DOUBLE_PRECISION, local_A, local_m*n, MPI_DOUBLE_PRECISION, 0, comm, ierr)
       call MPI_SCATTER(vec, local_n, MPI_DOUBLE_PRECISION, local_x, local_n, MPI_DOUBLE_PRECISION, 0, comm, ierr)
    end if
	
  end subroutine populate_matrices

  subroutine matvec_multiply(local_A, local_x, local_b, local_m, local_n, n, comm)
    implicit none

    integer :: local_m, local_n, n, comm, ierr
    double precision :: local_A(0:local_m-1, 0:n-1)
    double precision :: local_x(0:local_n-1), local_b(0:local_m-1)
    double precision, dimension(:), allocatable :: global_x
    integer :: i, j

    allocate (global_x(0:n-1))
    call MPI_ALLGATHER(local_x, local_n, MPI_DOUBLE_PRECISION, global_x, local_n, MPI_DOUBLE_PRECISION, comm, ierr)

    do j = 0, local_m-1
       local_b(j) = 0.0d0
       do i = 0, n-1
          local_b(j) = local_b(j) + global_x(i) * local_A(i, j)
       end do
    end do

    deallocate(global_x)

  end subroutine matvec_multiply
  
end program matvecMultiplication
