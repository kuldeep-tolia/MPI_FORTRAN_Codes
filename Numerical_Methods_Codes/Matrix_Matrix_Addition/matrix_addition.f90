! MPI parallelized version of matrix(mxn) addition
! row-wise block parallelization

module matAdd_data

  implicit none
  include "mpif.h"

  double precision, dimension(:, :), allocatable :: local_A, local_B, local_C
  double precision :: start_time, end_time

  integer :: my_id, nprocs, status(MPI_STATUS_SIZE), ierr, comm
  integer, parameter :: m = 10240   ! number of rows
  integer, parameter :: n = 10240   ! number of columns
  integer :: local_m

contains

  subroutine allocate_memory()
    implicit none

    allocate (local_A(0:local_m-1, 0:n-1), local_B(0:local_m-1, 0:n-1), local_C(0:local_m-1, 0:n-1))
    
  end subroutine allocate_memory

  subroutine deallocate_memory()
    implicit none

    deallocate (local_A, local_B, local_C)

  end subroutine deallocate_memory

  subroutine populate_matrices()
    implicit none

    double precision, dimension(:, :), allocatable :: matA, matB

    if (my_id .eq. 0) then
       allocate (matA(0:m-1, 0:n-1), matB(0:m-1, 0:n-1))

       matA = 5.0d0             ! for simplicity and sanity check, it is kept as 5.0
       matB = 5.0d0             ! for simplicity and sanity check, it is kept as 5.0

       call MPI_SCATTER(matA, local_m*n, MPI_DOUBLE_PRECISION, local_A, local_m*n, MPI_DOUBLE_PRECISION, 0, comm, ierr)
       call MPI_SCATTER(matB, local_m*n, MPI_DOUBLE_PRECISION, local_B, local_m*n, MPI_DOUBLE_PRECISION, 0, comm, ierr)

       deallocate (matA, matB)

    else
       call MPI_SCATTER(matA, local_m*n, MPI_DOUBLE_PRECISION, local_A, local_m*n, MPI_DOUBLE_PRECISION, 0, comm, ierr)
       call MPI_SCATTER(matB, local_m*n, MPI_DOUBLE_PRECISION, local_B, local_m*n, MPI_DOUBLE_PRECISION, 0, comm, ierr)
    end if
    
  end subroutine populate_matrices

  subroutine matAdd()
    implicit none

    double precision, dimension(:, :), allocatable :: global_C
    integer :: i

    local_C = local_A + local_B

    ! if (my_id .eq. 0) then              ! user can turn off the comment for printing the obtained global matrix
    !    allocate (global_C(0:m-1, 0:n-1))

    !    call MPI_GATHER(local_C, local_m*n, MPI_DOUBLE_PRECISION, global_C, local_m*n, MPI_DOUBLE_PRECISION, 0, comm, ierr)

    ! else
    !    call MPI_GATHER(local_C, local_m*n, MPI_DOUBLE_PRECISION, global_C, 0, MPI_DOUBLE_PRECISION, 0, comm, ierr)
    ! end if

    ! call MPI_BARRIER(comm, ierr)

    ! if (my_id .eq. 0) then
    !    do i = 0, m-1
    !       print*, global_C(i, :)
    !    end do
       
    !    deallocate (global_C)
    ! endif
        
  end subroutine matAdd
          
end module matAdd_data

program main

  use matAdd_data
  implicit none

  call cpu_time(start_time)
  call MPI_INIT(ierr)

  comm = MPI_COMM_WORLD
  call MPI_COMM_SIZE(comm, nprocs, ierr)
  call MPI_COMM_RANK(comm, my_id, ierr)

  local_m = m / nprocs          ! m > 0 and should be evenly divisible by nprocs
  call allocate_memory()
  call populate_matrices()
  call matAdd()

  ! printing random element of the obtained matrix from each process
  ! as the values assigned were 5.0, the obtained matrix-C should be equal 10.0
  write(*, 11) "Printing local-C values: C(", local_m/2, ")(", n/2, ") = ", local_C(local_m/2, n/2), " from process = ", my_id
11 format (A, I5, A, I5, A, F6.3, A, I2)
  
  
  call deallocate_memory()
  
  call MPI_FINALIZE(ierr)
  call cpu_time(end_time)

  if (my_id .eq. 0) write(*, '(A, F8.6, A)') "Program running time = ", end_time - start_time, " seconds"
  

end program main
