! MPI parallelized version of trapezoidal rule using MPI derived data types
program v3_parallel_trap_rule

implicit none
include "mpif.h"
	
	double precision :: a, b, integration_result, local_a, local_b, local_sum, h
	integer :: n, local_n, my_id, nprocs, i, status(MPI_STATUS_SIZE), ierr
	
	call MPI_INIT(ierr)
	
	call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD, my_id, ierr)
	
	call read_user_input(my_id, nprocs, a, b, n)
	integration_result = 0.0d0
	
	h = (b - a) / n
	local_n = n / nprocs		! nprocs evenly divides with the number of divisions 
	local_a = a + my_id * local_n * h
	local_b = local_a + local_n * h
	call trap_rule(local_a, local_b, local_n, h, local_sum)
	
	call MPI_REDUCE(local_sum, integration_result, 1, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
	
	if (my_id .eq. 0) then
		write(*, 100) "The integration for the given function between limits", a, " and", b, " =", integration_result
		100 format (A, F10.5, A, F10.5, A, F15.7)
	end if
	
	call MPI_FINALIZE(ierr)
	
contains
	
	subroutine read_user_input(my_id, nprocs, a, b, n)
	implicit none
		
		integer :: my_id, nprocs, new_mpi_type, n
		double precision :: a, b
		
		call create_new_mpi_type(a, b, n, new_mpi_type)
		
		if (my_id .eq. 0) then
			write(*, *) "Enter values of a, b, n:"
			read(*, *) a, b, n
		end if
		
		call MPI_BCAST(a, 1, new_mpi_type, 0, MPI_COMM_WORLD, ierr)	! Broadcast values to all other processes in the communicator
		call MPI_TYPE_FREE(new_mpi_type, ierr)	! free the memory of new mpi datatype  
		
	end subroutine read_user_input
	
	subroutine create_new_mpi_type(a_p, b_p, n_p, new_mpi_type)	! subroutine to create a new mpi datatype
	implicit none
		
		double precision :: a_p, b_p
  		integer :: n_p, new_mpi_type
  		integer :: block_lengths(0:2), types(0:2)
  		integer(kind = MPI_ADDRESS_KIND) :: a_addr, b_addr, n_addr
  		integer(kind = MPI_ADDRESS_KIND) :: displacements(0:2)
  		
  		call MPI_GET_ADDRESS(a_p, a_addr, ierr)	! addresses will be corresponding to each process
  		call MPI_GET_ADDRESS(b_p, b_addr, ierr)
  		call MPI_GET_ADDRESS(n_p, n_addr, ierr)
  		
  		! offset wrt to first argument
  		displacements(0) = a_addr - a_addr
  		displacements(1) = b_addr - a_addr
  		displacements(2) = n_addr - a_addr
  		
  		block_lengths = (/1, 1, 1/)
  		types = (/MPI_DOUBLE_PRECISION, MPI_DOUBLE_PRECISION, MPI_INTEGER/)
  		
  		call MPI_TYPE_CREATE_STRUCT(3, block_lengths, displacements, types, new_mpi_type, ierr)	! creating new MPI structure
  		call MPI_TYPE_COMMIT(new_mpi_type, ierr)	! commit new MPI datatype for MPI's bookkeeping
		
	end subroutine create_new_mpi_type
	
	subroutine trap_rule(x_s, x_e, n, h, local_result)
	implicit none
	
		double precision :: x_s, x_e, h, local_result, x
		integer :: n, i
		
		local_result = (func(x_s) + func(x_e)) / 2.0d0
		do i = 1, n-1
			x = x_s + i * h
			local_result = local_result + func(x)
		end do
		
		local_result = local_result * h
	
	end subroutine trap_rule
	
	function func(x) result(y)
	implicit none
		
		double precision :: x, y
		y = 1.0d0 + sin(x)     ! given function to integrate
		
	end function func

end program v3_parallel_trap_rule
