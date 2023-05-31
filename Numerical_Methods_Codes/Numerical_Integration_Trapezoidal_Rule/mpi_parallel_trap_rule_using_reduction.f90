! MPI parallelized version of trapezoidal rule using reduction operation
program v2_parallel_trap_rule

implicit none
include "mpif.h"
	
	double precision, parameter :: PI = 4.0d0 * atan(1.0d0)
	double precision :: a, b, integration_result, local_a, local_b, local_sum, h
	integer :: n, local_n, my_id, nprocs, i, status(MPI_STATUS_SIZE), ierr
	
	call MPI_INIT(ierr)
	
	call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD, my_id, ierr)
	
	n = 1024		! number of divisions for integration
	a = 0.0d0	! integration lower limit
	b = PI		! integration upper limit
	integration_result = 0.0d0
	
	h = (b - a) / n
	local_n = n / nprocs		! nprocs evenly divides with the number of divisions 
	local_a = a + my_id * local_n * h
	local_b = local_a + local_n * h
	call trap_rule(local_a, local_b, local_n, h, local_sum)
	
	call MPI_REDUCE(local_sum, integration_result, 1, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
	
	if (my_id .eq. 0) then
		write(*, *) "The integration for the given function between limits 0 and PI =", integration_result
	end if
	
	call MPI_FINALIZE(ierr)
	
contains
	
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
		y = 1.0d0 + sin(x)
		
	end function func

end program v2_parallel_trap_rule
