program send_receive_non_blocking_call

implicit none
include "mpif.h"

	integer :: nproc, rank, ierr, request, i
	integer :: datasize = 1000000
	integer, dimension(:), allocatable :: num1, num2
	
	allocate (num1(0:datasize-1), num2(0:datasize-1))
	
	call MPI_INIT(ierr)
	call MPI_COMM_SIZE(MPI_COMM_WORLD, nproc, ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
	
	if (nproc .ne. 2) then	
		
		write(*, *) "Use exactly 2 processes to run this code."
		call MPI_ABORT(MPI_COMM_WORLD, -1, ierr)
		
	end if
	
	if (rank .eq. 0) then	
		do i = 0, datasize-1
			num1(i) = i
		end do
	
	else if (rank .eq. 1) then	
		do i = 0, datasize-1
			num1(i) = datasize - i - 1
		end do
	
	end if
	
	call MPI_BARRIER(MPI_COMM_WORLD, ierr)
	
	if (rank .eq. 0) then	
		
		call MPI_ISEND(num1, datasize, MPI_INT, 1, 10, MPI_COMM_WORLD, request, ierr)
		call MPI_IRECV(num2, datasize, MPI_INT, 1, 11, MPI_COMM_WORLD, request, ierr)
	
	else if (rank .eq. 1) then	
		
		call MPI_ISEND(num1, datasize, MPI_INT, 0, 11, MPI_COMM_WORLD, request, ierr)
		call MPI_IRECV(num2, datasize, MPI_INT, 0, 10, MPI_COMM_WORLD, request, ierr)
	
	end if
	
	if (rank .eq. 0) then
		
		write(*, *) "process-0 has received from process-1."
	
	else if (rank .eq. 1) then	
		
		write(*, *) "process-1 has received from process-0."
		
	end if
	
	call MPI_FINALIZE(ierr)
	
	deallocate (num1, num2)	
	
end program send_receive_non_blocking_call
