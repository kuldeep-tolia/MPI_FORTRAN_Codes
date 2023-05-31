program send_receive_array

implicit none
include "mpif.h"

	integer :: siz, rank, ierr
	integer :: status(MPI_STATUS_SIZE)
	integer :: cont = 1000, i
	integer, dimension(:), allocatable :: num, num1
	
	allocate (num(0:cont-1), num1(0:cont-1))
	
	call MPI_INIT(ierr)
	call MPI_COMM_SIZE(MPI_COMM_WORLD, siz, ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
	
	if (siz .ne. 2) then	
		
		write(*, *) "Use exactly 2 processes to run this code."
		call MPI_ABORT(MPI_COMM_WORLD, -1, ierr)
		
	end if
	
	if (rank .eq. 0) then
		
		do i = 0, cont-1
			num(i) = i
		end do
		
		call MPI_SEND(num, cont, MPI_INT, 1, 11, MPI_COMM_WORLD, ierr)
		call MPI_RECV(num1, cont, MPI_INT, 1, 12, MPI_COMM_WORLD, status, ierr)
		
		do i = 0, cont-1
			write(*, *) "process-0 received num(", i, ") =", num1(i), " from process-1."
		end do
	
	else if (rank .eq. 1) then 
		
		call MPI_RECV(num1, cont, MPI_INT, 0, 11, MPI_COMM_WORLD, status, ierr)
		
		do i = 0, cont-1
			write(*, *) "process-1 received num(", i, ") =", num1(i), " from process-0."
			num(i) = cont - i - 1
		end do
		
		call MPI_SEND(num, cont, MPI_INT, 0, 12, MPI_COMM_WORLD, status, ierr)
		
	end if
	
	call MPI_FINALIZE(ierr)
	
	deallocate (num, num1)	

end program send_receive_array
