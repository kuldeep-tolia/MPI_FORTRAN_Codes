! Simple broadcast demonstration program in MPI
program broadcast_call

implicit none
include "mpif.h"

	integer :: my_id, nprocs
	integer :: buff = 0
	integer :: status(MPI_STATUS_SIZE), ierr
	
	call MPI_INIT(ierr)
	
	call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD, my_id, ierr)
	
	if (my_id .eq. 0)	buff = 123
	
	write(*, *) "Before broadcasting, process-id =", my_id, " buff value =", buff
	call MPI_BARRIER(MPI_COMM_WORLD)
	
	call MPI_BCAST(buff, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
	write(*, *) "After broadcasting, process-id =", my_id, " buff value =", buff
	
	call MPI_FINALIZE(ierr)

end program broadcast_call
