! An example for an SPMD program by the use of if-else conditional statements
program hello_world_send_recv

  implicit none
  include "mpif.h"
  
  integer :: i, my_id, nproc, ierr, tag, status(MPI_STATUS_SIZE)
  character (len = 50) :: send_buff, recv_buff
  
  call MPI_INIT(ierr)
  
  call MPI_COMM_SIZE(MPI_COMM_WORLD, nproc, ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, my_id, ierr)
  
  tag = 100

  WRITE(send_buff, *) "Hello from process id =", my_id
  CALL MPI_SEND(send_buff, 50, MPI_CHARACTER, 0, tag+my_id, MPI_COMM_WORLD, ierr) ! A process can send to itself as well

  IF (my_id .EQ. 0) THEN
     
     DO i = 0, nproc-1

        CALL MPI_RECV(recv_buff, 50, MPI_CHARACTER, i, tag+i, MPI_COMM_WORLD, status, ierr)
        WRITE(*, *) recv_buff

     END DO
     
  END IF
    
  call MPI_FINALIZE(ierr)
  
end program hello_world_send_recv
