! Serial program to calculate the first derivative numerically using 4th order accurate explicit central difference scheme

program main

  implicit none

  integer, parameter :: N = 25  ! set number of grid points
  integer:: i

  double precision :: h, xmin, xmax, x
  double precision, dimension(:), allocatable :: f, df
  double precision :: start_t, end_t

  character(50) :: filename

  xmin = 0.0d0
  xmax = 3.0d0
  h = (xmax - xmin) / N         ! calculate grid spacing

  ! allocate memory
  allocate (f(0:N), df(0:N))
  
  call cpu_time(start_t)
  ! calculate given function analytically
  do i = 0, n
     x = xmin + i * h
     f(i) = sin(5.0d0 * x)
  end do

  ! calculate derivative using 4th order CDS for interior points and 1st order scheme at boundary nodes
  do i = 0, n
     if ((i .eq. 0) .OR. (i .eq. 1)) then
        df(i) = (f(i+1) - f(i)) / h
     else if ((i .eq. n-1) .OR. (i .eq. n)) then
        df(i) = (f(i) - f(i-1)) / h
     else
        df(i) = (-f(i+2) + 8.0d0 * f(i+1) - 8.0d0 * f(i-1) + f(i-2)) / (12.0d0 * h)
     end if
  end do

  write(filename, '(A, I2, A)') "a2q1a_first_derivative_", N, ".txt"
  open(UNIT = 11, FILE = filename)
  do i = 0, n
     x = xmin + i * h
     write(11, *) x, 5.0d0 * cos(5.0d0 * x), df(i)
  end do
  close(11)

  call cpu_time(end_t)
  write(*, '(A, F8.6, A)') "Program running time = ", end_t - start_t, " seconds"
  
end program main
