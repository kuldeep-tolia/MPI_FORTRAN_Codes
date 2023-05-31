Problem Description:  

-> In this problem, the objective is to develope the MPI version of the Jacobi iterative method to solve a 2D Poisson equation.    
-> Consider the following Poisson equation:  
$$\nabla^2 \phi = -q; \hspace{5mm} q = 2 \left(2 - x^2 - y^2 \right); \hspace{5mm} \phi \left(\pm 1,y \right) = 0; \hspace{5mm} \phi \left(x, \pm 1 \right) = 0$$  
-> The discretized equation obtained using Jacobi method can be written as follows:  
$$\phi_{i,j}^{(k+1)} = \frac{\Delta^2}{4}q_{i,j} + \frac{1}{4} \left( \phi_{i+1,j}^{(k)} + \phi_{i-1,j}^{(k)} + \phi_{i,j+1}^{(k)} + \phi_{i,j-1}^{(k)} \right)$$  
where $\Delta = \Delta x = \Delta y$.  
-> An initial guess of $\phi(x, y) = 0$ is considered everywhere.  
-> The exact solution of the given problem is given by $\phi = (x^2-1)(y^2-1)$.  
-> One dimensional domain decomposition is performed.  
-> The analytical solution is plotted with the obtained numerical solution of $\phi$ v/s $x$ for $y=0.5$.  
-> A speedup curve has also been plotted for $N=200$ and $N = 400$ to observe the parallel performance of the MPI-parallelized Jacobi iterative method.  

Code to be uploaded soon...
