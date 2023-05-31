Problem Description:  

-> Consider the calculation of the derivative of the following function,  
$$f(x) = sin(5x), \hspace{4mm} 0 \leq x \leq 3$$  
using $4^{th}$-order accurate central difference (E4) scheme for the interior and one-sided forward/backward schemes for near boundary points.    
-> For interior points, the E4 scheme is given as:  
$$f'_i = \frac{\left( -f_{i+2} + 8 f_{i+1} - 8 f_{i-1} + f_{i-2} \right)}{12 \Delta x} $$  

-> For near boundary points, use the following one-sided schemes:  
$$f^{\prime}_i = \frac{f_{i+1} - f_{i}}{\Delta x}, \hspace{4mm} f^{\prime}_i = \frac{f_{i} - f_{i-1}}{\Delta x} $$    

where $\Delta x$ is the grid spacing, and $n$ is the number of grid points in $x$ direction.  
-> The analytical solution is plotted with the obtained numerical solution.  
