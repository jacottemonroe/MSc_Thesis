## MSc Thesis 
## Jacotet Monroe 

## Function elephant modeling script

# Function takes initial (x,y) coordinates, a step length and turning angle. The position is shifted by doing basic trigonometry.
# The coordinates of the new position are returned.  


calculateNewPoint <- function(x, y, step, ta){
  # source: https://www.danaseidel.com/MovEco-R-Workshop/Materials/Day7/Simulating_Movement/
  # the turning angle is from -pi to pi --> translate to range [0, 2pi]
  if (ta < 0) {
    ta <- abs(ta) + pi
  }
  
  # calculate new x (following basic trigonometry --> calc adjacent edge when step = hyp and ta = angle)
  new_x <- x + (cos(ta) * step)
  
  # calculate new y 
  new_y <- y + (sin(ta) * step)
  
  return(c(new_x, new_y))
}