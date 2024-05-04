## MSc Thesis 
## Jacotte Monroe 

## Function elephant modeling script

# Function takes the true observations and corresponding probability density distribution, 
#   samples a random x value from uniform distribution bounded by min and max observation values (to keep it realistic), 
#   samples random y (probability) value from uniform distribution bounded by min and max of density distribution, 
#   only keeps the random x if the random y value is smaller than the probability density of x 
#   (otherwise it is not likely to occur in the actual dataset, so we reject it). Returns the random x that was accepted. 


sampleCustomDistribution <- function(observations, density_function){
  
  # create empty vector to store random value 
  random_value <- c()
  
  # loop to sample random value from distribution by means of rejection sampling 
  # method checks if random point is under the PDF (if over = omitted and new value is generated)
  while(length(random_value) < 1){
    
    # generate random x value from uniform distribution (between values from existing observations)
    # source: https://www.spsanderson.com/steveondata/posts/2023-10-30/index.html#:~:text=The%20runif()%20function%20generates,of%20random%20numbers%20to%20generate
    random_x <- runif(1, min(observations, na.rm = T), max(observations, na.rm = T))
    
    # generate random y value from uniform distribution 
    random_y <- runif(1, min(density_function$y, na.rm = T), max(density_function$y, na.rm = T))
    
    # interpolate density function for the random x
    # source: https://stackoverflow.com/questions/28077500/find-the-probability-density-of-a-new-data-point-using-density-function-in-r
    density_y <- approx(density_function$x, density_function$y, xout=random_x)$y
    
    # check the random y of random x is above the probability density y or not --> if not, accept, if so, reject
    if(random_y <= density_y){
      random_value <- append(random_value, random_x)
    }
  }
  
  return(random_value)
}