## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


# Un par de funciones que almacenan en caché la inversa de una matriz.


# Creo un objeto de matriz especial que puede almacenar en caché su inverso

makeCacheMatrix <- function( m = matrix() ) {
  
  # Initialize the inverse property
  i <- NULL
  
  # Method to set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  # Method the get the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  # Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  # Method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  
  
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  # Get the matrix from our object
  data <- x$get()
  
  # Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  # Return the matrix
  m
}


####FINISH