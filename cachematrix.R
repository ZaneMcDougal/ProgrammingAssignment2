## creates a Matrix with a storable inverse value to 
## prevent unnecessary/time comsuming processsing

## makeCacheMatrix creates a matrix with storable inverse value
## acutally a list with function calls


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse<- function(solve) i <<- solve
  getinverse<- function() i
  
  list( set = set, get = get, 
        setinverse = setinverse,
        getinverse = getinverse)
  
  
}


##cacheSolve returns the inverse of the matrix 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  ##returns without solving again
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  ##if not previsously solved then solves for inverse 
  ## and stores value
  
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}
