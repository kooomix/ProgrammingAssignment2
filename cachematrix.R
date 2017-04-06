## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  #Set the matrix
  set <- function(y) {
    
    #If the matrix is different than the existing one, clean cached matrix
    if (!identical(x,y)) i <<- NULL
    x <<- y
    i <<- NULL
  }
  
  #return the set matrix
  get <- function() x
  
  #solve the matrix
  setInverted <- function(solve) i <<- solve
  
  #return the solved matrix
  getInverted <- function() i
  
  
  list(set = set, get = get,
       setInverted = setInverted,
       getInverted = getInverted)

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverted()

  ## If inverse was solved, return it from cache
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## If inverse wasn't solve, solve it now.
  data <- x$get()
  i <- solve(data, ...)
  x$setInverted(i)
  i
}
