## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  ## Set the inverse matrix
  setinversematrix <- function(inversematrix) m <<- inversematrix
  
  ## Get the Inverse Matrix
  getinversematrix <- function() m
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Get the inverse matrix from Cache and if exists, return it
  m <- x$getinversematrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## inverse matrix does not exist or the matrix changed. Compute the inverse of the matrix and return.
  data <- x$get()
  m <- solve(data, ...)
  x$setinversematrix(m)
  m
}
