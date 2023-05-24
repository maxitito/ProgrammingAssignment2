## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inversem <- NULL
  set <- function(y) {
    x <<- y
    inversem <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inversem <<- solve
  getinverse <- function() inversem
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversem <- x$getinverse()
  if(!is.null(inversem)) {
    message("getting cached data")
    return(inversem)
  }
  data <- x$get()
  inversem <- solve(data, ...)
  x$setinverse(inversem)
  inversem
}
