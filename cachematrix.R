## The two functions included in this program can be used in
## tandom to cache the inverse of a matrix rather than compute 
## it repeatedly throughout a session. The matrix argument for
## the function MakeCacheMatrix must be an invertible square
## matrix. The input argument for the function cacheSolve must
## be an object of type makeCacheMatrix().


## The function makeCacheMatrix creates an object that can
## store the inverse of the matrix argument.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
}

## The function cacheSolve checks if the inverse for the
## has already been calculated. If so, the existing solution 
## is returned. If not, the solution is calculated and returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)){
    message("getting cached inverse matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinv(i)
  i
}
