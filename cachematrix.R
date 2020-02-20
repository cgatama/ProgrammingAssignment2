## Put comments here that give an overall description of what your
## functions do
#~~~~~~~
##The makeCacheMatrix function creates a special "matrix" object that can cache its inverse
##The set(); sets the value of the matrix
#The get();gets the value of the matrix
#The setinverse(); sets the value of the inverse of the matrix
# The getinverse();gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
#~~~~~~~~~~~~~~
#cacheSolve() function computes the inverse of the special “matrix” 
#returned by makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
