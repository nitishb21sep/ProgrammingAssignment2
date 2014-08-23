

makeCacheMatrix <- function(x = matrix()) { ## This function creates a list object
  i <- NULL       ##i(final answer) is initialized to NULL 
                  ##as soon as makeCacheMatrix function is called
 
  set <- function(y) { 
    x <<- y
    i <- NULL
  }
  get  <- function() {x}
  setinverse <- function(inverse)
    { i <<- inverse}
  getinverse <- function()
  {i}
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
i <- x$getinverse()
if(!is.null(i)) {
  message("getting cached data")
  return(i)
}
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
mat <- matrix(10:18, nrow=3, ncol=3)
matobject <- makeCacheMatrix(mat)