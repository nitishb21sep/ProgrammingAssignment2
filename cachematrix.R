## makeCacheMatrix function creates an object. The object thus created 
##have 4 functions (get, set, getinverse, setinverse) which are also 
## called as object methods. The 3 object methods(get, getinverse, 
## setinverse) will be called by another function named cacheSolve. 
## cacheSolve takes input as the object created by makeCacheMatrix. 
## It then checks if the inverse is already calculated by getinverse 
## function. If it is already there, it simply gets the value 'i' from 
## getinverse funtion. If not, then it calcuates inverse 
## and store that new inverse in x setinverse.    

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL       
 
  set <- function(y) { 
    x <<- y
    i <- NULL
  }
  get  <- function() {x} ## returns the value of original vector
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
mat <- matrix(1:4, nrow=2, ncol=2) 
matobject <- makeCacheMatrix(mat)