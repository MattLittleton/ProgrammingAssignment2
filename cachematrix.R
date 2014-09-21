## Allow for the caching (and retrival) of the inverse of a matrix

##  creates a variable, i, that allows for caching the inverse of an input matrix, x,
##  and defines the functions to set and retrun i

makeCacheMatrix <- function(x = matrix()) {
  #sets value of i (inverse) to NULL
  i <- NULL
  
  #sets value of x (outside of function) to function input
  #sets value of i (outside of function) to NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  #returns value of x
  get <- function() x
  
  #sets value of i (outside of setinverse) to paramater inverse
  setinverse <- function(inverse) i <<- inverse
  
  #retuns value of i
  getinverse <- function() i
  
  #returns a vector of functions above functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## determines if the inverse of a matrix has been cached, and returns the value if so
## if not, calculates the value of the inverse and caches it

cacheSolve <- function(x, ...) {
  #return the value of getinverse, which will be populated if it has been previously calculated 
  i <- x$getinverse()
  
  #if i has a value it has been previously calculated, so return the message and cached value of i
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  #since i has no value, it has yet to be calculated, so get the original matrix, x
  data <- x$get()
  #calculate the inverse
  i <- solve(data)
  #cache the result for later use
  x$setinverse(i)
  #return the inverse
  i
}