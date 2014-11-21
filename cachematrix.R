## Function returns inverse of matrix  
## first checks in cache if present otherwise solve and returns


## Caches inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ##Check if it is a square matrix else break out of code
  if(nrow(x) != ncol(x)){
    stop("matrix is not a square matrix")
  }
  set <- function(y) {    
    x <<- y
  }
  get <- function() x
  setinverse <- function(myinv) inv <<- myinv
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Tries to find in cache if not creates and store it. 
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  data
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
