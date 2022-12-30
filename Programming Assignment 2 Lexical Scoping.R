#Matrix object that can cache inverse
makeCacheMatrix <- function(x = matrix()) {
 
   #Initialize inverse property
  i <- NULL
  
  #Set matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  #Get matrix
  
  get <- function() x
  
  #Set inverse of matrix
  setinverse <- function(inverse) i <<- inverse
  
  #Get inverse of matrix
  getinverse <- function() i
 
  #List of methods
   list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Compute the inverse of the special matrix returned by "makeCacheMatrix" above. If the inverse has already been calculated (and the matrix has not changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
 
  #Return a matrix, inverse of x
   i <- x$getinverse()
  
   #Return inverse if already set
   if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
 
   #Get matrix
    data <- x$get()

    #Calculate inverse
      i <- solve(data, ...)
 
      #Set inverse
       x$setinverse(i)
 
       #Return matrix
        i
}