# Function for the matrix cache object
# Function for create matrix object with setter and getter, 
# and cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  mx <- NULL
  
  set <- function(y) {
    x <<- y
    mx <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) mx <<- inverse
  
  getinverse <- function() mx
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# Function to caculate the inverse if not still cached
#and return the calculation from the cache object

cacheSolve <- function(x) 
  {
  mx <- x$getinverse()
  
  if(!is.null(mx)) { # data already in cache
    message("getting cached data")
    return(mx)
  }
  
  data <- x$get()
  mx <- solve(data)
  x$setinverse(mx)
  mx
}