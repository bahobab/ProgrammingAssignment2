  ## Write a short comment describing this function:
  ## this function takes in a reversible matrix then gets the value of the matrix,
  ## sets the reverse of the matrix, gets the reverse of the matrix
  
  makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
      x <<- y
      i <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
  }
  
  ## Write a short comment describing this function
  ## this function takes takes in a special matrix then checks the cache wheither
  ## it contains the inversed matrix then returns it.
  ## If the inversed is not in the cache then it is computed
  
  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)){
      message("Getting inversed matrix")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
  }