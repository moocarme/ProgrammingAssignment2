## The first of these functions stores a matrix in the cache, then the second 
## takes the matrix from the cache and computes the inverse. This is useful
## for the computationally intensive inverse function and to have control on 
##when it operates

## makechacheMatrix initializes a matirx and stores the variables in the workspace

makeCacheMatrix <- function(x = matrix()) {
      ## Initializes a matirx and stores the variables in the workspace
      
      m <- NULL       ## Intialize matrix
      
      ## Create set function 
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      ## Create get funtion
      get <- function() x
      setInverse <- function(solve) m <<- solve  ## Set inverse function
      getInverse <- function() m                 ## Get inverse function
      
      ## Create list containing all functions
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Returns a matrix that is the inverse of x
      
      ## if the inverse is already solved do not compute
      m <- x$getInverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      
      data <- x$get()  ## Get the matrix
      m <- solve(data, ...)  ## Solve the matrix
      x$setInverse(m)
      m    ## return m from the function
}