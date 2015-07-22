## makeCacheMatrix
##  
##  Creates a matrix object that can cache its inverse value.
##
##  Args: 
##    x = matrix (empty by default)
##  Returns:
##    function list comprising:
##      - getInverse (obtains inverse of x, if available)
##      - setInverse (sets inverse of x)
##      - get (obtains x)
##      - set (sets x)
##
##  Error Handling:
##    None.

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize inverse result to NULL.
  inv <- NULL
  
  set <- function(y) {
    ## Sets x and resets inv to NULL.
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {
    ## Returns x.
    x
  }
  
  setInverse <- function(m) {
    ## Sets inverse of x. 
    inv <<- m
  }
  
  getInverse <- function() {
    ## Returns inverse of x.
    inv
  }
  
  # Return function list.
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## cacheSolve
##
##  Calculates and stores the inverse of the matrix obj returned by 
##  makeCacheMatrix.
##  
##  If a calculated inverse exists and the matrix has not been modified,
##  the existing result is retrieved. 
##  
##  Args:
##    x = a return of makeCacheMatrix
##    ... = further arguments to pass on, if necessary
##  Returns:
##    inverse matrix of x
##
##  Error Handling:
##    None. An invertible matrix is always assumed.

cacheSolve <- function(x, ...) {
  
  ## Retrieve calculated inverse, if it already exists.
  inv <- x$getInverse()
  
  ## If inv does not yet exist, calculate and set inv.
  if(is.null(inv)) {
    
    ## Retrieve matrix and calculate; solve can take further args.
    m <- x$get()
    inv <- solve(m, ...)
    
    ## Now set the inverse.
    x$setInverse(inv)
    message("...calculating and setting inverse...")
    
  } else {
    
    ## Output message indicating calculated inv exists,
    ## and is merely being retrieved.
    message("...retrieving cached inverse value...")
    
  }
  
  ## Returns non-NULL inverse.
  inv
  
}
