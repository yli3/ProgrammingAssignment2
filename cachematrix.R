## makeCacheMatrix
##  
##  Accepts a passed square invertible matrix, and creates a special object
##  which caches both that matrix, and its inverse in the resultant closure. 
##  These cached values are accessible to set/get functions returned by
##  makeCacheMatrix via the global assignment operator <<-.
##
##  Args: 
##    x = square invertible matrix (empty by default)
##  Data:
##    x = cached a square invertible matrix
##    inv = cached inverse matrix (NULL by default)
##  Returns:
##    function list comprising:
##      - getInverse (obtains inverse of x, if available)
##      - setInverse (sets inverse of x)
##      - get (obtains x)
##      - set (sets x)
##  
##  Error Handling:
##    None. A square invertible matrix x is assumed, but the class of x
##    is not enforced.

makeCacheMatrix <- function(x = matrix()) {
  # Initialize inverse result to NULL.
  inv <- NULL
  
  set <- function(y) {
    # Sets x and resets inv to NULL.
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {
    # Returns x.
    x
  }
  
  setInverse <- function(m) {
    # Sets inverse of x. 
    inv <<- m
  }
  
  getInverse <- function() {
    # Returns inverse of x.
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
##  This function is passed a return object from makeCacheMatrix and returns 
##  the inverse of the square invertible matrix x within that object's closure.
##
##  If no stored inverse exists within the passed closure, a new inverse will 
##  be calculated using the stored matrix.
##  
##  If a stored inverse already exists, then it will be retrieved and returned.
##
##  Args:
##    x = a return of makeCacheMatrix
##    ... = further arguments to pass on, if necessary
##  Returns:
##    inverse matrix of x
##
##  Error Handling:
##    None. A square invertible matrix is always assumed.

cacheSolve <- function(x, ...) {
  
  # Retrieve calculated inverse, if it already exists.
  inv <- x$getInverse()
  
  # If inv does not yet exist, calculate and set inv.
  if(is.null(inv)) {
    
    inv <- solve(x$get(), ...) # solve may accept further ... args.
    
    x$setInverse(inv)
    
  } else {
    
    # Inverse has already been calculated.
    message("...retrieving cached inverse value...")
    
  }
  
  # Returns non-NULL inverse.
  inv
  
}
