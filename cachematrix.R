##  Functions for creating and using inverted matrices which caching ability
##  makeCacheMatrix functions stores a matrix X in memory
##  cacheSolve creates inverse of a matrix if is in memory or computes the inverse and then shows the inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  # Make sure X is a matrix
  if (!is.matrix(x)) {
    stop("No matrix given")
  }
  
    inv_mat <- NULL
    set <- function(y) {
        x <<- y
        inv_mat <<- NULL
    }
    # Functions for getting and setting cached inv. matrix value
    get <- function() X
    
    setInverse <- function(solve) inv_mat <<- solve
    
    ## 4. get the value of the inverse of the matrix
    getInverse <- function() inv_mat
   
    #return list
    list(set=set,
         get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}


## Computes the inverse of the CACHED matrix returned by makeCacheMatrix()
## then the cacheSolve() returns the cached inverse

cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
  inv_mat <- x$getInverse()
  
  # Do we have cached matrix available?
  if(!is.null(inv_mat)) {
    message("Loading cached inversed matrix")
    return(inv_mat)
  }
  
  
  # create inverted matrix
  # when no cached matrix available.
  matrix_to_inverse <- x$get()
  inv_mat <- solve(matrix_to_inverse,...)
  x$setInverse(inv_mat)
  inv_mat
  
}
