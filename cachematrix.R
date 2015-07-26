############### BEGIN ####################

## Solving and Caching inverse matrices ##

##########################################

## Function creates a matrix object with ability to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## set the matrix
  set = function(y) {
    x <<- y
    inv <- NULL
  }
  ## get matrix object
  get = function() x
  
  ## set the object inverse
  setinv = function(inverse) inv <<- inverse
  
  ## get the inverse
  getinv = function() inv
  
  ## the return list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Function computes the inverse of matrix object:
## ## Is calculated before? Use the value in cache
## ## Never been calculated? then proceed with computation

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv = x$inv
  
  ## Has the inverse already been computed"
  if (!is.null(inv)) {
    message("Retrieving cached data...")
    return(inv)
  }
  
  ## NOt already calculated? Proceed.
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  #Set the new inverse in place
  x$setinv(inv)
  
  #Return
  return(inv)
  
}

################# END ####################














