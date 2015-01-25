##This function creates a matrix, assumed to be square, and calculates
##  the inverse of that matrix, an expensive operation, and stores the
##  the result for later use.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    return(x)
  }
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  getInverse <- function(mat) {
    matequal <- function(x, y) {
      is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
    }
    if (matequal(mat, x)) {
	    return(inv)
    } else {
      inv <<- NULL
      return(inv)
    }
  }
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}

##This function performs the actual work of calculating the inverse of
##  the matrix, if it hasn't been, and returning the result.

cacheSolve <- function(x, ...) {
  mat <- x$get()
  inverse <- x$getInverse(mat)
  if (!is.null(inverse)) {
    message("Getting stored data")
    return(inverse)
  } else {
    inverse <- solve(mat)
    x$setInverse(inverse)
    return(inverse)
  }
}