#function that returns the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set <- function(y) {
    x <<- y
    invert <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invert <<- inverse
  getInverse <- function() invert
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#now the function that returns the inverse if it has been calculated previously

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invert <- x$getInverse()
  if (!is.null(invert)) {
    message("getting cached data")
    return(invert)
  }
  matr <- x$get()
  invert <- solve(matr, ...)
  x$setInverse(invert)
  invert
}