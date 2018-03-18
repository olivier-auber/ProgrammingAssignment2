## Create an object to store and access a matrix and its invert
makeCacheMatrix <- function(m = matrix())
{
  invert <- NULL
  set <- function(y) {
    m <<- y
    invert <<- NULL
  }
  get <- function() m
  setinvert <- function(i) invert <<- i
  getinvert <- function() invert
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
  
}

## Calculate and Return the invert of matrix OR return the cached invert if already calculated
cacheSolve <- function(matrix, ...) {
  invert <- matrix$getinvert()
  if(!is.null(invert)) {
    message("getting cached invert matrix")
    return(invert)
  }
  message("generating and caching invert matrix")
  data <- matrix$get()
  i <- solve(data, ...)
  matrix$setinvert(i)
  i
}
