##cache the inverse of a matrix
##creates a Null "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setM <- function(inverse) i <<- inverse
  getM <- function() i
  list(set = set, get = get,
       setM = setM,
       getM = getM)
}


##retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  i <- x$getM()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setM(i)
  i
  ## Return a matrix that is the inverse of 'x'
}