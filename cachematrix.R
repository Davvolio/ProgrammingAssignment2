## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversematrix <- function(solve) m <<- solve
  getmean <- function() m
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getmean = getmean)
}

zz<-makeCacheMatrix (matrix(rnorm(9),3,3))
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversematrix(m)
  m
  ## Return a matrix that is the inverse of 'x'
}

cacheSolve(zz)
