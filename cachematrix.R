## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  local <- NULL
  set <- function(y) {
    x <<- y
    local <<- NULL
  }
  get <- function() x
  
  setinversematrix <- function(solve) local <<- solve
  getinversematrix <- function() local
  
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}

zz<-makeCacheMatrix (matrix(rnorm(9),3,3))

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  local <- x$getinversematrix()
  if(!is.null(local)) {
    message("we have the cache of this matrix:")
    rawmatrix <- x$get()
    print (rawmatrix)
    message ('and here it is!')
    return(local)
  }
  rawmatrix <- x$get()
  local <- solve(rawmatrix, ...)
  x$setinversematrix(local)
  message ('no cache was found for matrix...')
  print (rawmatrix)
  message ('so we calculated it here!')
  local
  ## Return a matrix that is the inverse of 'x'
}

cacheSolve(zz)

