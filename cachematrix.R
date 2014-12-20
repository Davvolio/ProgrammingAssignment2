## This function create a list of functions that allow manipulations with matrix "x"
## You can set and get the value of matrix, 
## Also you can get and set inverse value of matix
## When you initialise it, only set is called, and you need to pass a ready matrix
## something like "zz<-makeCacheMatrix (matrix(rnorm(9),3,3))"
## But beware of using matrixes like "matrix(1:9,3,3)", because you will get an error:
## "system is exactly singular: U[3,3] = 0" and no inversion will be possible

makeCacheMatrix <- function(x = matrix()) {

  local <- NULL                                       # local is omnipresent variable that will transport the values between the functions, according to the function called
  set <- function(y) {                                # here we set the matrix "x"
    x <<- y
    local <<- NULL
  }
  get <- function() x                                 # here we get the meaning of matrix "x" when time comes to inversion
  
  setinversematrix <- function(solve) local <<- solve #after inversion put new matrix here
  getinversematrix <- function() local                #calling the inverted matrix from cache if original matrix was already inverted before
  
  list(set = set, get = get,                          # creating a list of 4 functions
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


## This function first check if new matrix was already inverted. 
## If yes, directly put the inverted matrix from cache
## If no, calculate the inverted matrix and put it to the cache
## I've added some messages so you can see the workflow of code and 
## that both matrixes (original and inverted) was visible during execution (if you will execute it out of curiousity:))
## As a starting parameter it need the result of execution of previous makeCacheMatrix function
## For example if "zz<-makeCacheMatrix (matrix(rnorm(9),3,3))" then "cacheSolve(zz)"

cacheSolve <- function(x, ...) {
  local <- x$getinversematrix()                  # first step, check the inverted matrix
  if(!is.null(local)) {                          # if there is cached matrix came here
    message("we have the cache of this matrix:") # message to show original matrix
    rawmatrix <- x$get()                         # getting original matrix from makeCacheMatrix function
    print (rawmatrix)
    message ('and here it is!')
    return(local)                                # returning the inverted matrix from cache
  }
  rawmatrix <- x$get()                           # came here if no cache found, for inversion take original matrix from makeCacheMatrix function
  local <- solve(rawmatrix, ...)                 # inverse matrix using the recommened "solve" function
  x$setinversematrix(local)                      # putting to cache inverted matrix in makeCacheMatrix function
  message ('no cache was found for matrix...')
  print (rawmatrix)
  message ('so we calculated it here!')
  local                                          # Return a matrix that is the inverse of 'x'

}
