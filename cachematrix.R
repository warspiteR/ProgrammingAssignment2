## Put comments here that give an overall description of what your
## functions do

##The first function, makeCacheMatrix creates special "matrix" object that can cache its inverse. 
## solve and inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(i) inv <<- i
  getInv <- function() inv
  
  #exposes the 4 functions 
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  ## check cache
  if(!is.null(inv)) {
    print("getting cached data")
    return(inv)
  }
  data <- x$get()
  ## the actual inversion of x
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}

##Tests to validate
##A <- matrix(1:4,nrow=2,ncol=2)
##myTest<-makeCacheMatrix(A)
##myTest$get()
##cacheSolve(myTest)
##cacheSolve(myTest)

