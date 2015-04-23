## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object
##that can cache its inverse.  The matrix should be a square matrix,e.g.nxn, 
##with nrow=n,and rcol=n.  Also it should be inversible.  So when you test with sample
## data, make sure the sample data (an nxn matrix) should be inversible.  To garantee the 
## inversible nature of the matrix, suggest to create a matrix using random numbers,e.g
#mat <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n)
#matCached <- makeCacheMatrix(mat)
#matSolved1 <- cacheSolve(matCached)
#matSolved1
#matSolved2 <- cacheSolve(matCached)
#matSolved2
#then specify a value for n, e.g. n<-3, before you the above commands.

makeCacheMatrix <- function(x = matrix(data,nrow=n,ncol=n)) { 
  n <-c()
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inmatrix) inv <<- inmatrix
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## This function computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.
## Again, if the matrix is not inversible, an error message will be thrown at line
## "inv <- solve(data, ...)", so carefully choose a good set of sample data, as I indicate
## in makeCacheMatrix function

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

