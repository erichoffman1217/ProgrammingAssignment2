## Eric Hoffman's Copy
## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix creates a list or a special matrix that will store the original matrix
## and the inverse matrix in the get() and getinverse() arguments. The getinverse()
## will remain NULL until after the cacheSolve has been run on the created list/list matrix


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The cacheSolve function first checks to see if there is an inverse cached in the 
## input list created by makeCacheMatrix. If it is not, the second part of cacheSolve 
## creates the inverse function, prints it, and saves it in the input.
## Run a second time on the same input, the function will only look at the cache
## and not solve for the inverse.

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
      message("getting cached data")
      return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}

## Testing
a<-makeCacheMatrix(matrix(rnorm(49), nrow=7, ncol=7))
a$get()
a$getinverse()
cacheSolve(a)
#second time
cacheSolve(a)

