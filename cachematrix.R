## Put comments here that give an overall description of what your
## functions do
## 1. makeCacheMatrix defines all the functions to call on the matrix as well as store the inverse
## 2. cacheSolve is called passing the return from makeCacheMatrix
## The cacheSolve will check if the inverse exists, then it would get the cached version of it
## else it will computer the inverse and store it in the makeCacheMatrix

## Write a short comment describing this function
## Expose functions to get, and set values as well as get, and set inverse values
makeCacheMatrix <- function(x = matrix()) {
  inv_x<-NULL
  set <- function(y){
    x<<-y
    inv_x<<-NULL  
  }
  get <- function() x
  setinverse <- function(inverse) inv_x <<- inverse
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## call getinverse function and if null, set the value and return the inverse matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinverse()
  if (!is.null(inv_x)) {
    message("getting cached inverse matrix")
    return(inv_x)
  } else {
    inv_x <- solve(x$get(),...)
    x$setinverse(inv_x)
    return(inv_x)
  }
}
