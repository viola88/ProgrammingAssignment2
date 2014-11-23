## The functions shall help avoiding time-consuming computations of the inverse of an matrix
## The idea is to use the inverse from the cache if it has already been computed before
## For this we need two functions, makeCacheMatrix and cacheSolve

## makeCacheMatrix creates a special matrix by doing the following
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. get the value of the inverse
## 4. set the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m  <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}

## cacheSolve calculates the inverse of the special matrix created above but 
## only if that inverse is not in the cache, otherwise returns value from cache

cacheSolve <- function(x,...) {
  m <- x$getinverse()
  if (!is.null(m)){
    message ("getting cached inverse")
    return(m)
  }
  data  <- x$get()
  m <- solve(x,...)
  x$setinverse(m)
  m
}
