## R Programming - Programming Assignment 2 - together these functions
## create a list object that can store a matrix and its inverse
## and then get the inverse from the cache, or calculate and store it

## This function creates an object that can store a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y = matrix()){
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) inv <<- solve
     getinverse <- function() inv
     list (set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## This function checks for a cached inverse and if it doesn't find one
## it inverts the matrix using the special function list and saves it

cacheSolve <- function(x, ...) {
     inv <- x$getinverse()
     if (!is.null(inv)){
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinverse(inv)
     inv
     ## Return a matrix that is the inverse of 'x'
}
