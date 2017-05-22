##To avoid repetitive complex computation we can cache the value of the computation.the following functions will
##return the inverse of a mtrix from cached data by taking advantage of lexical scoping and in case the inverse 
##is not available from cached data they will compute the inverse of the matrix.


## MakeCacheMatrix function will return a list of functions to -
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve function will try to get the inverse of a mtrix from the cached data.
## the function will compute the inverse of the matrix in case cached value is not available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
