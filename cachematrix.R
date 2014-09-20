## Computes the inverse of a matrix and caches it, so that it can be retrieved (to avoid repeated computation)
## 

## Creates a special "matrix" object that stores a matrix x and cache's its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
   x <<- y
   i <<- NULL
 }
 get <- function() x
 setInv <- function(inverse) i <<- inverse
 getInv <- function() i
 list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Returns the inverse of a "matrix" object x. If the inverse stored in x has not already been computed, this function computes and cache's it. Otherwise returns the cached value of the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInv()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  y <- x$get()
  i <- solve(y)
  x$setInv(i)
  i
  
}
