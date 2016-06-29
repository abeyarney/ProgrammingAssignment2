# This function creates a matrix object
# The function caches the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y){
  x <<- y
  inv <<- NULL
  
  }
  
  get <- function()x
     setInverse <- function(inverse)inv <<- inverse
          getInverse <- function() inv
list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  }


# This function computes the inverse of a "matrix" returned by makeCacheMatrix above. 
# It checks to see if the inverse already exists.
# The inverse has already been calculated (and the matrix has not changed), 
# It then retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
    inv <- x$getInverse()
        if (!is.null(inv)){
                message("getting cached data")
                     return(inv)
        }
        matr <- x$get()
            inv <- solve(matr,...)
                 x$setInverse(inv)
        inv
}
