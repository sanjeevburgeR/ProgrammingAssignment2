## This pair of function caches the inverse of the given matrix
## so that the inverse is returned instead of calculating the costly solve function

## This function caches the inverse of the given matrix x

makeCacheMatrix <- function(x = matrix()) {
              cInverseMtrx <- NULL
              
              set <- function(y){
                x <<- y
                cInverseMtrx <<- NULL
              }
              
              get <- function() x
              setInverse <- function(invMtr) cInverseMtrx <<- invMtr
              getInverse <- function() cInverseMtrx
              
              list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
              
}


## This function first checks if the inverse of the given matrix x already exists
## and returns cached inverse matrix if there is one else calculates the inverse
## and caches it for later retrieval

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invM <- x$getInverse()
        if(!is.null(invM))
        {
          message("Returning cached inverse of the matrix")
          return(invM)
        }
        
        mtrx <- x$get()
        invMtrx <- solve(mtrx)
        x$setInverse(invMtrx)
        invMtrx
}
