## Pair of functions that cache the inverse of a matrix

## Creates a matrix object that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) { 
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

	  get <- function() x
        setmatrix <- function(solve) m <<- solve
	  getmatrix <- function() m
	  list(set = set, get = get,
   	  	setmatrix = setmatrix,
   	  	getmatrix = getmatrix)
}


## Computes the inverse of the matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## check if inverse has already been cached
	   m <- x$getmatrix()
   	   if(!is.null(m)){
      	message("getting cached data")
      	return(m)
    	   }
	  ## if the cached inverse was not returned above,
	  ## compute the inverse 
    	   matrix <-x $get()
    	   m <- solve(matrix, ...)
    	   x$setmatrix(m)
    	   m
}
