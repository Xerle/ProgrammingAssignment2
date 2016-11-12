## This program is able to cache Inverse

## The first function makes a special "matrix", which is a list containing
## functions to set and get the matrix and functions to set and get 
## the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {

		i <- NULL
		set <- function(y)
		{
			x <<- y
			i <<- NULL
		}
		get <- function() x
		setInverse <- function(inverse) i <<- inverse
		getInverse <- function() i
		
		list(set = set, get = get, 
			setInverse = setInverse,
			getInverse = getInverse)
}


## The following function calculates the inverse by checking if the inverse is 
## already has been calculated, if so it gets the inverse of the cache.
## Otherwise it calculates it and sets the inverse via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		i <- x$getInverse()
		if(!is.null(i))
		{
			message("Getting cached data")
			return(i)
		}
		
		data <- x$get()
		i <- solve(data, ...)
		x$setInverse(i)
		i
}
