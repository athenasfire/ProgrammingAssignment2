## The functions can be used together to create and retrieve the
##   inverse of a matrix

## Creates a matrix object that also stores its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL

    set <- function(y) {
        x <<- y
        i <<- NULL
    }

    get <- function() x

    setInverse <- function(inverse) {
        i <<- inverse
    }

    getInverse <- function() i

    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Takes the list created in makeCacheMatrix and
##   returns the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	i <- x$getInverse()

	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}

	data <- x$get()
	i <- solve(data, ...)
	x$setInverse(i)
	i
}
