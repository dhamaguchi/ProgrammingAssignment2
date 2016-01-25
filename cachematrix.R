## makeCacheMatrix() and CacheSolve() make use of the cache memory 
## to find and store the inverse of a matrix to avoid duplication 
## of using solve(), which can be time/memory consuming.

## makeCacheMatrix() assigns "x" as the matrix, "inv" as the inverse of x,
## and stores functions: setMatrix, getMatrix, setInverse, and getInverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
	setMatrix <- function (y) {
		x <<- y
		inv <<- NULL
	}
	getMatrix <- function(x) {
		return (x)
	}
	setInverse <- function(solve) {
		inv <<- solve(x)
		return (inv)
	}
	getInverse <- function() {
                return (inv)
	}
	list (setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve() checks if the inverse matrix, inv, is cached.
## If so, the cached inverse is returned. 
## Otherwise, the inverse is calculated using solve().

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
		if (!is.na(inv)) {
		        message (“getting cached inverse”)
			return (inv)
		}
		matrix <- x$getMatrix()
		inv <- solve(matrix)
		x$setInverse(inv)
		return (inv)
}
}
