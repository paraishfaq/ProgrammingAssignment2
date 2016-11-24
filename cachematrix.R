## These functions create a matrix, which is then stored as a cached value, and then finds the inverse of
##	the matrix, if it differs from the cached value.

## makeCacheMatrix: This function creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(solve) inv <<- solve
	getinv <- function() inv
	list(set = set, get = get,
		setinv = setinv, 
		getinv = getinv)
}


## cacheSolve: Calculates the inverse of the matrix returned by the above function.
##			If inverse has already been calculated and has not changed from the cached
##			version, will return inverse from cache directly.

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}

x = rbind(c(1, -1/4), c(-1/4, 1))
m=makeCacheMatrix(x)
cacheSolve(m)