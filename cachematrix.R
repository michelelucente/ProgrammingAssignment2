## Use makeCacheMatrix(M) to define an object containing the matrix M
## and allowing to cache its inverse value.

## Use cacheSolve(makeCacheMatrix(M)) to compute the inverse of the matrix M.
## If the inverse has been previously computed then cacheSolve will recover
## its value from the cache, skipping the computation.

## makeCacheMatrix() creates a list of functions to
##		1.  set the value of the matrix
##		2.  get the value of the matrix
##		3.  set the value of the inverse
##		4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function(y) {
			x <<- y
			inv <<- NULL
		}
		get <- function() x
		setinv <- function(solve) inv <<- solve
		getinv <- function() inv
		list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve() calculates the inverse of the special "matrix"
## created with the makeCacheMatrix() function. It first checks if the
## inverse has already been calculated. If so, it gets it from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the `setinv`
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
        	message("Recovering the inverse value from cached data")
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
