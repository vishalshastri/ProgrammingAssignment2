## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix stores a matrix X in memory

## cacheSolve shows the inverse of a matrix if is in memory or computes the inverse and then shows the inverse

## Write a short comment describing this function
## makeCacheMatrix uses scoping rules and stores matrices in memory
# Functions to compute the inverse of a matrix with caching.

# Caching function for a matrix argument.  Returns a vector 
# of getters and setters for the matrix itself, as well as 
# placeholders for the matrix inverse calculation.

makeCacheMatrix <- function(x = matrix()) {
# inverse will store the cached inverse matrix
xinv <- NULL

#To set the value of the matrix x
set <- function(y) {
	x <<- y
	xinv <<- NULL
}

#To get the value of the matrix x
get <- function() x

#To set the value of the inverse of x
setinv <- function(inverse) xinv <<- inverse

#To get the value of the inverse of x
getinv <- function() xinv

#Returns a matrix with newly defined functions to manipulate the cache
list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function
# Calculates the inverse of an assumed square matrix, or retrieves
# a previously calculated inverse from cache, given a list argument
# from the function makeCacheMatrix().

cacheSolve <- function(x, ...) {
xinv <- x$getinv()

# If the inverse is already calculated
if (!is.null(xinv)) {
message("Info - getting cached data")
return(xinv)
}

# The inverse is not yet computed. Compute it here.
data <- x$get()
message("Info - computing inverse")
xinv <- solve(data, ...)

# Cache the inverse of x
x$setinv(xinv)

# Returns inverse of x
xinv
}
