## function to cache a matrix and return its inverse

## function that makes an empty "matrix" consisting of a list that:
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the inverse
## 4. gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	inverted <- NULL
	set <- function(y) {
		x <<- y
		inverted <<- NULL
	}
	get <- function() x
	setInverse <- function(solveMatrix) inverted <<- solveMatrix
	getInverse <- function() inverted 
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## function that calculates inverse of the "matrix" created with makeCacheMatrix
## if inverse is already calculated, then retrieve inverse from the cache

cacheSolve <- function(x, ...) {
	inverted <- x$getInverse()
	if(!is.null(inverted)){
		message("getting cached data")
		return(inverted)
	}
 	matrix <- x$get()
	inverted <- solve(matrix, ...)
	x$setInverse(inverted)
	inverted
}
