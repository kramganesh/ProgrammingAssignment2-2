## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## assigning inverse variable to any NULL obects, setting value of matrix using double arrow assignment operator which represent a closer allowing for multiple layers of parameters. Then we will get the value of the matrix, setting the inverse and getting the inverse using the various functions.

makeCacheMatrix <- function(x = matrix()) {
		inverse <- NULL
		  apply <- function(y){
				x <<- y
				inv <<- NULL
		}
		get <- function() {x}
		setInv <- function(inv) {inverse <<- inv}
		getInv <- function() {inverse}
		list(apply = apply, get = get, setInv = setInv, getInv = getInv)
}

## Write a short comment describing this function
## cacheSolve will obtain inverse of x and apply to the inverse variable, we will check if the value is not null then we will capture and return the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inverse <- x$getInv()
        if(!is.null(inverse)){
            message("getting cached data")
            return(inverse)
	    }
	    matrix <- x$get()
	    inverse <- solve(matrix, ...)
	    x$setInv(inverse)
	    inverse
}
