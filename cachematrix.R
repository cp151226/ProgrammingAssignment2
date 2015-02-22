## These two functions below are used to create a special matrix that stores a matrix and
## cache's its inverse.

## This first function (makeCacheMatrix) creates a special matrix that is really a list containing a function to: 
## 1: Set the value of the matrix
## 2: Get the value of the matrix
## 3: Set the value of the inverse
## 4: Get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL												## First assigning "NULL" to inverse
        set_value <- function(y) {
                x <<- y											## Setting the matrix as 'x'
                inverse <<- NULL
        }
        get_value <- function() x								## Returning the matrix 'x'
        set_inverse <- function(solve) inverse <<- solve		## Caches the value of the inverse
        get_invserse <- function() inverse						## Returns inverse
        
		list(set_value = set_value, get_value = get_value,
             set_inverse = set_inverse,
             get_invserse = get_invserse)

}


## This second function calculates the inverse of the special matrix returned by the 
## makeCacheMatrix function above.  However, if the inverse was already calculated,
## without a change in the matrix, cacheSolve will retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {						## Return a matrix that is the inverse of 'x'
	inverse <- x$get_invserse()							## Gets inverse
        if(!is.null(inverse)) {							## Checks for presence of inverse
                message("getting cached data")			## Display message
                return(inverse)
        }
        data <- x$get_value()							## Gets matrix
        inverse <- solve(data, ...)						## Calculates inverse using solve()
        x$set_inverse(inverse)							## Caches inverse
        inverse											## Returns inverse
}
