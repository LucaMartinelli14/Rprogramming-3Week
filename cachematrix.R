## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function Function makeCacheMatrix gets a matrix as an input, set the value of the matrix,
# get the value of the matrix, set the inverse Matrix and get the inverse Matrix. The matrix object
# can cache its own object.

makeCacheMatrix <- function(x = matrix()) {

	inv_m <- NULL
      set <- function(y) {
                x <<- y
                in_m <<- NULL
       }
       get <- function() x
       setinverse <- function(inverse) inv_m <<- inverse
       getinverse <- function() inv_m
       
	list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)

}


## Write a short comment describing this function

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
       
	   #get the value of the invertible matrix from the makeCacheMatrix function
	    invM <- x$getinverse()
        if(!is.null(invM)) {
                message("getting cached data")
                return(invM)
        }
        dataMatrix <- x$get()	# original Matrix data
        invM <- solve(dataMatrix, ...) # compute the inverse
        x$setinverse(invM) # set the inverse
        return(invM) # return the inverse 
}
