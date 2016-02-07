## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

 inverse <- NULL   # to initiliza the inverse as NULL 
  set <- function(y) {
    x <<- y       # this set the uninitialized matrix 
    inverse <<- NULL	# again we haven't computed the inverse yet
  }
  get <- function() x   # return the matrix
  setinverse <- function(mat) inverse <<- mat # set the inverse of the matrix  
  getinverse <- function() inverse # retrieve the inverse of the matrix 
  list(set = set,get = get , setinverse = setinverse , getinverse = getinverse)		#list of functions returned to access them

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()  # get the inverse
  if(!is.null(inverse)) {		# decision statement to determine if we have already calculated the inverse or not
    message("getting cached data")		# in case we calculated it so we retrieve it directly from the cache
    return(inverse)
  }
  mat <- x$get()    # if not we have to calculate it and save it in the cache 
  inverse <- solve(mat) # calculate the inverse
  x$setinverse(inverse)	# cache it
  inverse	# display and return 
		
}


