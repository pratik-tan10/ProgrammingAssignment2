## The first function 'makeCacheMatrix' stores the
## inverse of a matrix in cache memory
## Second Function 'cacheSolve' first looks into cache memory to find
## if the inverese of passed matrix exists, then result is displayed
## from cache,else inverse is computed

## This function takes a square inversible matrix as argument
## has methods to set and get the matrix as well as it's inverse
## The passed matrix and it's inverse are both loaded in cache

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
      	x <<- y
      	m <<- NULL
	}
	get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,setinverse = setinverse,
		getinverse = getinverse)
}


## This function requires makeCacheMatrix(matrix) as it's argument
## Firslty it checks if the passed matrix exists in cache
## If yes, inverse is displayed from cache rather than computing again
## If not, inverse of the matrix is computed

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
      	message("getting cached data")
            return(m)
	}
      data <- x$get()
      m <- solve(data, ...)
	x$setinverse(m)
	m
        ## Return a matrix that is the inverse of 'x'
}
