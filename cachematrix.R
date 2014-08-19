# Create a matrix and define the set and get methods of the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    
    #Get value of matrix
    get <- function() x
    
    #Set value of matrix on cache
    setinverse <- function(solve) cache <<- solve
    
    #Get value of matrix from cache
    getinverse <- function() cache
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# Calcule the inverse of matrix and saves on cache. If the inverse was calculated returns from cache.
cacheSolve <- function(x, ...) {
	
	inversecache <- x$getinverse()
    
	# If the inverse was calculated returns from cache
    if(!is.null(inversecache)) {
        message("getting cached data")
        return(inversecache)
    }
    
    #Calcule the inverse
	data <- x$get()
    inverse <- solve(data)
    
	#Saves the inverse on cache
	x$setinverse(inverse)
    inverse
}