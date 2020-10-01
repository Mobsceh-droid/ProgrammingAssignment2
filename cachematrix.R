## Two functions are created that cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {      #Function sets the value of the matrix 
                x <<- y
                Inv <<- NULL
        }
        get <- function() x       #Function gets the value of the matrix
        setInverse <- function(inverse) Inv <<- inverse     #Function sets the value of the inverse
        getInverse <- function() Inv          #Function gets the value of the inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
#then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv <- x$getInverse()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data_mat <- x$get()
        Inv <- solve(data_mat, ...)
        x$setInverse(Inv)
        Inv
}
