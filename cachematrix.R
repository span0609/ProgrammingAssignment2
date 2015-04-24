## The following pair of functions is going to compute the
## inverse of a matrix, which is assumed always invertible.
## The aim of these functions is to cache potentially
## time-comsuming inverse of a matrix rather than computing
## it repeatedly.

## `makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix())
{
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cachesolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) 
{
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}
