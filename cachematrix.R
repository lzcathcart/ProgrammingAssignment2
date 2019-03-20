## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {               ## this argument defines what the default mode is. 
        invs <- NULL                                                        ## invs is created to hold the value of matrix inverse further down
        set <- function(y) {                                                ## the set function has been defined
                x <<- y                                                          ## this is the matrix value defined within the parent environment
                invs <<- NULL                                              ## any new matrix will be reset to NULL
        }
        get <- function() x                                                 ## this defines the get function for matrix value
        setinverse <- function(inverse) invs <<- inverse  ## invs is defined within the parent environment
        getinverse <- function() invs                                 ## invs gets a value
        list(set = set, get = get,                                         ## this allows you to call up each function ($ must be used)
             setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
                                                                                     ## Return a matrix that is the inverse of 'x'
 invs <- x$getinverse()                                                 ## invs is defined
        if(!is.null(invs)) {                                                   ## this is used if the inverse matrix is not NULL
                message("getting cached data")                  ## message issued 
                return(invs)                                                   ## the inverse matrix is returned
        }
        data <- x$get()                                                      ## the get function returns the data
        invs <- solve(data, ...)                                           ## the solve function inverses the matrix
        x$setinverse(invs)                                                ## the inverse matrix is set
        m                                                                          ## the inverse matrix is returned
}

