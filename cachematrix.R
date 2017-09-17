## The following two functions will cache the inverse of a matrix. By using such functions, it
## is beneficial for effective computing.

## Function makeCacheMatrix will convert a matrix to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function cacheSolve calculates th inverse of the special "matrix" created with the above funtion.
## It will check if the inverse has already been calculated. If so, it get the inverse from the cache
## and skip the calculation. Otherwise, it calculates the inverse of the data and sets the value of the
## inverse in teh cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}

## Sample run:
## a <- matrix(1:4,2,2)
## m <- makeCacheMatrix(a)
## m$get()
##      [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## no cache at this time
## cacheSolve(m)
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## Retrieving from cache at second run
## cacheSolve(m)
## getting cached data
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
