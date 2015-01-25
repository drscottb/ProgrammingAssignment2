## These two functions create a special object that stores a matrix
## and caches its inverse.

## The first function, makeCacheMatrix, creates a list of 4 functions that
## 1. sets the value of the matrix,      2. gets the value of the matrix,
## 3. sets the value of the inverse inv, 4. gets the value of the inverse inv.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## The second function, cacheSolve, computes the inverse of the matrix
## x, stored by the above function. But it first checks whether the
## inverse has already been calculated. If so, then it gets the inverse
## from the cache and skips the computation. Otherwise, it computes the
## inverse of matrix x and sets the value of the inverse in the cache to
## inv via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

