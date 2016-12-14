## The first function makeCacheMatrix() caches the inverse of a matrix, the second
## function cacheSolve() calculates the inverse of the matrix created by makeCacheMatrix()
## before execute the calculation, it will check if the same matrix has been calculated,
## if yes, the inverse will be retrived from cache, otherwise, the inverse will be calculated.

## makeCacheMatrix() create the matrix x and cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {
        x
    }
    setInv <- function(inverse) {
        inv <<- inverse
    }
    getInv <- function() inv
    list(set = set, get = get,      ## Give the name "set" to function set, etc.
         setInv = setInv,
         getInv = getInv)
}



## cacheSolve() retrive the inverse from cache if it has been calculated, otherwise
## it calculates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached matrix")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
