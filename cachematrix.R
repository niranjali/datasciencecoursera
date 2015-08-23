makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(matinv) inv <<- matinv
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
cacheSolve <- function(x, ...) {
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

m1 = matrix(1:4,2,2)
mm1 = makeCacheMatrix(m1)

m2 = matrix(2:8,3,3)
mm2 = makeCacheMatrix(m2)

cacheSolve(mm1)
cacheSolve(mm2)
cacheSolve(mm1)
cacheSolve(mm2)