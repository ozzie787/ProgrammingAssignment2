## These two functions take a nXm matrix (A) where n=m and calculates and stores the inverse (A^{-1}).
## A^{-1) is recalled rather then recalulated if matrix A has not been modified if A has not changed.

## This function creates a special "matrix" object that can cache its inverse. Checks that the argument matrix is a square matrix of dimension nxm where n=m.

makeCacheMatrix <- function(x = matrix()) {
		if (length(dim(x)) != 2) {
			message("Warning message:")
			message("Function requires 2-D Matrix, ", length(dim(x)), " dimensions found!")
			return			
		}
		if (dim(x)[1] != dim(x)[2]) {
			message("Warning message:")
			message("Square matrix required")
			message("	Non-square matrix found, dimension ", dim(x)[1], " X ", dim(x)[2])
			return
		}
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above .
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
