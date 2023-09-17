# This function creates an R object that caches the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setInverseMatrix <- function(invMat) inverseMatrix <<- invMat
    getInverseMatrix <- function() inverseMatrix
    # A list consisting of four functions as its components
    list(set=set, get=get, setInverseMatrix=setInverseMatrix, getInverseMatrix=getInverseMatrix)
}

# This function computes the inverse of a matrix returned by the above function.
# If the inverse has already been computed, then this function retrieves the inverse 
# from the cache
cacheSolve <- function(x, ...) {
    inverseMatrix <- x$getInverseMatrix()
    if(!is.null(inverseMatrix)) {
        message("Get cached data.")
        return(inverseMatrix)
    }
    
    dataMatrix <- x$get()
    # Compute the inverse of a matrix using the solve() function
    inverseMatrix <- solve(dataMatrix)
    x$setInverseMatrix(inverseMatrix)
    inverseMatrix
}
