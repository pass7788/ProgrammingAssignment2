## makeCacheMatrix  function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(sq_M = matrix()) {
        v_invt <- NULL
        set <- function(sq_Y) {
                sq_M <<- sq_Y
                v_invt <<- NULL
        }
        get <- function() sq_M
        setInverse <- function(inverse) v_invt <<- inverse
        getInverse <- function() v_invt
        list(set = set ,get = get
             ,setInverse = setInverse
             ,getInverse = getInverse)
}

# cacheSolve function computes the inverse of the special "matrix" 
# created by makeCacheMatrix above.
## Return a matrix that is the inverse of 'sq_M'
cacheSolve <- function(sq_M, ...) {
        v_invt <- sq_M$getInverse()
        if (!is.null(v_invt)) {
                message("getting cached data")
                return(v_invt)
        }
        sq_mat <- sq_M$get()
        v_invt <- solve(sq_mat, ...)
        sq_M$setInverse(v_invt)
        v_invt
}


