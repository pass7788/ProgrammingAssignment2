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


## excution result
> 
> source("makeMatrix.R")
> mk_invMatrix<- makeCacheMatrix(matrix(8:11, 2, 2))
> mk_invMatrix$get()
     [,1] [,2]
[1,]    8   10
[2,]    9   11
> mk_invMatrix$getInverse()
NULL
> cacheSolve(mk_invMatrix)
     [,1] [,2]
[1,] -5.5    5
[2,]  4.5   -4
> mk_invMatrix$getInverse()
     [,1] [,2]
[1,] -5.5    5
[2,]  4.5   -4
> mk_invMatrix$set(matrix(c(14, 9,13,7), 2, 2))
> mk_invMatrix$get()
     [,1] [,2]
[1,]   14   13
[2,]    9    7
> mk_invMatrix$getInverse()
NULL
> cacheSolve(mk_invMatrix)
           [,1]       [,2]
[1,] -0.3684211  0.6842105
[2,]  0.4736842 -0.7368421
> cacheSolve(mk_invMatrix)
getting cached data
           [,1]       [,2]
[1,] -0.3684211  0.6842105
[2,]  0.4736842 -0.7368421
> mk_invMatrix$getInverse()
           [,1]       [,2]
[1,] -0.3684211  0.6842105
[2,]  0.4736842 -0.7368421
> 
