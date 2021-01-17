## These functions help speed up the calculation of inverse of a large matrix
## by caching the value of inverse of the matrix that has been already computed.

## This function creates a special matrix that contains four functions
## to set and return the matrix and its inverse.

makeCacheMatrix <- function(xm = matrix())
{
        inv <- NULL
        set <- function(ym)
        {
                xm <<- ym
                inv <<- NULL
        }
        get <- function() xm
        set_inv <- function(inverse) inv <<- inverse
        get_inv <- function() inv
        list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
}


## This function computes the inverse of the matrix created by makeCacheMatrix().

cacheSolve <- function(xm,...)
{
        inv <- xm$get_inv()
        if(!is.null(inv))
        {
                message("getting cached data")
                return(inv)
        }
        my_matrix <- xm$get()
        inv <- solve(my_matrix, ...)
        xm$set_inv(inv)
        inv
}
