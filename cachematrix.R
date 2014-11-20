# Functions makeCacheMatrix and cacheSolve are designed for fast (if cached) computation of inverse matrix
# 
# to get the Inverse Matrix, use exclusively function cacheSolve on the List created by makeCacheMatrix
#
# Example usage (for invertible matrix A):
#
# > A_obj = makeCacheMatrix(A)
# > A_inv = cacheSolve(A_list)
#

# Creates pseudo-object (list) designed to compute and cache inverse matrix of it's input matrix.
makeCacheMatrix <- function(x = matrix()) {
    xi <- NULL # on creation we null the Inverse matrix
    # Defining of output functions:
    set <- function(y) {
        x <<- y
        xi <<- NULL
    }
    get <- function() { x } 
    setInverse <- function(xiNew) { xi <<- xiNew }
    getInverse <- function() { xi }
    # creation of the output list:
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)	
}

## Computes inverse matrix of the matrix set in the list created by makeCacheMatrix.
# If the inverse matrix was already computed, it will store this inversion for later use.
cacheSolve <- function(Xlist, ...) {
    xi <- Xlist$getInverse() # we try to get the cached inversion
    if(!is.null(xi)) {
        return(xi) # if there is any cached data, we return it
    }
    x <- Xlist$get() # otherwise we have to get the matrix, 
    xi <- solve(x, ...) # invert it
    Xlist$setInverse(xi) # and store it for later use.
    xi
}

