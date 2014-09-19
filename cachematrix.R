## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    #set inverse matrix to NULL
    inv_matrix <- NULL
    
    #setMatrix sets x (matrix to be inverted) to the argument arg_matrix and sets inv_matrix to null
    setMatrix <- function(arg_matrix) {
        x <<- arg_matrix
        inv_matrix <<- NULL
    }
    
    #getMatrix returns the value of x (argument of makeCacheMatrix)
    getMatrix <- function() {
        x
    } 
    
    #setInverse sets inverse of x into inv_matrix in makeCacheMatrix
    setInverse <- function(i_matrix){
        inv_matrix <<- i_matrix 
    }
    
    # getInverse returns the matrix of inv_matrix (from makeCacheMatrix)
    getInverse <- function() {
        inv_matrix
    }
    
    #returns a labeled vector of functions set, get, setmean and getmean
    list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}    


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix funtion above.
## If the inverse has already been calculated for this matrix, then this function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    #attempts to get the inverted matrix from x (if it was inverted previously)
    inv_matrix <- x$getInverse()
    
    #if not null, an inverted matrix was cached, so return inv_matrix 
    if(!is.null(inv_matrix)) {
        message("getting cached data")
        return(inv_matrix)
    }
    
    #since its null, set data to x from makeVector
    
    data <- x$getMatrix()
    
    #Invert the matrix. In this cacheSolve function it is assumed that the matrix supplied is always invertible.
    inv_matrix <- solve(data, ...)
    
    #set m in x to calculated mean
    x$setInverse(inv_matrix)
    
    #return inverse
    inv_matrix    
    
}



##