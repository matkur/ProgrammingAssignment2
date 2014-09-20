## With these two functions (makeCacheMatrix and cacheSolve) a matrix can be inverted and the inversion is cached to be
## used again. It is assumed that there is an inverse of the matrix.
## Call first makeCacheMatrix with argument matrix to be inverted. This function returns a list of fuction pointers that are 
## used in cacheSolve funtion.
## Example of the use of these function calls:
## m<-makeCacheMatrix(matrix(1:4,2,2))  
## cacheSolve(m)                         ## this call inverts the matrix and retuns the inverted matrix
## cacheSolve(m)                         ## the next call uses the cached inverted matrix and returns it
##                                       ## and you get the message "getting cached data"
## Your program can call the functions inside makeCacheMatrix function directly, but then the results are
## not guaranted to be correct !!

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    #set inverse matrix to NULL
    inv_matrix <- NULL
    
    #setMatrix sets x (matrix to be inverted) to the argument arg_matrix and sets inv_matrix to null
    setMatrix <- function(arg_matrix) {
        x <<- arg_matrix
        inv_matrix <<- NULL
    }
    
    #getMatrix returns the value of x (matrix in makeCacheMatrix to be inverted)
    getMatrix <- function() {
        x
    } 
    
    #setInverse sets inverse of x into variable inv_matrix (this matrix is returned in cacheSolve to the calling program)
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
    ## Return a matrix that is the inverse of the argument matrix 'x'
    
    #attempts to get the inverted matrix from x (if it was inverted previously)
    inv_matrix <- x$getInverse()
    
    #if not null, an inverted matrix was cached, so return inv_matrix 
    if(!is.null(inv_matrix)) {
        message("getting cached data")
        return(inv_matrix)
    }
    
    #since the inverted matrix is null, set arg_matrix to the matrix from makeCacheMatrix
    
    arg_matrix <- x$getMatrix()
    
    #Invert the matrix. In this cacheSolve function it is assumed that the matrix supplied is always invertible.
    inv_matrix <- solve(arg_matrix, ...)
    
    #set inverted matrix to inv_matrix
    x$setInverse(inv_matrix)
    
    #return inverse
    inv_matrix    
    
}
