## With these two functions (makeCacheMatrix and cacheSolve) a matrix can be inverted and the inversion is cached 
## for further use. It is assumed that the matrix is a square matrix and that there is an inverse of the matrix.
##
## Call first makeCacheMatrix with argument the matrix to be inverted. This function returns a list of fuction pointers
## that are used in cacheSolve funtion.
##
## Example of the use of these function calls:
## m<-makeCacheMatrix(matrix(1:4,2,2))  
## cacheSolve(m)                         ## this call calculates the inverse matrix and returns it
## cacheSolve(m)                         ## the next cacheSolve call returns the cached inverted matrix and returns it
##                                       ## and you get the message "getting cached data"
##
## Although your program can call the functions inside makeCacheMatrix directly, the results might be wrong !!
## If you use a matrix that is not a square matrix or that has no inverse, you will get error message from the "solve"
## function.

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
    
    #setInverse sets inverse of x into variable inv_matrix
    setInverse <- function(i_matrix){
        inv_matrix <<- i_matrix 
    }
    
    # getInverse returns the inverted matrix
    getInverse <- function() {
        inv_matrix
    }
    
    #makeCacheMatrix returns a labeled vector of functions setMatrix, getMatrix, setInverse, getInverse
    list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}    


## cacheSolve: This function computes the inverse of the special "matrix" returned from makeCacheMatrix getInverse function
## If the inverse has already been calculated for this matrix, then this function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
   
    
    # attempts to get the inverted matrix from x (if it was inverted previously)
    inv_matrix <- x$getInverse()
    
    # if not null, an inverted matrix was cached, so return it and give a message 
    if(!is.null(inv_matrix)) {
        message("getting cached data")
        return(inv_matrix)
    }
    
    # since the inverted matrix is null, set arg_matrix to the orginal matrix from makeCacheMatrix
    
    arg_matrix <- x$getMatrix()
    
    # invert the matrix. Here we assume that the matrix used is always invertible and is a square matrix
    inv_matrix <- solve(arg_matrix, ...)
    
    #set inverted matrix to inv_matrix
    x$setInverse(inv_matrix)
    
    ## Return a matrix that is the inverse of the argument matrix 'x'
    inv_matrix    
    
}
