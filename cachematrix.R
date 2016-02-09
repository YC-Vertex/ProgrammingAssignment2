## this file contains a pair of functions that cache the inverse of a matrix

## function "makeCacheMatrix"
## creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(newMatrix = matrix()) {
    inverseOfMatrix <- NULL
    
    ## function "set" caches the original matrix
    set <- function(matrix) { 
        newMatrix <<- matrix
        inverseOfMatirx <<- NULL
    }
    
    ## function "get" returns the original matrix
    get <- function() 
        return(newMatrix)
    
    ## function "setInverse" caches the inverse of the matrix
    setInverse <- function(inverse) 
        inverseOfMatrix <<- inverse
    
    ## function "getInverse" returns the inverse of the matrix
    getInverse <- function()
        return(inverseOfMatrix)
    
    return(list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse))
}

## function "cacheSolve"
##c omputes the inverse of the special "matrix" returned by "makeCacheMatrix"
cacheSolve <- function(targetMatrix, ...) {
    result <- targetMatrix[["getInverse"]]()
    
    ## check whether the inverse of the target matrix
    ## had already been calculated
    if (!is.null(result)) {
        return(result)
    }
    
    else {
        ## get the inverse of the target matrix
        target <- targetMatrix[["get"]]()
        result <- solve(target, ...)
        
        ## return the inverse of the target matrix
        targetMatrix[["setInverse"]](result)
        return(result)
    }
}
