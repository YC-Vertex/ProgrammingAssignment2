## this file contains a pair of functions that cache the inverse of a matrix

## function "makeCacheMatrix" creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(newMatrix = matrix()) {
    inverseOfMatrix <- NULL
    
    set <- function(matrix) {
        newMatrix <<- matrix
        inverseOfMatirx <<- NULL
    }
    
    get <- function() 
        return(x)
    
    setInverse <- function(inverse) 
        inverseOfMatrix <<- inverse
    
    getInverse <- function()
        return(inverseOfMatrix)
    
    return(list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse))
}

## function "cacheSolve" computes the inverse of the special "matrix"
## returned by "makeCacheMatrix"

cacheSolve <- function(targetMatrix) {
    result <- targetMatrix[["getInverse"]]()
    
    ## check whether the inverse of the target matrix
    ## had already been calculated
    if (!is.null(result)) {
        return(result)
    }
    
    else {
        ## get the inverse of the target matrix
        target <- targetMatrix[["get"]]()
        result <- matrix(nrow = ncol(target), ncol = nrow(target))
        for (i in 1:ncol(target)) 
            result[i, ] = target[, i]
        
        ## return the inverse of the target matrix
        targetMatrix[["setInverse"]](result)
        return(result)
    }
}
