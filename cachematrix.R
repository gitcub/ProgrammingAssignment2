## These functions cache a matrix and its inverse (for square matrices only)
## Storing these values avoids repeated recalculation of the inverse value.

# The makeCacheMatrix creates a function, which is really a list.
# This list contains four nested functions:
# 1. the 'set' function assigns the input matrix to a variable
# 2. the 'get' function recalls the exisiting matrix
# 3. the 'setInverse' function assigns the inverted matrix to a variable
# 4. the 'getInverse' function recalls the inverted matrix

makeCacheMatrix <- function(storeMatrix = matrix()) {
    
    # create the inverse matrix variable and set it to null initially
    # this variable will store the cached value
    inverseMatrix = NULL
    
    # allows an original matrix to be stored
    set <- function(inputMatrix) {
        
        # store the input matrix
        storeMatrix <<- inputMatrix
        
        # ensure that any previous inverted matricies are removed
        inverseMatrix <<- NULL
    }
    
    # allows the stored matrix to be recalled
    get <- function() storeMatrix
    
    # allows an inverse matrix to be stored
    setInverse <- function(inverse) inverseMatrix <<- inverse
    
    # allows the inverse matrix to be recalled
    getInverse <- function() inverseMatrix
        
    list(set = set,
         get = get, 
         setInverse = setInverse,
         getInverse = getInverse)
}


# The cacheSolve function requires that the makeCacheMatrix is run first.
# Only square matrices can be inverted. 
# If the inverse matrix is already stored, it will be returned.
# If the inverse matrix is not stored, the cacheSolve function will solve it
# to return and store the matrix inverse.

cacheSolve <- function(inputMatrix, ...) {
    
    # check to make sure the matrix is invertible (e.g. is square)
    if(nrow(inputMatrix$get()) == ncol(inputMatrix$get())) {

        # check if there is a stored inverse matrix
        inverseMatrix <- inputMatrix$getInverse() 
    
        # return the cached matrix value if it exists
        if(!is.null(inverseMatrix)) {
            message("Getting cached data.")
            return(inverseMatrix)
            
        } else {
            
            # if the inverse matrix is not cached, get the input matrix
            data <- inputMatrix$get()
            
            # inverse the input matrix
            inverseMatrix <- solve(data, ...)
            
            # store the inverse matrix
            inputMatrix$setInverse(inverseMatrix)
            
            # display the inverse matrix
            inverseMatrix   
        }
        
    } else {
        
        # Eror if the matrix is not square and can't be inverted.
        message("Error: Matrix must be square to invert.")
    }

}
