## ASSIGNMENT2:
# 1.) makeCacheMatrix(): This function creates a special "matrix" object that
#                        can cache its inverse.
# 2.) cacheSolve():      This function computes the inverse of the special  
#                        "matrix" returned by makeCacheMatrix above. If the  
#                        inverse has already been calculated (and the matrix  
#                        has not changed), then cacheSolve should retrieve the 
#                        inverse from the cache.

## ASSUMPTION: "For this assignment, assume that the matrix supplied is always 
#              invertible."

## FUNCTION: makeCacheMatrix
## Purpose: facilitates caching of the original matrix and a place to store the 
##          inverse matrix. Note: There is no checking to verify the inverse is
##          really the inverse of the first matrix. It should under most cases
##          get cleaned up with NULL assignments.

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  setMatrix <- function(y) {                # Function facilitating the
    x <<- y;                                # cache of both the Matrix
    inv_matrix <<- NULL;                    # and Inverse Matrix (NULL to start)
  }  
  getMatrix <- function() x                 # Return the cached Matrix
  setInvMatrix <- function(solved = NULL) { # Set Inv Matrix function, default
    inv_matrix <<- solved                   # is NULL.
  }
  getInvMatrix <- function() {inv_matrix}   # Return the stored Inv Matrix
  
  list(setMatrix = setMatrix,               # Return a list, detailing the 
       getMatrix = getMatrix,               # functions, which facilitates 
       setInvMatrix = setInvMatrix,         # an interactive 'help' for
       getInvMatrix = getInvMatrix)         # makeCacheMatrix()
}


## FUNCTION: cacheSolve
## Purpose: Checks if we have a cached inverse matrix, and returns it if it 
##          exists, otherwise it solves for the inverse of the stored matrix
##          in the input object.

cacheSolve <- function(x, ...) {
  inv_matrix <- x$getInvMatrix()            # Grab inverse matrix from x
  if(!is.null(inv_matrix)) {                # If the value is NOT NULL, 
    message("getting cached matrix")        # let the user know it is cached
    return(inv_matrix)                      # and return the cached value.
  }

  inv_matrix <- solve(x$getMatrix(), ...)   # Grab the matrix, solve its
  x$setInvMatrix(inv_matrix)                # inverse, then save it.
  
  ## Return a matrix that is the inverse of 'x'
  inv_matrix
}
