## Programming Assignment 2 - R Programming
## Usage example:
## x <- matrix(1:4, nrow=2, ncol=2) or
## x <- matrix(rnorm(1:16),4,4) - if the matrix is larger than 2x2
## m <- makeCacheMatrix(x)

makeCacheMatrix <- function(original.matrix = matrix()) {
      
      ## Check if we have correct input
      
      if (!is.matrix(original.matrix)) {
            stop("Please provide a proper Matrix")
      }
      
      inverted.matrix <- NULL
      
      set <- function(y) {
            original.matrix <<- y
            inverted.matrix <<- NULL
      }
      
      ## Functions to get and set cached inverted matrix value
      
      get <- function() original.matrix
      
      ## Inverse the matrix using solve() function in R
      
      set.inverse <- function(solve) inverted.matrix <<- solve
      get.inverse <- function() inverted.matrix
      
      list(
            set = set, 
            get = get,
            set.inverse = set.inverse,
            get.inverse = get.inverse)
      
}


## Construct the inverse of the original matrix returned by makeCacheMatrix()
## If the inverse has already been done and there's no change in the matrix
## then the cacheSolve() returns the cached inverse

cacheSolve <- function(cacheable.matrix, ...) {
      inverted.matrix <- cacheable.matrix$get.inverse()
      
      ## Is cached matrix available?
      
      if(!is.null(inverted.matrix)) {
            message("Getting cached inverse matrix")
            return(inverted.matrix)
      }
      
      
      ## Construct inverted matrix if there's no cached matrix.
      
      matrix.to.inverse <- cacheable.matrix$get()
      inverted.matrix <- solve(matrix.to.inverse)
      cacheable.matrix$set.inverse(inverted.matrix)
      
      ## return the inverted matrix      
      inverted.matrix
      
}
