## The makeCacheMatrix function takes in a matrix and outputs a list with four functions,
## which allow for getting and setting the embedded matrix and for either calculating the 
## matrix's inverse or for retreiving the cached value of the inverse if previously calculated.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL       ## Initializes the inverse variable
      set <- function(y) {
            x <<- y     ## Establishes the function to set the value of the input matrix
            inv <<- NULL
      }
      get <- function() x  ## Establishes the function to get the value of the input matrix
      setinverse <- function(solve) inv <<- solve  ## The function to solve the input matrix
      getinverse <- function() inv   ## The function to retrieve an already-calculated solution

      list(set = set, get = get,     ## Returns the list of functions
           setinverse = setinverse,
           getinverse = getinverse)
}

## The cacheSolve function takes in a list created by makeCacheMatrix, and if an inverse has
## been previously calculated, it returns it.  If not, then it calls the function to calculate
## the inverse and returns the calculated value.


cacheSolve <- function(x, ...) {
      
      inv <- x$getinverse()     ## This code calls the function to get the inverse,
      if(!is.null(inv)) {       ## and if the result is non-empty, returns the value
            message("getting cached inverse")
            return(inv)
      }
      
      data <- x$get()          ## If the function makes it to this point, the value was empty,
      inv <- solve(data, ...)  ## so the function calculates the inverse and then
      x$setinverse(inv)        ## sets the result as the cached inverse for the list object.
      inv
}