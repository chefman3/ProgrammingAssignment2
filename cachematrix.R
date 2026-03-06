## This first function will create a cached Matrix whose value can be replaced
## using the second function cacheSolve. The second function computes the 
## inverse matrix set in the first function.

## The makeCacheMatrix will take an argument x as a matrix, and first assign
## a value of 'NULL' to the variable 'invmtx'. The first function in the list
## is 'set', which defines a function to assign a variable 'y' to the variable
## 'x' in this local closure. At the same time, it will assign 'NULL' to
## 'invmtx'. Next, the 'get' function will return the stored matrix 'x', 
## which was defined in the environment made when makeCacheMatrix was executed. 
## Next, 'setinvmatrix' will assign a variable 'y' to 'invmtx' and overwrites 
## the initial 'NULL' value assigned, copying the computed value into that 
## cached storage. Next, the 'getinvmatrix' function will return the cached 
## inverse 'invmtx'. 
## Finally, a list is returned of the function names.

makeCacheMatrix <- function(x = matrix()) {
    invmtx <- NULL
    set <- function(y) {
      x <<- y
      invmtx <<- NULL
    }
    get <- function() x
    setinvmatrix <- function(y) invmtx <<- y
    getinvmatrix <- function() invmtx
    list(set = set, get = get, setinvmatrix = setinvmatrix, getinvmatrix = getinvmatrix)
}


## cacheSolve calls the function 'getinvmatrix' created in the makeCacheMatrix
## and assigns the returned value to 'invmtx'. The if statement checks whether 
## 'invmtx' is not a 'NULL' value; if it returns TRUE, then the message is 
## printed and the non-null value is returned. Next, the 'get' function stores 
## the matrix to 'data'. Next, the inverse of the matrix stored in 'data' will 
## be computed using 'solve()' and then stored to 'invmtx'. 
## The computed value will then be passed to 'setinvmatrix' from the first 
## function and cached. Finally, 'invmtx' is returned.

cacheSolve <- function(x, ...) {
    invmtx <- x$getinvmatrix()
    if(!is.null(invmtx)) {
      message("getting cached data")
      return(invmtx)
    }
    data <- x$get()
    invmtx <- solve(data, ...)
    x$setinvmatrix(invmtx)
    invmtx
}
