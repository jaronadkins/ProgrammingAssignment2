## The makeCacheMatrix function assigns the value of the matrix, retrieves the value of the matrix,
#assigns the value of the inverse, and retrieves the value of the inverse. The cacheSolve function
#checks for a cached value of the inverse, and if present retrieves it. Otherwise it calculates the
#inverse of the matrix and sets the value. 

## The set function assigns the matrix, x, to the value y and creates the empty matrix, i. The 
# get function retrieves the matrix x. The setinv function assigns the inverse matrix to i, and the
#getinv function retrieves the inverse matrix, i. 
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}



## This function first checks to see if values are already assigned to the variable i. If so, it 
#displays a message and retrieves the values. If not, it retrieves the orignial matrix, solves for
#the inverse, caches the solution and displays the solution. 

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
