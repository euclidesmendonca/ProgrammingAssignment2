## This function solve matrices and in case of unalterations of the inputed matrix it uses a  
## cached matrix

## This function cache the input matrix

makeCacheMatrix<- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## This function check if the input matrix is the same, if it is, it uses the previously cached matrix. 
## If not, it repeats solving step

cacheSolve  <- function(x, ...) {
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}
