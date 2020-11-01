
## This function creates a special invertible "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
        # set the matrix
    set <- function(y) {
        # use '<<-' operator to assign a value to an object within an environment
        x <<- y
        inv <<- NULL
    }
        # get the matrix
    get <- function() {x}
        # set the inverse
    setInverse <- function(inverse) {inv <<- inverse}
        # get the inverse
    getInverse <- function() {inv}
        # this list is used as the input to cacheSolve()
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function returns the inverse of the "matrix" returned by 'makeCacheMatrix()'

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
        # if the inverse has already been calculated
    if (!is.null(inv)) {
            # get if from the cache and skip the computation
            message("getting cached data")
            return(inv)
    }
    
    # otherwise, calucate the inverse
    mat <- x$get()
    inv <- solve(mat, ...)
    
    # sets the value of the inverse in the cache 
    x$setInverse(inv)
    # return the inverse of the 'x'
    inv 
}

# TESTING

pmatrix <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
  
    # get the matrix you created
pmatrix$get()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4

    # get inverse of the matrix
pmatrix$getInverse()
# NULL (the matrix has not been cached)

cacheSolve(pmatrix)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

cacheSolve(pmatrix)
# getting cached data (skips the computation as the inverse is already cached)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

pmatrix$getInverse()
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5