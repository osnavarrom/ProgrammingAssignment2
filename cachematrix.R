## R-Programming - Peer Assessment  No. 2
## Oscar S. Navarro-Morato 

## Functions that takes as input a matrix and calculates its inverse and storing
## the result in cache and reuse this.

## The inverse is computed the first time only. If the matrix does not change, 
## instead of computing the inverse again, the stored value in cache is returned.

## The first function, makeCaheMatrix creates a special "Matrix", which is really a 
## list containing a function to:

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## 1. Set the value of the matrix
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## 2. Get the value of the matrix
    
    get <- function() x
    
    ## 3. Set the value of the inverse
    
    setinverse <- function(solve) m <<- solve
    
    ## 4. Get the value of the inverse
    
    getinverse <- function() m
    
    ## 5. Returns the list with the content of the function 
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# The second function calculate the inverse of a "special" matrix created with 
# makeCacheMatrix

cacheSolve <- function(x, ...) {
    
    ## 1. Get the cached value
    
    m <- x$getinverse()
    
    ## 2. If a cached value exists return it
    
    if(!is.null(m)) {
        message("    Getting cached data")
        return(m)
    }
    
    ## 3. Otherwise get the matrix, caclulate the inverse and store it in
    ##    the cache
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    
    ## 4. Return the inverse
    
    m
}

##                      Complete Example: 

##    > my_matrix <- matrix(c(4,0,2,2,5,6,2,4,1), nrow=3, ncol=3)
##    > proofm <- makeCacheMatrix(my_matrix)
##    > cacheSolve(proofm)
##    .... calculate inverse ...
##    > cacheSolve(proofm)
##    .... outputs inverse from cache ...