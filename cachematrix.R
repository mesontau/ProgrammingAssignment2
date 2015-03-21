## makeCacheMatrix stores a matrix 'x' and is able to cache the inverse of 'x'.
## cacheSolve returns the inverse of a matrix stored in an object like makeCacheMatrix.
## If the inverse was cached in the object, it does not calculate it, but returs the cached value.

## Example:
##  > myx <- matrix(c(1,2,3,0,1,4,5,6,0), nrow = 3,ncol = 3)
##  > x.with.cache <- makeCacheMatrix(myx)
##  > cacheSolve(x.with.cache)
##  (returns the inverse after calculating it)
##  > cacheSolve(x.with.cache)
##  (returns the inverse from cache)

## makeCacheMatrix stores a matrix x and is able to store a value for the 
## inverse matrix of 'x' as a cache.
## Accession to values is performed through get/set functions:
## set(y)       : sets the value of the matrix 'x' (from external matrix 'y') 
## get()        : returns the matrix 'x'
## setinverse   : sets the value of the inverse of 'x'
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL # 'i': cached value of the inverse of 'x'
    
    # sets a new matrix into the object
    set <- function(y) {  
        x <<- y # set the matrix value from outside of the scope of makeCacheMatrix 
        i <<- NULL # since the matrix is newly set, we set to null its cached inverse 
    }
    
    # returns the matrix 'x'
    get <- function() x  
    
    setinverse <- function(inverse){
        i <<- inverse #sets the inverse value of 'x' as cache.
        
        # extra: we should check that the value we store is *actually*
        #       the inverse of the matrix 'x':  x %*% i == identity.matrix
        if(is.null(i)) return(invisible(i))
        test.x_inv <- abs( (x %*% i) - diag(dim(x)[1]) )
        tol <- 1e-12
        if( sum(test.x_inv) > tol ){
            message("Value is not the inverse of 'x'! It is not cached.")
            i <<- NULL
        }
    }
    
    # return the cache-stored value of the inverse of the matrix 'x'
    getinverse <- function() i
    
    #return a list of functions that: 
    #   a) gets/sets the value of the matrix 'x'
    #   b) gets/sets a cache value for the inverse of the matrix 'x'
    list(set = set, get = get,
         setinverse = setinverse, 
         getinverse = getinverse) 
}
 
## cacheSolve returns the inverse of a matrix stored in an object 'x',
## where 'x' is a list of functions containing 'x$get()' (returns the 
## actual matrix), and 'x$getinverse()' returns a cached value of the 
##inverse if it was previously calculated.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    #first try to get the inverse from the cache (stored in object x)
    i <- x$getinverse()
    if(!is.null(i)) {
        message("retrieving inverse matrix from cache")
        return(i)
    }
    
    #if the inverse matrix was not stored in cache, calculate it
    data <- x$get()  #retrieve the data (in this case, the matrix we want to invert)
    i <- solve(data) # solve() calculates matrix inverses. 
    x$setinverse(i)  # store the calculated inverse into the cache of object x
    i
}
