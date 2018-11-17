## Cache the inverse of a matrix

## x is a square invertible matrix
## Create a list of inputs for the cacheSolve() function
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse
## 4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Set the inverse matrix to NULL
        inv <- NULL
        
        ## Set the matrix (y is in a different environment, therefore, use <<-)
        set = function(y){
                x <<- y
                inv <<- NULL
        }
        
        ## Get the matrix
        get = function() x
 
        ## Set the inverse matrix
        setinv = function(inverse) inv <<- inverse
        
        ## Get the inverse matrix
        getinv = function() inv
        
        ## Create the list of inputs for the cachesolve() function
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Return the output of the cached matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv = x$getinv()
        
        ## Check if the cached data already exists
        ## Note that you do not need to compute the inverse if it already exists 
        if (!is.null(inv)){
                message("cached data exists")
                return(inv)
        }
        
        ## if it doesn't exist then compute the inverse 
        inv_matrix = x$get()
        inv = solve(inv_matrix, ...)
        
        ## sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}

