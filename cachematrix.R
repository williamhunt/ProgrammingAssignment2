# These two functions work together to use a core matrix, coreM, and cache its
# inverse, coreI, for reuse. The first function, makeCacheMatrix(), returns a
# list of matrix functions.  The second function, cacheSolve(), uses the
# functions within makeCacheMatrix() to return coreI efficiently. The operator
# "<<-" is used to cache coreM and coreI in the environment of MakeCacheMatrix()
# where both these values are stored and accessible.
# 
# Example:
# > coreM <- matrix(7:4,2,2)  ## create core matrix
# > coreM
#         [,1] [,2]
# [1,]    7    5
# [2,]    6    4
# > cMcache <- makeCacheMatrix(coreM)  ## make cache
# > cMcache$get()  ## get core matrix from cache
#         [,1] [,2]
# [1,]    7    5
# [2,]    6    4
# > cMcache$getinverse()  ## see it there is an inverse
# [1] NA
# > coreI <- cacheSolve(cMcache) ## create inverse
# > cMcache$getinverse()  ## see it there is an inverse
#         [,1] [,2]
# [1,]   -2  2.5
# [2,]    3 -3.5
# > ## now there is an inverse cached
# > cacheSolve(cMcache)  ## solve by retrieval
# getting cached matrix
#         [,1] [,2]
# [1,]   -2  2.5
# [2,]    3 -3.5
# > coreM %*% coreI  ## test inverse
#         [,1] [,2]
# [1,]    1    0
# [2,]    0    1


# This function creates functions to get and set both coreM and coreI. The set
# functions cache the values. A list of the functions is returned.
        
makeCacheMatrix <- function(x = matrix()) {
        IM <- NA
        set <- function(y) {
                x <<- y
                IM <<- NA
                }
        get <- function() x
        setinverse <- function(inverseM) IM <<- inverseM
        getinverse <- function() IM
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)    
}


# This function uses the "get" and "set" functions to see if the inverse, coreI,
# exists, i.e., was previously calculated (or set). If coreI is cached, it
# returns it. Otherwise, coreI is calculated, cached (with the "setinverse"
# function), and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        IM <- x$getinverse()
        if ((length(IM) > 1) || (!is.na(IM))) { ## checks NA only is 1x1
                message("getting cached matrix")
                return(IM)
        }
        data <- x$get()
        IM <- solve(data, ...)
        x$setinverse(IM)
        IM
}