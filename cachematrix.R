## It is an R function to cache the inverse of a matix. It is useful and save time when the dimension of the matrix is large and
## the content of its inverse is needed repeatedly. When the inverse of a matrix is first needed, it is computed and stored in
## cache. Susequently, when it is repeatedly needed, computation is no longer required as it can be retrieved from cache. In 
## this manner, processing time could be minimised.  

## The first function is a list containing four sub-functions to: 
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse.


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                                   # initialize  the internal matrix
    set <- function(y) {                        # set the internal matrix to the passed argument
        m<<- NULL                               # reset the cached inverse  
    }
    get <- function() x                         # return the internal matrix
    setinv <- function(solve) m <<- solve       # set the cached inverse 
    getinv <- function() m                      # return the cached inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)                       # return the list of functions available 
}

## The second function is to check if the inverse of matrix is already in cache. If it is, get the inverse from cache, otherwise 
## compute the inverse and store it in cache.

cacheSolve <- function(x, ...) { 
     m <- x$getinv()                            # get the cached inverse from cache
     if(!is.null(m)) {                          # check if the cached inverse exists in cache
          message("getting cached data")        # display a message when it exists
          return(m)                             # return cached inverse 
     }
     data <- x$get()                            # get the matrix
     m <- solve(data)                           # compute the inverse
     x$setinv(m)                                # store it in cache 
     m
} 
