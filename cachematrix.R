# makeCacheMatrix creates a list of different functions for matrix x

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL                 # creates empty object that will later hold the inverse matrix
        set <- function(a) {            # set(): sets the value of the matrix
                x <<- a
                inverse <<- NULL        # deletes old values of inverse
        }
        get <- function() {             # get(): gets the value of the matrix
                x
        }
        setinverse <- function(i) {
                inverse <<- i       # setinverse(): caches the inverse
        }
        getinverse <- function() {              # getinverse(): gets the cached inverse
                inverse
        }
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
	
}

# cacheSolve returns the cached inverse of matrix x if already calculated
# otherwise the function calculates and caches the inverse

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()     # assigns cached inverse to i
        if(!is.null(inverse)) {       # returns cached inverse if there actually was a cache
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)   # calculates the inverse
        x$setinverse(inverse)         # caches the inverse
        inverse                       # returns the inverse
}

