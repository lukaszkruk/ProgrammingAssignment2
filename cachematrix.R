#The below pair of functions creates a cache of matrix inverse and 
#accesses it as necessary - if a matrix inverse is required and not
#calculated, it is calculated from scratch. If the inverse has 
#already been calculated, it is returned from cache rather than
#recalculated anew.

#The first function takes input matrix and stores is in a local 
#variable. It also defines methods for calculating the inverse and
#storing it in another local variable. Finally, it defines way of
#accessing the inverse from the local variable (cache).

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
	
}

#The second function checks if the inverse has been calculated and, if so
#returns the inverse from cache or, if not, calculates the inverse (solves
#the matrix) and returns the result.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}