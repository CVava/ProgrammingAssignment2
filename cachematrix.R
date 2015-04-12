# pair of functions that cache the inverse of a matrix

# makeCacheMatrix: creates a special "matrix" object that can cache its inverse
# makes available functions to get / set the content of the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL								# clear an eventual local copy of m
        set <- function(y) {					# set the value of the vector
                x <<- y
                m <<- NULL						# inverse hasn't been computed yet 
        }
        get <- function() x						# get the value of the matrix
        setminv <- function(minv) m <<- minv	# set the value of the inverse
        getminv <- function() m					# get the value of the inverse

        list(set = set, get = get,				# return as list functions created here
             setminv = setminv,
             getminv = getminv)
}


# cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix
# If the inverse has already been calculated (and the matrix has not changed), 
# then cachesolve retrieves the inverse from the cache.
# Computing the inverse of a square matrix is done with the solve function. 
# Assume the matrix is square and always invertible, no tests required
cacheSolve <- function(x, ...) {				
        m <- x$getminv()						# get from cache the inverse of matrix 'x'

        if(!is.null(m)) {						# checks to see if the inverse has already been calculated
                message("getting cached data")	# ! should be commented out after testing !
                return(m)						# If so, gets the inverse from the cache and return
        }

        data <- x$get()							# Else get the matrix,
        m <- solve(data, ...)					# calculates its inverse, and
        x$setminv(m)							# sets it in the cache (setminv)

        m										# return value
}
