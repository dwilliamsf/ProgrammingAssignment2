## The makeCacheMartix and cachesolve function works together to allow inverse
## matrix calculations to be stored and returned if needed.

## The MakeCacheMatrix function creates a list containing that contains function:
## The set_matrix functions allowing you to set or change the matrix you are running 
## The get_matrix funcion returns the current matrix in the list
## The get_inverse will return the cached invers or NULL if it is not cached
## The set_inverse will store the inverse into the inverse_already_cached variable.


makeCacheMatrix <- function(x = matrix()) {
     Inverse_already_cached <- NULL
     set_matrix <- function(new_matrix) {
          x <<- new_matrix
          Inverse_already_cached <<- NULL
     }
     get_matrix <- function() {x}
     set_inverse <- function(Cache_inverse) {Inverse_already_cached <<- Cache_inverse}
     get_inverse <- function() Inverse_already_cached
     list(set_matrix = set_matrix, get_matrix = get_matrix,
          set_inverse = set_inverse,
          get_inverse = get_inverse)
}



## The cacheSolve function will use the maekCacheMatrix list functions.
## it will check the get_inverse function to see if a value is already cached
## if it is, it will return that value.  If it is not it will calculate the inverse
## and use the set inverse function to cache that value for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     Cached_inverse <- x$get_inverse()
     if(!is.null(Cached_inverse)) {
          message("getting cached data")
          return(Cached_inverse)
     }
     data <- x$get_matrix()
     Cached_inverse <- solve(data, ...)
     x$set_inverse(Cached_inverse)
     Cached_inverse
}
