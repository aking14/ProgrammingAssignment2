makeCacheMatrix <- function(x=matrix()) {
      ## List of functions that caches the inverse of a matrix.
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInversa <- function(inverse) m <<-inverse
      getInversa <- function() m
      list(set = set, get = get,
           setInversa = setInversa,
           getInversa = getInversa)
      
}

cacheSolve <- function(x, ...) {
      ## Computes the inverse of the matrix returned by makeCacheMatrix(), 
	  ## If the inverse has already been calculated, it retrieves it from the cache.
      m <- x$getInversa()
      if ( ! is.null(m)) {
            print("Getting data from cache")
            return(m)
      }
      print("Calculating inverse matrix.")
      m <- solve(x$get())
      x$setInversa(m)
      m
}

