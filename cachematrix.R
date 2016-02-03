## Create a matrix that caches the inverse function (solve).
## See inner comments for further details.

makeCacheMatrix <- function(x = matrix()) {
	# Initialize cached inverse m(atrix) to NULL
	m <- NULL

	# Setter function. If user invokes this function to set a new matrix to
	# later apply solve(), then cache gets empty.
	set <- function (y)
		{
			x <<- y
			m <<- NULL
		}

	# Getter function for the established matrix x
	get <- function() x

	# Solve routine, store into cache
	setsolve <- function(solve) m <<- solve

	# Solve routine, read from cache
	getsolve <- function() m

	# Return cache matrix structure through 4 functions
	list(
	  set = set,
	  get = get,
	  setsolve = setsolve,
	  getsolve = getsolve)
}


## cacheSolve returns the inverse of 'x'. It uses previously cached
## results if available. See inner comments for further details.

cacheSolve <- function(x, ...) {
	# Return a matrix that is the inverse of 'x'
	m <- x$getsolve()

	# Is there a cached result?
	if (!is.null(m))
	{
		message ("using solve() cached data")
		return (m)
	}

	# There isn't a cached result (the matrix x is likely to have changed
	# through set function). Get the data and then store in the cache through
	# setsolve.
	data <- x$get()
	m <- solve(x$get(), ...)
	x$setsolve(m)

	# Return result
	m
}

