## This function contains a list of functions in order to set or get a Matrix inside it.
## It also keeps track of the previous matrix everytime a new one is set, in order to perform a comparison to determine if a cache needs to be recalculated.

makeCacheMatrix <- function(a = matrix()) {
	## Initializing
	cachedMatrix <- NULL
	previousMatrix <- NULL
	m <<- NULL
	
	set <- function(a) {
		## We set the current matrix to the previous matrix to store it
		if(!is.null(m)) {
			previousMatrix <<- m
		}
		
		## Then, we set the new matrix as the current one
		m <<- a
		cachedMatrix <<- NULL
	}
	
	get <- function() m
	getPrevious <- function() previousMatrix
	setCache <- function(x) cachedMatrix <<- x
	getCache <- function() cachedMatrix
	list(	set = set,
		get = get,
		getPrevious = getPrevious,
		setCache = setCache,
		getCache = getCache)
}

## This function first compares the previous (if any) matrix with the current one, to see if the cache needs to be recomputed.
## If the matrices are different, then solve the new matrix then return the result. If the matrices are the same, no need to recompute, just fetch the cache then return it.
cacheSolve <- function(m) {
	## Fetch the current matrix
	currentMat <- m$get()
	
	## Fetch the previous matrix, if there's one
	previousMat <- m$getPrevious()

	## Verifying first that the matrix has not changed by comparing the current with the previous
	if(!identical(currentMat, previousMat) && !is.null(previousMat)) {
		print("Matrix has changed. Setting cache to NULL to recompute it.")
		m$setCache(NULL)
	}
	
	## Get the cache matrix to evalute it
	cachedMatrix <- m$getCache()
	
	## If valid cache, no need to compute it. Return it right away.
	if(!is.null(cachedMatrix)) {
		print("Cached matrix is found, fetching it.")
		return(cachedMatrix)
	} else {
		print("Cached matrix is NOT found. Solving.")
		matrix <- m$get()
		
		cachedMatrix <- solve(matrix)
		m$setCache(cachedMatrix)
		return(cachedMatrix)
	}
}

## Calling code execution, which can be used to test the functions above
mat <- matrix(c(2,2,3,2),2,2)
b <- makeCacheMatrix(mat)
b$set(mat)
cacheSolve(b)
mat2 <- matrix(1:4,2,2)
b$set(mat2)
cacheSolve(b)
