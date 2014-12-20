## This pair of functions cache the inverse of a matrix. The inverse is calculated using the 'solve' function.

## makeCacheMatrix creates a list of functions to:
##   1. Set the values of the matrix
##   2. Get the values of the matrix
##   3. Set the values of the matrix inverse
##   4. Get the values of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {  ## Defines x as an empty matrix
	
	inv <- NULL ## Sets matrix inverse to NULL
	
	## Set the values of the matrix (1)	
	set <- function(y) {
			
			x <<- y ## Sets x to be the argument matrix, y
			inv <<- NULL ## Since the matrix has been redefined, inverse is reset to be NULL
			
	}
	
	## Get the values of the matrix (2)
	get <- function() x
	
	## Set the values of the matrix inverse (3)
	setInverse <- function(solve) inv <<- solve
	
	## Get the values of the matrix inverse (4)
	getInverse <- function() inv
	
	## Create a list of the functions (1-4)
	list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
		 
}

## cacheSolve returns a matrix that is the inverse of the input matrix, x.  If the inverse has been
## previously calculated (i.e. inv != NULL), the cached inverse is returned.  If the inverse has not
## been previous calculated (i.e. inv == NULL), the inverse is calculated and returned.

cacheSolve <- function(x, ...) { ## x is the matrix whose inverse is to be calculated
		
	inv <- x$getInverse() ## Returns current value of inverse
	
	if(!is.null(inv)) { ## Inverse value is not NULL and has therefore already been calculated
	
			message("getting cached inverse")
			return(inv) ## Return the cached inverse of the matrix, x

	}
	
	else { ## Inverse value is NULL, so it must be calculated
	
		message("inverse not in cache, calculating...")
		data <- x$get() ## Get the values of the matrix, x
		inv <- solve(data, ...) ## Use the 'solve' function to calculate the matrix inverse
			## The solve(a, b, ...) functions solves the equation a %*% x = b for x, where b can be either a vector or a matrix.
			## If a value for b is not specified, it is taken to be an identity matrix.
			## The product of a matrix and its inverse is an identify matrix, so solving for x in the above equation will
			## return the inverse when no b is specified (i.e. b is an identity matrix).
			## It is assumed that the matrix provided is invertible.
		x$setInverse(inv) ## Sets the values for the inverse of matrix, x
		inv ## Return the newly calculated inverse of the matrix, x

	}
	
}