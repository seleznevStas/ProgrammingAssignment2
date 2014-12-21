## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(matr = matrix()) {
	inversematrix = NULL
	set <- function(y) {
		matr <<- y
		inversematrix <<- NULL
	}
	get <- function() {
		matr
	}
	setinverse <- function(inverse) {
		inversematrix <<- inverse
	}
	getinverse <- function() {
		inversematrix
	}

	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
cacheSolve <- function(matr, ...) {
	inverse <- matr$getinverse()

	if(is.null(inverse)) {
		print("computed inverse")
		realmatr = matr$get()
		inverse <- solve(realmatr)
		matr$setinverse(inverse)
	}

	inverse
}