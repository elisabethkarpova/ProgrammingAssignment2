## This function creates a matrix and cache inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	if (ncol(x)==nrow(y)) {
			inv <- NULL
			set <- function(y) {
				x <<- y
				inv <<- NULL
			}
			get <- function() x
			setInverse  <- function(inverse) 
			inv <<- inverse
			getInverse <- function() inv 
			list(set=set,
		 		get=get,
		 		setInverse=setInverse,
		 		getInverse=getInverse)
}else{
		return(message("The matrix is not invertible"))	
}
}


## This function returns inversed matrix 

cacheSolve <- function(x, ...) {
     inv <- x$getInverse()
	 if (!is.null(inv)) {                 message("getting cached data")                 return(inv)         }
        mat <- x$get()
        int <- solve(mat, ...)
        x$setInverse(inv)
        inv

}
