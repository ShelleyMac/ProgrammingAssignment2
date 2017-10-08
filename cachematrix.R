## Create a matrix object that can cache its inverse. Then compute the inverse of
## the matrix object or return the previously calculated inverse from the cache


makeCacheMatrix <- function(x = matrix()) { ## declare x as an empty matrix
        
	inv <- NULL 			## initialise inv as object in makeCacheMatrix
	                                ## environment
	
	set <- function(y) {
			x <<- y         ## assign value of y to object x in the parent
			                ## environment
			                
			inv <<- NULL    ## assign valuve of NULL to inv object in
			                ## parent environment
	
	}
	get <- function() x             ## R gets x from parent environment
	                               
	setInverse <- function(solve) inv <<- solve ## assign solve to value of inv
	                                            ## in parent environment
	
	getInverse <- function() inv    ## get inv from parent environment
	
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
	##assign each functions as elements in a list and return to parent environment
}


## Retrieve inverse from x object, check if the result is not NULL. If result is not NULL - 
## return cached inverse. If result is NULL calculates the inverse, sets the inverse and then
## return value of inverse to parent environemnt.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       
        inv <- x$getInverse() ## get inverse from x object passed in as argument
       
        if(!is.null(inv)) {
        	message("getting cached data")
        	return(inv) ## If inv is not null, return the cached value
        }
        data <- x$get() ## If inv is null, get the matrix from x
        inv <- solve(data, ...) ## calculate the inverse of the matrix
        x$setInverse ## set the inverse in x
        inv         ## return value of inv
}
 