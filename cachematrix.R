## Create a matrix with functionality to cache inverse of a matrix to reduce computation time

## Cache Matrix object.
## Argument x is a regular R matrix
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y)
	{
		x <<- y
		inverse <<- NULL
	}
	
	get <- function()
	{
		x
	}
	set_inverse <- function(new_inverse) 
	{
		inverse <<- new_inverse
	}
	get_inverse <- function()
	{
		inverse
	}
	list(set = set, 
		get = get, 
		set_inverse = set_inverse, 
		get_inverse = get_inverse)
}


## Returns inverse of a matrix
## Argument x is a cached matrix
## Calculates inverse if not already cached. 
## If cached, returns inverse from cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$get_inverse()
        
        if (!is.null(inverse))
        {
        	message("Getting cached data")
        	return(inverse)
        }
        
        original_matrix <- x$get()
        
        inverse <- solve(original_matrix)
        x$set_inverse(inverse)
        inverse
}
