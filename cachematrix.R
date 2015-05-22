## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL #set the inverse of the matrix to NULL. It will be a placeholder for the future inverse matrix
        
        set <- function(newmtrx) {
                x <<- newmtrx
                inv <<- NULL
        } #this is the function to set the matrix, x, to the matrix, newmtrx, and resets the inverse matrix to NULL
        
        get <- function() x #returns the matrix
        
        setinvmatrix <- function(solve) inv <<- solve #sets the solve (s) to inverse matrix
        getinvmatrix <- function() inv #returns the inverse matrix
        
        list(set = set, get = get,
             setinvmatrix = setinvmatrix,
             getinvmatrix = getinvmatrix) #returns the list containing all defined functions
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        inv <- x$getinvmatrix()

        # check if inverse matrix has been calculated and return the value 
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)  
        }
        
        # if inverse is not yet calcualted, call get() to get the underlying matrix
        data <- x$get()
        
        # calculate the inverse of the underlying matrix
        
        inv <- solve(data, ...)
        # now set the inverse matrix in inv 
        x$setinvmatrix(inv)
        # return the caching inverse matrix
        inv
}
