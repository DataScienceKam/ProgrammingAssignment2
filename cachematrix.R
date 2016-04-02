## Put comments here that give an overall description of what your
## functions do
## These two functions are used to create a special matrix that cache's its inveerse - computed by solve(x).
## Write a short comment describing this function
## Creates a matrix function that caches the inverse 
makeCacheMatrix <- function(x = matrix()) {
        ## set the value of the matrix
	## get the value of the matrix
	## set the value of the inverse
	## get the value of the inverse
	## variable for matrix initialized to NULL
        m = NULL
	#set value of m
        set = function(y) {
                x <<- y
                m <<- NULL
        }
        get = function() x
        setinverse = function(inverse) m <<- inverse
        getinverse = function() m
        list(set=set, 
		get=get, 
		setinverse=setinverse, 
		getinverse=getinverse)
}


## Write a short comment describing this function
## computes the inverse of the passed matrix returned by the function makeCacheMatrix(). 
## If the inverse has already been calculated and the matrix has not changed, the function retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
       
        # if the inverse has already been calculated
        if (!is.null(m)){
                # getting the value from the the cache and skipping the inverse computation.
                message("getting cached data")
                return(m)
        }
       
        # if not calculate the inverse matrix value
        data <- x$get()
	#Using solve to find the inverse matrix- following the assignment example
        m <- solve(data, ...)
       
        # sets the value of the inverse in the cache via the setinv function.
        x$setinverse(m)
       
        return(m)
}

# To run this program for testing
run <- function(mat){
	m <- makeCacheMatrix(mat)
	cacheSolve(m)	
}

#Calling the run function to run the inverse method to test the functions
set.seed(123456)
r = rnorm(1000000)
mat = matrix(r, nrow=1000, ncol=1000)
run(mat)
