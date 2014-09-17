## A pair of functions that cache the inverse of a matrix.


#This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) 
{
        i <- NULL 
        set <- function(y)   #Define function to set the matrix value
		{
                x <<- y
                i <<- NULL
        }
        get <- function() x #Define function to get the matrix value
        setinverse <- function(inverse) i <<- inverse   #Define function to set inverse of the matrix value
        getinverse <- function() i  #Define function to get the inverse of the matrix value
        list(set = set, get = get,  #Create the list special list containing posible operations
             setinverse = setinverse,
             getinverse = getinverse)

}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) 
{
        i <- x$getinverse()   #Trying to get inverse value from cache
        if(!is.null(i))       #if the value exists, its obtained from cache
		{
                message("getting cached data")
                return(i)
        }
        data <- x$get()       #if the value doesn't exist, get the matrix
        i <- solve(data)      #and do the operation
        x$setinverse(i)       #set the result of the operation to the special matrix 
        i					  #return the value calculated	
		## Return a matrix that is the inverse of 'x'  
}
