#The cachematrix.R file contains two functions, makeCacheMatrix() and cachesolve()

#makeCacheMatrix function is for caching the invers of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(inverse) 
    {
        inv <<- inverse
    }
    
    getinv <- function() inv
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The cachesolve function checkes if the inverser of a matrix is null or already cached

#Based on the if function outcome returns the inverse from cache or computes 

#caches the inverse and returns the inverse

cacheSolve <- function(x)
{
    inv <- x$getinv()
    if(!is.null(inv)) 
    {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    
    #Error handling to check if the determinant of the given matrix input is 0
    
    MatDeterminant<-det(data)
    if(MatDeterminant==0)
    {
        message("Determinant of matrix is 0,Matrix cannot be inverted, please choose differnt matrix data set")
    }
    else 
    {
        inv <- solve(data)
        x$setinv(inv)
        inv
    }
}
