## Put comments here that give an overall description of what your
## functions do
##      Assignment2 = create function to calculate inverse of matrix
##      and save in cache, check with cache function

## Write a short comment describing this function
##      Part1= create function to calculate inverse "i" of a matrix

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get<-function() x
        setinverse<-function(solve) x<<-solve
        getinverse<-function() i
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
##     function to check if inverse has already been calculated, 
##     if it has then retrieve else calculate

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
