## The fuction "makeCacheMatrix" creates a special "matrix", which is a list containing functions to
## set the value of the matrix,
## get the value of the matrix,
## set the value of the inverse matrix,
## get the value of the inverse matrix.

makeCacheMatrix <- function(M = matrix()) {
        
        Inv <- NULL
        
        set <- function(new_M){
                
                M <<- new_M
                
                Inv <<- NULL
        }
        
        get <- function(){
                
                M
        }
        
        setInverse <- function (new_Inv){
                
                Inv <<- new_Inv
        }
        
        getInverse <- function(){
                
                Inv
        }
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The function "cacheSolve" find the inverse of a "matrix object" created by "makeCacheMatrix".
## It first checks to see if the inverse matrix has already been calculated. 
## If so, it gets the inverse from the cache.

cacheSolve <- function(Cache_M, ...) {
        ## Return a matrix that is the inverse of 'Cache_M'
        
        Inverse <- Cache_M$getInverse()
        
        if(!is.null(Inverse)) {
                message("getting cached data")
                return(Inverse)
        }
        
        Mat <- Cache_M$get()
        
        Inverse <- solve(Mat)
        
        Cache_M$setInverse(Inverse)
        
        Inverse
}
