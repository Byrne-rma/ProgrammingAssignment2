## These functions calculate and cache the inverse of a matrix
##  Given a matrix p, assign q <- makeCacheMatrix(p)
##  then call cacheSolve(q) 

## makeCacheMatrix Creates a list to store functions that cache the matrix and 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- matrix()
        setx <- function(y) {
                x   <<- y
                inv <<- matrix()
        } 
        getx <- function() x
        setinv <- function(invx)  inv <<- invx       
        getinv <- function() inv
        list(setx = setx,
             getx = getx,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve uses the the list to see if there is a cached inverse, retrieves
## it if yes and calculates it if no. 

cacheSolve <- function(q, ...) {
        inv <- q$getinv()
        if(is.numeric(inv))  {
                message("getting cached data")
                return(inv)
        }               
        data <- q$getx()   
        inv <- solve(data)
        q$setinv(inv)
        inv
}
