## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix gets, sets both matrix and its inverse. 
## Four methods -> get(), set(), getinverse(), setinverse()
## Input is a matrix x.
## m_inv the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m_inv <- NULL
        
        get <- function() x
        getinverse <- function() m_inv
        
        set <- function(y) {
                x <- y
                m_inv <- NULL     ##when a new matrix is assigned to x
        }
        
        setinverse <- function(new_inverse) m_inv <<- new_inverse
        
        #method list
        list(get=get, set = set, getinverse = getinverse, setinverse = setinverse)
}


## Write a short comment describing this function
## cacheSolve makes if the inverse is available in Cache.
## If so, it returns the value from cache.
## If not, it compute the inverse using SOLVE and then stores it into cache.
## input is matrix x (which was set using makeCacheMatrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inv <- x$getinverse()
        if(!is.null(m_inv)) {
                message("Getting Cached Data...") 
                message("Inverse of the matrix from cache is as below:")
                return(m_inv)
        }
        message("Costly operation... Data was not cached. Inverse of the matrix is as below:")
        matri <- x$get()
        m_inv <- solve(matri)
        x$setinverse(matri)    #This will cache the new computed inverse 
        m_inv
        
}



## Testing the code:

## m <- matrix(c(1,2,3,4), nrow=2, ncol=2)
## k <- matrix(c(4,3,2,1), nrow=2, ncol=2)
## r <- matrix(c(6,7,8,9), nrow=2, ncol=2)

## a <- makeCacheMatrix(m)
## b <- makeCacheMatrix(k)

##cacheSolve(a)
##cacheSolve(b)
##cacheSolve(a)

## a <- makeCacheMatrix(r)
##cacheSolve(a)