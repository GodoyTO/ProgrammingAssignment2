## Just a simple function capable to save a function in the cache, so you can 
## latter see if it's has an inverse matrix. It does it by firstly check if the
## determinant exist, and if so, applying the inverse function to the first matrix

install.packages("matlib")
library(matlib)

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinv <- function(inv) im <<- inv
        getinv <- function() im
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Here we check if the determinant is real. If so, apply the inverse function

cacheSolve <- function(x, ...) {
        im <- x$getinv()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- inv(data, ...)
        x$setinv(im)
        im
}

## A simple test to see if both functions works

test<-makeCacheMatrix(matrix(1:4,2,2))
I<-cacheSolve(test)
a<-matrix(1:4,2,2)
a%*%I

##looks like they work fine