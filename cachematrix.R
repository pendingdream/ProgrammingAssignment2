## For Coursera Data Science :R programming, Assignment 2
## Use cached information to reduce calculaton for the inverse of a matrix

## makeCacheMatrix will construct a speciel matrix which is really a list containing a function to
## 1.set the matrix
## 2.get the matrix
## 3.set the inverse of the matrix
## 4.get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The following function calculates the inversion matrix of the special 
## matrix created with the above function. 
## 1.First checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and put it in the cache.
## Error messages will be shown if the matrix is not invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data...")
                return(inv)
        }
        data <- x$get()
        if (!nrow(data) == ncol(data)) {
                stop("Not Square Matrix")
        }
        inv <- try(solve(data, diag(nrow(data))),silent=TRUE)
        if(class(inv) == "try-error") {
                stop("Not Invertible")
        }
        x$setinv(inv)
        inv
}
