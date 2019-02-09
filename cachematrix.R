## My first function creates a list of functions and two objects,
## one of which is the matrix passed as an argument to the function (x),
## and the other one being the object m, which is set to null the first
## time cacheSolve is ran on a specific matrix. The functions serve to 
## set and then retrieve information.

## My second function uses the list created by makeCacheMatrix
## and retrieves its elements to check first whether the inverse of 
## the matrix provided has already been calculated, and if not, calculates
## the inverse and then prints it to the console.

## makeCacheMatrix description
## Object m is created in the environment of makeCacheMatrix and is set
## to null. 
## Four functions are created: set (to set m to null the first time a given
## matrix is passed), get(to retrieve the matrix x), setinverse (to create m
## in the global environment equal to inverse, the argument) and  
## getinverse(to retrieve the value of the inverse).
## Then a list is created with specific names for every element.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve description
## Creates an object m retrieving the element called 
## getinverse from the argument x, which is the list creates
## with makeCacheMatrix. If this element is not null, it then returns 
## the inverse previously calculated. Otherwise, it creates an object
## called data, that retrieves the matrix stored by makeCacheMatrix. 
## It then uses the matrix in data to calculate the inverse and
## store it in m, and it finally stores m using the setinverse function. 
## It prints m.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
