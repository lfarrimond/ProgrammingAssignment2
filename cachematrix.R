##This function creates a matrix object which can cache its inverse

makeCacheMatrix <- function(x = matrix()){
    #define the argument with default as matrix
    
    inv <- NULL
    #NULL holds the value of the matrix inverse
    
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    #defines the set function to assign a new value of matrix 'x' in the parent environment
    
    get <- function() {x}
    #defines the get function
    
    setInverse <- function(inverse) {inv <<- inverse} #assigns the value of inv
    getInverse <- function() {inv} #gets the value of inv when called
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    #This enables you to refer to the functions with the $operator
    
}


## cacheSolve is a function which computes the inverse of the matrix returned by makeCacheMatrix above

cacheSolve <- function(x,...) {
    inv <- x$getInverse()
    #returns a matrix that is the inverse of x and assigns it 'inv'
    #if the inverse is already calculated then cacheSolve will retrieve it from the cache
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv) 
    inv
}
