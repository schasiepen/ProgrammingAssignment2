## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix function will store a matrix for futher use
## USE example
## 1. Step: create matrix
## my_matrix <- matrix(runif(9,1,100),3,3)
## 2. Step: store the matrix
## cached_matrix <- makeCacheMatrix(my_matrix)
## 3. Step: use cacheSolve first time: inverse matrix is calculated
## test <- cacheSolve(cached_matrix)
## 4. Step: use cacheSolve : use inverse matrix from cache
## test <- cacheSolve(cached_matrix)
makeCacheMatrix <- function(x = matrix()) {
        ##m will store the inverse matrix
        m <- NULL
        ## setter function if more than one matrix is needed
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## get function returns stored matrix
        get <- function() x
        ## setinvert stores the inverse matrix
        setinvert <- function(invert) m <<- invert
        ## getinvert will return inverse matrix
        getinvert <- function() m
        ## return list of functions output
        list(set = set, get = get,
             setinvert = setinvert,
             getinvert = getinvert)
        

}


## Write a short comment describing this function
## cacheSolve will retrieve the inversed matrix if it exists
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Stop, if cachSolve is used with wronng argument
        if(typeof(x) != "list")
        {
           stop("Please use makeCacheMatrix(yourmatrix) to create the cache object first!")
                
        }
        ## Get the inverse matrix
        m <- x$getinvert()
        ## Test if inverse matrix already exists, return martix, if test == true
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## Get data (x$get(), calculate matrix (solve), store result (x$setinvert(m)), output result
        data <- x$get()
        m <- solve(data, ...)
        x$setinvert(m)
        m
        
}
