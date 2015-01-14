## My function at first creates a special matrix object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
	 m <- NULL
  set <- function(y){
    x <<- y  
    m <<- NULL  
  }
  get <- function() x 
  setInverse <- function(solve) m<<- solve 
  getInverse <- function() m 
  list(set = set, get = get,
       setInverse = setInverse,
      getInverse = getInverse) 
}


## Then the function below computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
	m <- x$getInverse()                 
	if(!is.null(m)){                    
	message("getting cached data")    
	return(m)                           
	 }
	data <- x$get()                     
	m <- solve(data, ...)               
	x$setInverse(m)                     

## Return a matrix that is the inverse of 'x'
