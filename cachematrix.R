## The program noted below is part R Programming Assignment 2: Lexical Scoping for
## Computing the inverse of a square matrix 


## The first fucntion will output a list which will:
# set the value of the vector
# get the value of the vector
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) 
  {
  m<-NULL
  set<-function(y)
  {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinv<-function(inv) m<<- inv
  getinv<-function() m
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)
  }


## The second function cacheSolve calls functions from the output of the  makeCacheMatrix function note above
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache
## If the input is new, it calculates the inverse of the data using the "solve" function and sets the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) 
  {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get() 
  m<-solve(matrix, ...)
  x$setinv(m)
  m  
  }
