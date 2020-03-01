## Matrix
makeCacheMatrix <- function(x=matrix()){
  inv <- NULL
  set <- function(y){
      x <<- y
      inv <<- NULL
  }
  get <- function() x

    setinv <-function(inverse) inv <<-inverse
    getinv <- function() inv
  list(get=get, set=set, getinv=getinv, setinv=setinv)
}

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("inverse is cached")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m, ...)
  x$setinv(inv)
  inv
}

# Testing the Function

## Define the matrix
m <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
m
     [,1] [,2]
[1,]    0    2
[2,]    1    0

## Store function inverse
m2 <- makeCacheMatrix(m)

## Solve the inverse and store in a new matrix
m3 <- cacheSolve(m2)
     [,1] [,2]
[1,]  0.0    1
[2,]  0.5    0

## Testing the inverse
m%*%m3
     [,1] [,2]
[1,]    1    0
[2,]    0    1

m3%*%m
     [,1] [,2]
[1,]    1    0
[2,]    0    1
