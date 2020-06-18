Introduction
This second programming assignment will require you to write an R function that is able to cache potentially time-consuming computations. For example, taking the mean of a numeric vector is typically a fast operation. However, for a very long vector, it may take too long to compute the mean, especially if it has to be computed repeatedly (e.g. in a loop). If the contents of a vector are not changing, it may make sense to cache the value of the mean so that when we need it again, it can be looked up in the cache rather than recomputed. In this Programming Assignment you will take advantage of the scoping rules of the R language and how they can be manipulated to preserve state inside of an R object.
Example: Caching the Mean of a Vector
In this example we introduce the <<- operator which can be used to assign a value to an object in an environment that is different from the current environment. Below are two functions that are used to create a special object that stores a numeric vector and caches its mean.

The first function, makeVector creates a special "vector", which is really a list containing a function to

set the value of the vector
get the value of the vector
set the value of the mean
get the value of the mean
# makeCacheMatrix.r
makeCacheMatrix <- function(x = matrix()){
     inv <- NULL
     set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() {x}
      setInverse <- function(inverse) {inv <<- inverse}
      getInverse <- function() {inv}
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){
      inv <- x$getInverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInverse(inv)
      inv
}
## Results
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()
[,1] [,2]
[1,] 1 3
[2,] 2 4
my_matrix$getInverse()
NULL
cacheSolve(my_matrix)
[,1] [,2]
[1,] -2 1.5
[2,] 1 -0.5
cacheSolve(my_matrix)
getting cached data
[,1] [,2]
[1,] -2 1.5
[2,] 1 -0.5
my_matrix$getInverse()
[,1] [,2]
[1,] -2 1.5
[2,] 1 -0.5
my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
my_matrix$get()
[,1] [,2]
[1,] 2 1
[2,] 2 4
my_matrix$getInverse()
NULL
cacheSolve(my_matrix)
[,1] [,2]
[1,] 0.6666667 -0.1666667
[2,] -0.3333333 0.3333333
cacheSolve(my_matrix)
getting cached data
[,1] [,2]
[1,] 0.6666667 -0.1666667
[2,] -0.3333333 0.3333333
my_matrix$getInverse()
[,1] [,2]
[1,] 0.6666667 -0.1666667
[2,] -0.3333333 0.3333333
Grading
This assignment will be graded via peer assessment.
