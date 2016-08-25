##makeCacheMatrix() builds a set of functions and returns the functions within a list to the parent environment. 
##More specifically the function creates an R object that stores a matrix and its inverse.
## b <- Null sets the inverse to Null as a place holder for a future value
## set<-function(c) {q <<-c; b<<-Null} defines a function to set the vector q to a new vector c, and rests the inverse b to Null
## get <-function() q returns the vector q from the parent environment
##setinverse <-function(solve) b<<-solve sets the inverse, b, to solve
##getinverse<-function()b returns the inverse from the parent environment 
##list(set=set...) returns the "special vector" containing all the functions just defined

makeCacheMatrix <-function(q = matrix()) {
	 		b <-NULL
	 		set <-function(c) {
	 			q <<- c
	 			b <<- NULL
	 		}
			get <- function() q
			setinverse <-function(solve) b <<-solve
			getinverse <-function() b
			list(set = set, get = get,
				setinverse = setinverse,
				getinverse = getinverse)

##cacheSolve requires an argument/input from makeCacheMatrix in order to retrieve the inverse from
##the cached value that is stored in the makeCacheMatrix() object`s environment

cacheSolve <- function(q, ...){
			b <- q$getinverse()
			if(!is.null(b)) {
				message("getting cached data")
				return(b)
			}
			data <-q$get()
			b <-solve(data,...)
			q$setinverse(b)
			b
}
