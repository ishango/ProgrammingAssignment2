## Rather than repeatedly compute the inverse of a matrix, the following two functions will solve the inverse once and
## cache it or retrieve it if it is already in the cache.

## This function creates a matrix object that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## This function computes the inverse of the matrix from the makeChacheMatrix function. If the matrix hasn't changed,
## and the inverse was previously calculated then it will retrieve the inverse matrix from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getmatrix()        ##load matrix into m and see if it is cached
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()         ##if matrix isn't in the cache, solve for inverse
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}
