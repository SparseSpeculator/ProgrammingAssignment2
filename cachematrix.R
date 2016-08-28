## Solving matrix and caching results

## returns list of functions to get and set a matrix and its inverse (solved)

makeCacheMatrix <- function(x = matrix()) {
  mat <- x
  solved_mat <- NA
  get_mat <- function() mat
  set_mat <- function(new_mat){
    if (identical(mat, new_mat) == FALSE) {
      mat <<- new_mat
      solved_mat <<- NA
    }
    
  }
  
  get_solved_mat <- function() solved_mat
  set_solved_mat <- function(new_solved_mat){
    solved_mat <<- new_solved_mat
  }
  
  return(list(get_mat=get_mat, 
              set_mat=set_mat, 
              get_solved_mat=get_solved_mat, 
              set_solved_mat=set_solved_mat))
}


## Solves matrix if not already solved, returnes solved matrix 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  if (is.na(x$get_solved_mat())) { 
    # Warning can be ignored! Checking the dimension would solve the warning but inflate the code unccessarily!
    print("Solving!")
    x$set_solved_mat(
      solve(x$get_mat())
                    )
  }
  return(x$get_solved_mat())
}

# Test code:
xm <- matrix(c(5,3,4,6,3,6,7,7,5), 3, 3) 
solve(xm) # Expected result

xm_cache = makeCacheMatrix(xm)
cacheSolve(xm_cache) # Solving
cacheSolve(xm_cache) # Accessing cache
xm_cache$set_mat(matrix(c(5,3,4,6,3,6,7,7,5), 3, 3) ) # Matrix is unchanged / identical
cacheSolve(xm_cache) # Accessing cache
xm_cache$set_mat(matrix(c(6,3,4,6,3,6,7,7,5), 3, 3) ) # Matrix is changed
cacheSolve(xm_cache) # Solving
cacheSolve(xm_cache) # Accessing cache
