#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
  m= matrix(nrow= r, ncol= c)
  ones= round(r*c*p)
  total= vector()
  total= c(rep(0, r*c - ones), rep(1, ones/2), rep(2, ones/2))
  for(i in 1:r) {
    for(j in 1:c) {
      m[i,j] = sample(0:2, 1, replace= TRUE, prob= c(1-p, p/2, p/2))
    }
  }
   return(m)
}

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

bml.step <- function(m){
  n = m
  numcol = ncol(m)
  numrow = nrow(m)
  for(i in 1:nrow(m)) {
    for (j in 1:ncol(m)) {
      if (n[i,j]== "1") {
        m = move.east(m, i, j)
      } else if (n[i,j]== "2") {
        m = move.north(m, i, j)
      }
    }
  }
  grid.new = FALSE
  if(identical(m,n)) grid.new = FALSE
  else grid.new = TRUE
   return(list(m, grid.new))
}

move.east = function(m, i, j) {
  if (j == ncol(m)) {
    check1= m[i, j+1 - ncol(m)]
    if (check1== "0") {
      m[i, j+1 - ncol(m)] = 1
      m[i,j] = 0
    }
  } else {
    check1= m[i, j+1]
    if (check1== "0") {
      m[i, j+1] = 1
      m[i,j] = 0
    }
  }
  return (m)
}

move.north = function(m, i, j) {
  if (i == "1") {
    check2 = m[i - 1 + nrow(m), j]
    if (check2 == "0") {
      m[i - 1 + nrow(m), j] = 2
      m[i,j] = 0
    }
  } else {
    check2 = m[i-1, j]
    if (check2 == "0") {
      m[i - 1, j] = 2
      m[i,j] = 0
    }
  }
  return(m)
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
  m = bml.init(r,c,p)
  image(m, axes= FALSE, col = c("white", "red", "blue"))
  ret = list()
  Done = TRUE
  Count = 0
  num1 = length(which(m == "1"))
  num2 = length(which(m == "2"))
  while(Done == TRUE) {
    ret = bml.step(m)
    m = ret[[1]]
    Done = ret[[2]]
    Count = Count + 1
    if(Count == 10000) Done = FALSE
  }
  image(m, axes= FALSE, col = c("white", "red", "blue"))
  return(Count, num1, num2)
}
