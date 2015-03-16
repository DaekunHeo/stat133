#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.

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
  image(m, axes= FALSE, col= c("white", "red", "blue"))
  return(c(Count, num1, num2))
}

bml.data = function(r,c,p) {
  samp = data.frame()
  for(i in 1:25) {
    test = bml.sim(r,c,p)
    samp = rbind(samp, test)
  }
  names(samp) = c("# of Steps", "# of 1's", "# of 2's")
  return(samp)
}
