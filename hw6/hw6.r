# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).

sim.doctors <- function(initial.doctors, n.doctors, n.days, p){

  # Set up the output variable, define it as a matrix then use initial.doctors
  # to set the first column (day)
  has_adopted = matrix(initial.doctors)
  #print(has_adopted)
  
  # Run a simulation for <n.days> (use a for loop).  In the loop:
  # 1) pick two random doctors
  # 2) check if one has adopted the other hasn't
  # 3) convert the non-adopter with probability p
  
  for(i in 1:n.days) {
    a= runif(1, min= 1, max= n.doctors + 1)
    a= as.integer(a)
    b= runif(1, min= 1, max= n.doctors + 1)
    b= as.integer(b)
    while(b == a) {
      runif(1, min= 1, max= n.doctors + 1)
    }
    
    a.doc = has_adopted[a,i]
    
    b.doc = has_adopted[b,i]
    
    if(a.doc == 0 && b.doc == 1) {
      chance1 = sample(0:1, size = 1, replace = TRUE, prob= c(1-p, p))
      if(chance1 == 1) {
        rep1 = has_adopted[,i]
        rep1[a] = 1
        has_adopted = cbind(has_adopted, rep1)
      } else {
        has_adopted = cbind(has_adopted, has_adopted[,i])
      }
    } else if(b.doc == 0 && a.doc == 1) {
      chance2 = sample(0:1, size = 1, replace = TRUE, prob= c(1-p, p))
      if(chance2 == 1) {
        rep2 = has_adopted[,i]
        rep2[b] = 1
        has_adopted = cbind(has_adopted, rep2)
      } else {
        has_adopted = cbind(has_adopted, has_adopted[,i])
      }
    } else if(a.doc == 0 && b.doc == 0) {
      has_adopted = cbind(has_adopted, has_adopted[,i])
    } else if(a.doc == 1 && b.doc == 1) {
      has_adopted = cbind(has_adopted, has_adopted[,i])
    }
    
  }
  has_adopted= has_adopted[,-1]
  colnames(has_adopted) = c(paste("Day", 1:n.days))
  # return the output
  return(has_adopted)
}

# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters. sim.doctors(test, 10, 10, .5)

set.seed(42)
# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)

doctorcount = function(m){
  doctors = vector()
  for(i in 1:ncol(m)){
    doctors = c(v, length(which(m[,i] == 1)))  
  }
  return(doctors) 
}

initial.doctors = c(rep(1,10), rep(0,90))

matrix.25 = sim.doctors(initial.doctors, 100, 500, 0.25)
count.25 = doctorcount(matrix.25) 

matrix.4 = sim.doctors(initial.doctors, 100, 500, 0.4)
count.4 = doctorcount(matrix.4)

matrix.5 = sim.doctors(initial.doctors, 100, 500, 0.5)
count.5 = doctorcount(matrix.5) 

matrix.6 = sim.doctors(initial.doctors, 100, 500, 0.6)
count.6 = doctorcount(matrix.6) 

matrix.75 = sim.doctors(initial.doctors, 100, 500, 0.75)
count.75 = doctorcount(matrix.75) 

plot(x= 1:500, y= count.25, xlab = "Days", ylab = "Doctors Who've Adopted Drug", 
     main = "Drug Adoption vs Density for p-values", type = "l",
     col = "black", lwd = 3, ylim = c(0, 100))
lines(x= 1:500, y= count.4, type = "l", col = "red", lwd = 3)
lines(x= 1:500, y= count.5, type = "l", col = "purple", lwd = 3)
lines(x= 1:500, y= count.6, type = "l", col = "green", lwd = 3)
lines(x= 1:500, y= count.75, type = "l", col = "blue", lwd = 3)

color= c("black", "red", "purple", "green", "blue")
legend(10, 100, c(0.25, 0.4, 0.5, 0.6, 0.75), col = color, lwd=3, 
       title = "p-value")

