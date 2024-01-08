#-----------------------------------------------------------#
# Deterministic SEIRD epidemic model in a closed population #
#-----------------------------------------------------------#

#install.packages("deSolve", dependencies = T)
library(deSolve)
library(ggplot2)

SEIRDmodel <- function(t, x, parms)
{
  S <- x[1]
  E <- x[2]
  I <- x[3]
  R <- x[4]
  D <- x[5]
  N <- x[6]
  
  with(as.list(parms), {
    dS <- -beta*S*I/N
    dE <-  beta*S*I/N - gamma*E
    dI <-  gamma*E - sigma*I - delta*I
    dR <-  sigma*I
    dD <-  delta*I
    
    dN <- dS + dE + dI + dR + dD
    list(c(dS, dE, dI, dR, dD, dN))
  }) 
}

times <- seq(0, 175, by = 1) 
parms <- c(beta = 0.35, gamma = 1/7, sigma = 1/21,	delta = 0.05/7)
xstart <- c(S = 499, E = 0, I = 1, R = 0, D = 0, N = 500)

SEIRD.out <- as.data.frame(lsoda(xstart, times, SEIRDmodel, parms))

# dim(SEIRD.out)
# head(SEIRD.out)
# tail(SEIRD.out)

# Function to calculate R0 for SEIRD model
calculate_R0_SEIRD <- function(beta, sigma, gamma, delta) {
  R0 <- beta / (sigma + gamma + delta)
  return(R0)
}

# Parameters from the SEIRD model
beta <- 0.35
sigma <- 1/21
gamma <- 1/7
delta <- 0.05/7

# Calculate and print R0 for SEIRD model
R0_SEIRD <- calculate_R0_SEIRD(beta, sigma, gamma, delta)
cat("Basic Reproduction Number (R0) for SEIRD model:", R0_SEIRD, "\n")

# Write a code for plotting using ggplot

plot(times,  SEIRD.out$S, col = "blue", type = "l", lwd = 2, bty = "l", xlab = "Time", ylab = "Compartment Size", main = "SEIRD Epidemic Model")
lines(times, SEIRD.out$E, lwd = 2, col = "green")
lines(times, SEIRD.out$I, lwd = 2, col = "black")
lines(times, SEIRD.out$R, lwd = 2, col = "red")
lines(times, SEIRD.out$D, lwd = 2, col = "purple")
abline(v = 0, h = 0)
legend(150, 500, c("S", "E", "I", "R", "D"), col = c("blue", "green", "black", "red", "purple"), lty = c(1,1,1,1,1))

# beta is the coefficient of transmission
# gamma is the latency
# sigma is the recovery rate
# delta is the death rate
# delta is the lethality rate