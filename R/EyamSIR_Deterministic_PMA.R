#---------------------------------------------------------#
# Deterministic SIR epidemic model in a closed population #
#---------------------------------------------------------#
#           by Ashok Krishnamurthy, PhD           	      #
#---------------------------------------------------------#

#install.packages("deSolve", dependencies = T)
library(deSolve)

SIRmodel <- function(t, x, parms)
{
	S <- x[1]
	I <- x[2]
	R <- x[3]
	N <- x[4]

	with(as.list(parms), {
	dS <- -beta*S*I
	dI <-  beta*S*I - gamma*I
	dR <-  gamma*I
	
	dN <- dS + dI + dR

	list(c(dS, dI, dR, dN))
	})
}

times  <- seq(0, 8, by = 1)
parms  <- c(beta = 0.0178, gamma = 2.73)
xstart <- c(S = 254, I = 7, R = 0, N = 261)

SIR.out <- as.data.frame(lsoda(xstart, times, SIRmodel, parms))
# dim(SIR.out)
# SIR.out

##########################
# The S-I-R Density Plot #
##########################

plot(times,  SIR.out$S, ylim = c(0, 261), type = "l", lwd = 2, col = "blue", xlab = "Time", ylab = "Compartment Size", main = "The Eyam Plague (1665-66)")
lines(times, SIR.out$I,  lwd = 2,col = "green")
lines(times, SIR.out$R,  lwd = 2,col = "red")
#abline(v = 0, h = 0)
legend(6.5, 250, c("S(t)", "I(t)", "R(t)"), col = c("blue","green","red"), lty = c(1,1,1))

###############################################################################
# Declare global variables used to calculate the right hand sides of the ODEs #
###############################################################################

S_0 <- 254
S_Inf <- 83
I_0 <- 7
K <- S_0

(beta_gamma_ratio <- log(S_0/S_Inf)/(K - S_Inf))
(R_0 <- K*beta_gamma_ratio)
(gamma_beta_ratio <- 1/beta_gamma_ratio)

########################
# Parameter Estimation #
########################

(gamma <- 1/(11/30))
(beta <- gamma/gamma_beta_ratio)

(Imax <- (S_0 + I_0) - (gamma_beta_ratio*log(S_0)) - gamma_beta_ratio + (gamma_beta_ratio*log(gamma_beta_ratio)))

############################
# The S-I Phase Plane Plot #
############################

trueSi <- c(254, 235, 201, 153.5, 121, 110, 97, 83)
#trueIi <- c(7, 14.5, 22, 29, 20, 8, 8, 0)
It <- -trueSi + S_0 + I_0 + gamma_beta_ratio*log(trueSi/S_0)

plot(trueSi, It, type = "l", ylim = c(0, 32),  xlim = c(70, 270), lwd = 2, col = "blue", xlab = "S(t)", ylab = "I(t)", main = "The S-I Phase Plane")
points(SIR.out$S, SIR.out$I, pch = "*", cex = 1.5)
