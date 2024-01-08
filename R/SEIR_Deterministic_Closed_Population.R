#----------------------------------------------------------#
# Deterministic SEIR epidemic model in a closed population #
#----------------------------------------------------------#

#install.packages("deSolve", dependencies = T)
#install.packages("rgl", dependencies = T)
library(deSolve)
library(ggplot2)
library(rgl)

SEIRmodel <- function(t, x, parms)
{
  S <- x[1]
  E <- x[2]
  I <- x[3]
  R <- x[4]
  N <- x[5]
  
  with(as.list(parms), {
    dS <- -beta*S*I/N
    dE <-  beta*S*I/N - gamma*E
    dI <-  gamma*E - sigma*I
    dR <-  sigma*I
    
    dN <- dS + dE + dI + dR
    list(c(dS, dE, dI, dR, dN)) 
  }) 
}

# Set time points, parameters and initial conditions
times <- seq(0, 150, by = 1) 
parms <- c(beta = 0.35, gamma = 1/7, sigma = 1/21)
xstart <- c(S = 499, E = 0, I = 1, R = 0, N = 500)

# Solve the differential equations using the ode solver
SEIR.out <- as.data.frame(lsoda(xstart, times, SEIRmodel, parms))

# SEIR.out

# dim(SEIR.out)
# dim(SEIR.out)
# head(SEIR.out)

# Function to calculate R0 for SEIR model
calculate_R0_SEIR <- function(beta, sigma, gamma) {
  R0 <- beta / (sigma + gamma)
  return(R0)
}

# Parameters from the SEIR model
beta <- 0.35
sigma <- 1/21
gamma <- 1/7

# Calculate and print R0 for SEIR model
R0_SEIR <- calculate_R0_SEIR(beta, sigma, gamma)
cat("Basic Reproduction Number (R0) for SEIR model:", R0_SEIR, "\n")

# Plot using ggplot2
ggplot(SEIR.out, aes(x = time)) +
  geom_line(aes(y = S, color = "Susceptible"), linewidth = 1) +
  geom_line(aes(y = E, color = "Exposed"), linewidth = 1) +
  geom_line(aes(y = I, color = "Infectious"), linewidth = 1) +
  geom_line(aes(y = R, color = "Recovered"), linewidth = 1) +
  labs(x = "Time", y = "Number of individuals", title = "SEIR Compartmental Model") +
  scale_color_manual(name = "Compartment", values = c("Susceptible" = "blue", "Exposed" = "orange", "Infectious" = "red", "Recovered" = "green"),
                     breaks = c("Susceptible", "Exposed", "Infectious", "Recovered")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# # Plot the tetrahedron
# open3d()
# sides <- matrix(c(1, 0, 0, 0,
#                   0, 1, 0, 0,
#                   0, 0, 1, 0,
#                   1, 1, 1, 1), ncol = 4, byrow = TRUE)
# plot3d(sides, type = "s", col = c("blue", "orange", "red", "green"), 
#        xlab = "Susceptible", ylab = "Exposed", zlab = "Infectious")
# 
# # Add SEIR points
# points3d(SEIR.out$S, SEIR.out$E, SEIR.out$I, col = "black", size = 3)
# 
# # Add lines connecting the points
# segments3d(c(0, 0, 0, 0), c(0, 1, 0, 0), c(0, 0, 1, 0), col = "black")
# 
# # Add labels
# text3d(c(0, 0, 0, 0, 1), c(0, 1, 0, 0, 0), c(0, 0, 1, 0, 0), 
#        text = c("S", "E", "I", "R", "N"), adj = c(-0.2, -0.2))