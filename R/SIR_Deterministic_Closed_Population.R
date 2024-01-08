#---------------------------------------------------------#
# Deterministic SIR epidemic model in a closed population #
#---------------------------------------------------------#

#install.packages("deSolve", dependencies = T)
library(deSolve)
library(ggplot2)

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

# Set time points, parameters and initial conditions
times  <- seq(0, 100, by = 1)
parms  <- c(beta = 0.001, gamma = 0.1)
xstart <- c(S = 499, I = 1, R = 0, N = 500)

# Solve the differential equations using the ode solver
SIR.out <- as.data.frame(lsoda(xstart, times, SIRmodel, parms))

# SIR.out

# dim(SIR.out)
# head(SIR.out)
# tail(SIR.out)

# Function to calculate R0
calculate_R0 <- function(beta, gamma) {
  R0 <- beta / gamma
  return(R0)
}

# Parameters from the SIR model
beta <- 0.001
gamma <- 0.1

# Calculate and print R0
R0 <- calculate_R0(beta, gamma)
cat("Basic Reproduction Number (R0):", R0, "\n")

# Plot
ggplot(SIR.out, aes(x = time)) +
  geom_line(aes(y = S, color = "Susceptible"), linewidth = 1) +
  geom_line(aes(y = I, color = "Infectious"), linewidth = 1) +
  geom_line(aes(y = R, color = "Recovered"), linewidth = 1) +
  labs(x = "Time", y = "Number of individuals", title = "SIR Compartmental Model") +
  scale_color_manual(name = "Compartment", values = c("Susceptible" = "blue", "Infectious" = "red", "Recovered" = "green"),
                     breaks = c("Susceptible", "Infectious", "Recovered")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# SI Phase Plane
ggplot(SIR.out, aes(x = S, y = I)) +
  geom_line(linetype = "solid", color = "blue", linewidth = 1) +
  labs(x = "Susceptible", y = "Infectious", title = "SI Phase Plane Plot") +
  theme_minimal() +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))