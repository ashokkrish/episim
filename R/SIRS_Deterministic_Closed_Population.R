#----------------------------------------------------------#
# Deterministic SIRS epidemic model in a closed population #
#----------------------------------------------------------#

#install.packages("deSolve", dependencies = T)
library(deSolve)
library(ggplot2)

SIRSmodel <- function(t, x, parms)
{
  S <- x[1]
  I <- x[2]
  R <- x[3]
  N <- x[4]
  
  with(as.list(parms), {
    dS <- -beta*S*I/N + gamma*R
    dI <- beta*S*I/N - alpha*I
    dR <- alpha*I - gamma*R
    
    dN <- dS + dI + dR
    
    list(c(dS, dI, dR, dN))
  })
}

# Set time points, parameters and initial conditions
times <- seq(0, 100, by = 1)
parms <- c(beta = 0.3, alpha = 0.1, gamma = 0.05)
xstart <- c(S = 990, I = 10, R = 0, N = 1000)

# Solve the differential equations using the ode solver
SIRS.out <- as.data.frame(ode(xstart, times, SIRSmodel, parms))

# SIRS.out

# dim(SIRS.out)
# head(SIRS.out)
# tail(SIRS.out)

# Plot using ggplot2
ggplot(SIRS.out, aes(x = time)) +
  geom_line(aes(y = S, color = "Susceptible"), linewidth = 1) +
  geom_line(aes(y = I, color = "Infectious"), linewidth = 1) +
  geom_line(aes(y = R, color = "Recovered"), linewidth = 1) +
  labs(x = "Time", y = "Number of individuals", title = "SIRS Compartmental Model") +
  scale_color_manual(name = "Compartment", values = c("Susceptible" = "blue", "Infectious" = "red", "Recovered" = "green"),
                     breaks = c("Susceptible", "Infectious", "Recovered")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))