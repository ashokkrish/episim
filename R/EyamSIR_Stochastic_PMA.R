#------------------------------------------------------#
# Stochastic SIR epidemic model in a closed population #
#------------------------------------------------------#
#           by Ashok Krishnamurthy, PhD           	   #
#------------------------------------------------------#

# Load necessary libraries
library(ggplot2)
library(gridExtra)

# Set the parameters
chunksize <- 50
S0 <- 254
I0 <- 7
gamma <- 2.73
beta <- 0.0178
vectorlengths <- chunksize

# Create empty vectors
S <- rep(0, chunksize)
I <- rep(0, chunksize)
R <- rep(0, chunksize)
et <- rep(0, chunksize)

# Initialize our vectors
S[1] <- S0
I[1] <- I0
R[1] <- 0
et[1] <- 0
i <- 1

while (I[i] > 0) {
  # while there are still infectious individuals
  #print(sprintf('event number %d, t=%g, I = %d', i, et[i], I[i]))
  
  rSI <- beta * S[i] * I[i]                # total rate of S->I
  rIR <- gamma * I[i]                      # total rate of I->R
  totalRate <- rSI + rIR                   # total rate of all combined events
  et[i + 1] <- et[i] + rexp(1, totalRate)  # time of next event
  
  if (runif(1) < rSI / totalRate) {
    # a susceptible is being infected
    S[i + 1] <- S[i] - 1
    I[i + 1] <- I[i] + 1
    R[i + 1] <- R[i]
  } else {
    # an infectious is recovering/dying
    S[i + 1] <- S[i]
    I[i + 1] <- I[i] - 1
    R[i + 1] <- R[i] + 1
  }
  
  i <- i + 1
  
  # If our vectors have reached the length we've pre-allocated space for, then "grow" them.
  if (i == vectorlengths) {
    vectorlengths <- vectorlengths + chunksize
    
    length(et) <- vectorlengths
    length(S) <- vectorlengths
    length(I) <- vectorlengths
    length(R) <- vectorlengths
  }
}

# Strip off any extra elements remaining at the ends of the vectors, which we didn't use
length(et) <- i
length(S) <- i
length(I) <- i
length(R) <- i

# Create a data frame from the simulation results
df <- data.frame(Time = et, S = S, I = I, R = R)

# Plot S, I, and R versus time using ggplot2
time_plot <- ggplot(df, aes(x = Time)) +
  geom_line(aes(y = S, color = 'S(t)'), linetype = "solid", linewidth = 1.2) +
  geom_line(aes(y = I, color = 'I(t)'), linetype = "solid", linewidth = 1.2) +
  geom_line(aes(y = R, color = 'R(t)'), linetype = "solid", linewidth = 1.2) +
  scale_color_manual(name = "Compartment", 
                     values = c('S(t)' = 'red', 'I(t)' = 'green', 'R(t)' = 'blue'),
                     breaks = c('S(t)', 'I(t)', 'R(t)')) +
  labs(title = "Stochastic SIR Epidemic Model", x = "Time", y = "Compartment Size") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Create the SI phase plot using ggplot2
phase_plot <- ggplot(df, aes(x = S, y = I)) +
  geom_line(color = 'purple', linewidth = 0.75) +
  labs(title = "SI Phase Plot", x = "Susceptible (S)", y = "Infectious (I)") +
  theme_minimal()

# Arrange the two plots together with the legend at the bottom
grid.arrange(time_plot, phase_plot, ncol = 1)
