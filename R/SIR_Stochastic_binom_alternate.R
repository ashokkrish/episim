# Load necessary libraries
library(ggplot2)
library(deSolve)
library(reshape2)

# Function to simulate the stochastic SIR model
stochastic_SIR <- function(S0, I0, R0, beta, gamma, N, T) {
  # Initialize
  S <- S0
  I <- I0
  R <- R0
  results <- data.frame(time = 0, S = S, I = I, R = R)
  
  # Run the simulation
  for (t in 1:T) {
    # Transition rates
    lambda <- beta * I / N
    mu <- gamma
    
    # Calculate the number of transitions
    dS <- rbinom(1, S, 1 - exp(-lambda))
    dR <- rbinom(1, I, 1 - exp(-mu))
    dI <- dS - dR
    
    # Update compartments
    S <- S - dS
    I <- I + dI
    R <- R + dR
    
    # Store results
    results <- rbind(results, data.frame(time = t, S = S, I = I, R = R))
  }
  return(results)
}

# Parameters
N <- 1000
S0 <- 999
I0 <- 1
R0 <- 0
beta <- 0.3
gamma <- 0.1
T <- 100
iterations <- 50

# Run the simulation for multiple iterations
all_runs <- list()
for (i in 1:iterations) {
  print(i)
  run <- stochastic_SIR(S0, I0, R0, beta, gamma, N, T)
  run$iteration <- as.factor(i)
  all_runs[[i]] <- run
}
all_data <- do.call(rbind, all_runs)

# Plot the results for S, I, R compartments
compartments_plot <- ggplot(all_data, aes(x = time)) +
  geom_line(aes(y = S, color = 'S', group = iteration), alpha = 0.3, size = 0.8) +
  geom_line(aes(y = I, color = 'I', group = iteration), alpha = 0.3, size = 0.8) +
  geom_line(aes(y = R, color = 'R', group = iteration), alpha = 0.3, size = 0.8) +
  labs(title = 'Stochastic SIR Model - Compartments over Time',
       x = 'Time',
       y = 'Number of Individuals') +
  scale_color_manual(values = c('S' = '#4B0082', 'I' = '#008000', 'R' = '#FF8C00'),
                     name = 'Compartments') +
  theme_minimal()

# Plot the SI phase plot
phase_plot <- ggplot(all_data, aes(x = S, y = I)) +
  geom_path(aes(group = iteration, color = iteration), alpha = 0.3) +
  labs(title = 'Stochastic SIR Model - SI Phase Plot',
       x = 'Susceptible (S)',
       y = 'Infected (I)') +
  theme_minimal() +
  theme(legend.position = 'none')

# Print the plots
print(compartments_plot)
print(phase_plot)
