# Load necessary libraries
library(ggplot2)
library(dplyr)

# Function for SIRS model with vital dynamics
simulate_SIRS <- function(S0, I0, R0, beta, gamma, mu, delta, n_steps) {
  # Initialize vectors to store results
  S <- numeric(n_steps + 1)
  I <- numeric(n_steps + 1)
  R <- numeric(n_steps + 1)
  t <- 0:n_steps
  
  # Initial conditions
  S[1] <- S0
  I[1] <- I0
  R[1] <- R0
  
  # Total population
  N <- S0 + I0 + R0
  
  # Simulation loop
  for (step in 1:n_steps) {
    # Calculate the number of new infections, recoveries, births, deaths, and loss of immunity
    new_infections <- rbinom(1, S[step], 1 - exp(-beta * I[step] / N))
    new_recoveries <- rbinom(1, I[step], 1 - exp(-gamma))
    new_births <- rpois(1, mu * N)
    deaths_S <- rbinom(1, S[step], mu)
    deaths_I <- rbinom(1, I[step], mu)
    deaths_R <- rbinom(1, R[step], mu)
    loss_of_immunity <- rbinom(1, R[step], 1 - exp(-delta))
    
    # Update compartments
    S[step + 1] <- S[step] - new_infections + new_births - deaths_S + loss_of_immunity
    I[step + 1] <- I[step] + new_infections - new_recoveries - deaths_I
    R[step + 1] <- R[step] + new_recoveries - deaths_R - loss_of_immunity
    
    # Ensure no negative values
    S[step + 1] <- max(S[step + 1], 0)
    I[step + 1] <- max(I[step + 1], 0)
    R[step + 1] <- max(R[step + 1], 0)
    
    # Update total population
    N <- S[step + 1] + I[step + 1] + R[step + 1]
  }
  
  # Return results as a data frame
  data.frame(t, S, I, R)
}

# Parameters
S0 <- 990  # Initial number of susceptible individuals
I0 <- 10   # Initial number of infected individuals
R0 <- 0    # Initial number of recovered individuals
beta <- 0.3  # Infection rate
gamma <- 0.1  # Recovery rate
mu <- 0.01    # Birth and death rate
delta <- 0.05 # Rate of loss of immunity
n_steps <- 160  # Number of time steps
n_runs <- 50    # Number of stochastic runs

# Run the simulations
all_runs <- lapply(1:n_runs, function(x) simulate_SIRS(S0, I0, R0, beta, gamma, mu, delta, n_steps))

# Combine all runs into one data frame for plotting
all_results <- do.call(rbind, lapply(1:n_runs, function(i) {
  run <- all_runs[[i]]
  run$run <- i
  return(run)
}))

# Plot time series of S, I, R
time_plot <- ggplot(all_results, aes(x = t, group = run)) +
  geom_line(aes(y = S, color = "Susceptible"), linewidth = 1, alpha = 0.5) +
  geom_line(aes(y = I, color = "Infected"), linewidth = 1, alpha = 0.5) +
  geom_line(aes(y = R, color = "Recovered"), linewidth = 1, alpha = 0.5) +
  labs(title = "Stochastic SIRS Model with Vital Dynamics",
       x = "Time",
       y = "Population",
       color = "Compartment") +
  scale_color_manual(values = c("blue", "red", "green")) +
  theme_minimal()

# SI Phase plane plot
si_phase_plot <- ggplot(all_results, aes(x = S, y = I, group = run)) +
  geom_path(linewidth = 1, alpha = 0.5, color = "darkred") +
  labs(title = "SI Phase Plane Plot",
       x = "Susceptible",
       y = "Infected") +
  theme_minimal()

# Print the plots
print(time_plot)
print(si_phase_plot)
