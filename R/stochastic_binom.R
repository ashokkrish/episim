#-------------------------------------------------------------
# BINOMIAL STOCHASTIC MODEL
#-------------------------------------------------------------
binomial_SI <- function(
  iterations,
  # Variables
  timestep, population, susceptible, infected, recovered,
  # Parameters
  beta, gamma,
  ## Simulation options
  trueMassAction = FALSE) {
  all_runs <- list()
  for (i in 1:iterations) {
    run <- run_simulation(timestep,population,susceptible,infected,recovered,
                          beta,gamma,TRUE)
    run$iteration <- as.factor(i)
    all_runs[[i]] <- run
  }
  all_data <- do.call(rbind, all_runs)
  plot_binomial_SI(all_data)
}

# Function to simulate the stochastic SI models
run_simulation <- function(
    # Variables
  timestep, population, susceptible, infected, recovered,
  # Parameters
  beta, gamma,
  ## Simulation options
  trueMassAction = FALSE
  ){
  # Initialize
  S <- susceptible
  I <- infected
  R <- recovered
  N <- population
  
  results <- data.frame(time = 0, S=S, I=I, R=R)
  
  # Run the simulation
  for (t in 1:timestep) {
    # Transition rates
    lambda <- beta * I / (N^trueMassAction)
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
plot_binomial_SI <- function(all_data) {
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
}
test_binomial <- binomial_SI(iterations = 50,
                             timestep = 100,
                             population = 1000,
                             susceptible = 999,
                             infected = 1,
                             recovered = 0,
                             beta = 0.3,
                             gamma = 0.1,
                             TRUE)