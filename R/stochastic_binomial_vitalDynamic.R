library(ggplot2)
binomialSI_VD <- function(
    replicates,
    # Variables
    timesteps, population, susceptible, infected, recovered,
    # Parameters
    beta, gamma, xi,
    mu, #Is mu_B equal to mu_D?
    ## Simulation options
    trueMassAction = FALSE){
  all_runs <- lapply(1:replicates, function(x) simulate_SI(timesteps, population, susceptible, infected, recovered,
                                                           beta, gamma, xi,mu,trueMassAction))
  all_results <- do.call(rbind, lapply(1:replicates, function(i) {
    run <- all_runs[[i]]
    run$run <- i
    return(run)
  }))
  
  plot_results(all_results)
}

simulate_SI <- function(
    # Variables
  timesteps, population, susceptible, infected, recovered,
  # Parameters
  beta, gamma, xi,
  mu, #Is mu_B equal to mu_D?
  ## Simulation options
  trueMassAction = FALSE){
  # Initialize vectors to store results
  S <- numeric(timesteps + 1)
  I <- numeric(timesteps + 1)
  R <- numeric(timesteps + 1)
  N <- population
  t <- 0:timesteps
  
  S[1] <- susceptible
  I[1] <- infected
  R[1] <- recovered
  
  for(step in 1:timesteps){
    # Calculate the number of new infections, recoveries, births, and deaths
    new_infections <- rbinom(1, S[step], 1 - exp(-beta * I[step] / N^as.numeric(trueMassAction)))
    new_recoveries <- rbinom(1, I[step], 1 - exp(-gamma))
    new_births <- rpois(1, mu * N)
    deaths_S <- rbinom(1, S[step], mu)
    deaths_I <- rbinom(1, I[step], mu)
    deaths_R <- rbinom(1, R[step], mu)
    
    # Update compartments
    S[step + 1] <- S[step] - new_infections + new_births - deaths_S
    I[step + 1] <- I[step] + new_infections - new_recoveries - deaths_I
    R[step + 1] <- R[step] + new_recoveries - deaths_R
    
    if(xi != 0){
      loss_of_immunity <- rbinom(1, R[step], 1 - exp(-xi))
      S[step + 1] <- S[step + 1] + loss_of_immunity
      R[step + 1] <- R[step + 1] - loss_of_immunity
    }
    
    # Ensure no negative values
    S[step + 1] <- max(S[step + 1], 0)
    I[step + 1] <- max(I[step + 1], 0)
    R[step + 1] <- max(R[step + 1], 0)
    
    # Update total population
    N <- S[step + 1] + I[step + 1] + R[step + 1]
  }
  data.frame(t = t, S = S, I = I, R = R)
}

plot_results <- function(all_results){
  # Plot time series of S, I, R
  time_plot <- ggplot(all_results, aes(x = t, group = run)) +
    geom_line(aes(y = S, color = "Susceptible"), linewidth = 1, alpha = 0.5) +
    geom_line(aes(y = I, color = "Infected"), linewidth = 1, alpha = 0.5) +
    geom_line(aes(y = R, color = "Recovered"), linewidth = 1, alpha = 0.5) +
    labs(title = "Stochastic SI Model with Vital Dynamics",
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
}

test <- binomialSI_VD(replicates = 50, timesteps = 160, population = 1000, susceptible = 990,
                      infected = 10, recovered = 0, beta = 0.3, gamma = 0.1, xi = 0, mu = 0.01,
                      TRUE)