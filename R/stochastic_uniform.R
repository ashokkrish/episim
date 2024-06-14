#-------------------------------------------------------------
# UNIFORM STOCHASTIC MODEL
#-------------------------------------------------------------
library(ggplot2)
uniform_SI <- function(
    # Variables
  num_replications, population, susceptible, infected, recovered,
  # Parameters
  beta, gamma,
  ## Simulation options
  trueMassAction = FALSE
){
  vector_length = num_replications
  
  initial_values <- c(population,susceptible, infected, recovered,0)
  
  vector_names <- c("N","S", "I", "R","et") #TODO: Need to verify the use of et with Ashok
  
  # Initialize vectors using assign
  lapply(seq_along(vector_names), function(i) {
    assign(vector_names[i], c(initial_values[i], rep(0, num_replications)), envir = .GlobalEnv)
  })
  
  index = 1;
  
  while(I[index] > 0){
    # while there are still infectious individuals
    #print(sprintf('event number %d, t=%g, I = %d', index, et[index], I[index]))
    
    #TODO: need to consider vital dynamic
    rSI = beta* ((S[index] * I[index])/N[index]^trueMassAction)  # total rate of S->I
    rIR = gamma* I[index] # total rate of I->R
    totalRate = rSI + rIR # total rate of all combined events
    #print(c(rSI, rIR,totalRate))
    et[index + 1] = et[index] + rexp(1,totalRate) # time of next event
    
    if (runif(1) < rSI/totalRate){
      S[index + 1] = S[index] -1
      I[index + 1] = I[index] + 1
      R[index + 1] = R[index]
    } else{
      S[index + 1] = S[index]
      I[index + 1] = I[index] - 1
      R[index + 1] = R[index] + 1
    }
    index = index + 1
    if (index == vector_length){
      vector_length = vector_length + num_replications
      
      lapply(vector_names, function(name) {
        assign(name, `length<-`(get(name, envir = .GlobalEnv), vector_length), envir = .GlobalEnv)
      })
    }
  }
  lapply(vector_names, function(name) {
    assign(name, `length<-`(get(name, envir = .GlobalEnv), index), envir = .GlobalEnv)
  })
  
  # Combine data into a data frame
  results <- data.frame(Time = et[1:index], S = S[1:index], I = I[1:index], R = R[1:index])
  plot_uniform_SI(results)
}

#plot function
plot_uniform_SI <- function(all_data) {
  print(ggplot(all_data, aes(x = Time)) +
          geom_line(aes(y = S, color = "S(t)"), linetype = "solid", linewidth = 1) +
          geom_line(aes(y = I, color = "I(t)"), linetype = "solid", linewidth = 1) +
          geom_line(aes(y = R, color = "R(t)"), linetype = "solid", linewidth = 1) +
          labs(x = "Time", y = "Number of individuals") +
          ggtitle("Stochastic SIR Epidemic Model") +
          scale_color_manual(name = "Compartment",
                             values = c("S(t)" = "blue", "I(t)" = "red", "R(t)" = "green"),
                             breaks = c("S(t)", "I(t)", "R(t)")) +
          theme_minimal() +
          theme(panel.grid = element_blank(), legend.position = "bottom",
                plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")) +
          geom_vline(xintercept = 0, linetype = "solid") +
          geom_hline(yintercept = 0, linetype = "solid")
  )
}

# Call the function and assign the returned list to a variable
test <- uniform_SI(
  num_replications = 50,
  population = 0,
  susceptible = 254,
  infected = 7,
  recovered = 0,
  beta = 0.0178,
  gamma = 2.73,
  FALSE
)