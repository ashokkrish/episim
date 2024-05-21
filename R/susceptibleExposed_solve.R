## FIXME: the differential equations don't use the time variable; this may be
## mathematically or scientifically inaccurate; assess this fact, then resolve
## this as needed.
equationsSusceptibleExposed <- function(time, variables, parameters, q = 0, ...) {
  with(append(as.list(variables), as.list(parameters)), {
    betaSINq <- beta * ((S * I) / N^q)

    dS <- sum((muB * N),   -(muD * S),   (xi * R), -(betaSINq))
    dE <- sum(betaSINq,    -(gamma * E), -(muD * E))
    dI <- sum((gamma * E), -(sigma * I), -(muD * I))
    dR <- sum((sigma * I), -(xi * R),    -(muD * R))
    dN <- dS + dE + dI + dR

    dEqns <- c(dS, dE, dI, dR, dN)
    if (delta != 0) assign(dEqns, append(dEqns, delta * I, length(dEqns) - 1))

    return(list(dEqns, q))
  })
}

solveSusceptibleExposed <-
  function(## Variables
           population, susceptible, exposed, infected, recovered, dead,

           ## Parameters (not alphabetically sorted)
           beta, gamma, sigma, delta = 0, xi = 0,

           ## Simulation options
           muB = 0, muD = 0, vitalStatistics = FALSE,
           massActionSelect = FALSE, # Formerly, the variable was named "q".

           ## Simulation variables
           numIterations = 100,
           increment = 1)
{
  ## Assign the variables and parameters to vectors then name them, making the
  ## function readable.
  variables <- c(population, susceptible, exposed, infected, recovered, dead)
  names(variables) <- c("N", "S", "E", "I", "R", "D")

  parameters <- c(beta, gamma, sigma, delta, xi,
                  muB * vitalStatistics,
                  muD * vitalStatistics)
  names(parameters) <- c("beta", "gamma", "sigma", "delta", "xi", "muB", "muD")

  as.data.frame(
    deSolve::lsoda(
      variables,
      seq(0, length = numIterations, by = increment),
      equationsSusceptibleExposed,
      parameters,
      q))

}
