## FIXME: the differential equations don't use the time variable; this may be
## mathematically or scientifically inaccurate; assess this fact, then resolve
## this as needed.
equationsSusceptibleInfected <- function(time, variables, parameters, q = 0, ...) {
  with(append(as.list(variables),
              as.list(parameters)), {
    betaSINq <- beta * ((S * I) / N^q)

    dS <- sum((muB * N),   -(muD * S),   (xi * R), -(betaSINq))
    dI <- sum(betaSINq,    -(gamma * I), -(muD * I))
    dR <- sum((gamma * I), -(muD * R))
    dN <- dS + dI + dR

    dEqns <- c(dS, dI, dR, dN)
    if (delta != 0) assign(dEqns, append(dEqns, delta * I, length(dEqns) - 1))

    return(list(dEqns, q))
  })
}

solveSusceptibleInfected <-
  function(## Variables
           population, susceptible, exposed, infected, recovered, dead,

           ## Parameters (not alphabetically sorted)
           beta, gamma, delta = 0, xi = 0,

           ## Simulation options
           muB = 0, muD = 0, vitalStatistics = FALSE,
           massActionSelect = FALSE, # Formerly, the variable was named "q".

           ## Simulation variables
           numIterations = 100,
           increment = 1)
{
  ## Assign the variables and parameters to vectors then name them, making the
  ## function readable.
  variables <- c(population, susceptible, infected, recovered, dead)
  names(variables) <- c("N", "S", "I", "R", "D")

  parameters <- c(beta, gamma, delta, xi,
                  muB * vitalStatistics,
                  muD * vitalStatistics)
  names(parameters) <- c("beta", "gamma", "delta", "xi", "muB", "muD")

  as.data.frame(
    deSolve::lsoda(
      variables,
      seq(0, length = numIterations, by = increment),
      equationsSusceptibleInfected,
      parameters,
      q))
}
