## FIXME: the differential equations don't use the time variable; this may be
## mathematically or scientifically inaccurate; assess this fact, then resolve
## this as needed.
equationsSusceptibleInfected <- function(time, variables, parameters, trueMassAction = FALSE, ...) {
  with(append(as.list(variables),
              as.list(parameters)), {
    massAction = as.numeric(trueMassAction)
    betaSIN <- beta * ((S * I) / N^massAction)

    dS <- sum((muB * N),   -(muD * S),   (xi * R), -(betaSIN))
    dI <- sum(betaSIN,    -(gamma * I), -(muD * I))
    dR <- sum((gamma * I), -(muD * R))
    dN <- dS + dI + dR

    dEqns <- c(dS, dI, dR, dN)
    if (delta != 0) assign("dEqns", append(dEqns, delta * I, length(dEqns) - 1))

    return(list(dEqns, massAction))
  })
}

solveSusceptibleInfected <-
  function(## Variables
           population, susceptible, infected, recovered, dead = 0,

           ## Parameters (not alphabetically sorted)
           beta, gamma, delta = 0, xi = 0,

           ## Simulation options
           muB = 0, muD = 0, vitalStatistics = FALSE,
           trueMassAction = FALSE, # Formerly, the variable was named "q".

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
      trueMassAction = as.numeric(trueMassAction)))
}

## alias model-specific symbols to the unified solver functions for each model
## type (SE-type and SI-type).
solveSIR <- solveSIRS <- solveSIRD <- solveSusceptibleInfected
