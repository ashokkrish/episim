## MAYBE FIXME: the differential equations don't use the time variable; this may
## be mathematically or scientifically inaccurate; assess this fact, then
## resolve this as needed.
equationsSusceptibleInfected <- function(time, variables, parameters, ...,
                                         trueMassAction = FALSE) {
  with(append(as.list(variables), as.list(parameters)), {
    trueMassAction <- get("trueMassAction")
    betaSIN <- beta * ((S * I) / N^trueMassAction)

    dS <- c((muB * N),   -(betaSIN),   -(muD * S))
    dI <- c((betaSIN),   -(gamma * I), -(muD * I))
    dR <- c(              (gamma * I), -(muD * R))

    ## Enable loss of immunity if xi is non-zero.
    if (xi != 0) {
      newlySusceptible <- xi * R
      dS <- append(dS, newlySusceptible)
      dR <- append(dR, -newlySusceptible)
    }

    ## Enable fatality if delta is non-zero.
    if (delta == 0 && D != 0) stop("D (dead) must be zero when delta is zero.")
    if (delta != 0) {
      dI <- append(dI, -(delta * I))
    }

    results <- map(list(dS, dI, dR), sum)
    dS <- results[[1]]
    dI <- results[[2]]
    dR <- results[[3]]
    dN <- sum(dS, dI, dR)

    dEqns <- c(dN, dS, dI, dR)
    if (delta != 0) {
      dEqns <- append(dEqns, dD <- (delta * I))
    }

    return(list(dEqns, trueMassAction))
  })
}

solveSusceptibleInfected <-
  function(## Variables
           population, susceptible, infected, recovered, dead = 0,

           ## Parameters
           beta, gamma, delta = 0, xi = 0,

           ## Simulation options
           muB = 0, muD = 0, vitalDynamics = FALSE,
           trueMassAction = FALSE,

           ## Simulation variables
           numIterations = 100,
           increment = 1)
{
  variables <- c(population, susceptible, infected, recovered, dead)
  names(variables) <- c("N", "S", "I", "R", "D")
  parameters <- c(beta, gamma, delta, xi,
                  muB * vitalDynamics,
                  muD * vitalDynamics)
  names(parameters) <- c("beta", "gamma", "delta", "xi", "muB", "muD")

  ## NOTE: the variable D must not be passed to lsoda if delta is zero.
  if (delta == 0) variables <- variables[-variables["D"]]
  lsoda(variables,
        seq(0, length = numIterations, by = increment),
        equationsSusceptibleInfected,
        parameters,
        trueMassAction = as.numeric(trueMassAction)) |>
    as.data.frame()
}

## alias model-specific symbols to the unified solver functions for each model
## type (SE-type and SI-type).
solveSIR <- solveSIRS <- solveSIRD <- solveSusceptibleInfected
