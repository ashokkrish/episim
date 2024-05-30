## MAYBE FIXME: the differential equations don't use the time variable; this may be
## mathematically or scientifically inaccurate; assess this fact, then resolve
## this as needed.
equationsSusceptibleExposed <- function(time, variables, parameters, ...,
                                         trueMassAction = FALSE) {
  with(append(as.list(variables), as.list(parameters)), {
    trueMassAction <- get("trueMassAction")
    betaSIN <- beta * ((S * I) / N^trueMassAction)

    dS <- c((muB * N),   -(betaSIN),   -(muD * S))
    dE <- c((betaSIN),   -(gamma * E), -(muD * E))
    dI <- c((gamma * E), -(sigma * I), -(muD * I))
    dR <- c(              (sigma * I), -(muD * R))
    dD <- (delta * I) # Always zero if delta == 0.

    ## Enable loss of immunity if xi is non-zero.
    if (xi != 0) {
      newlySusceptible <- xi * R
      dS <- append(dS, newlySusceptible)
      dR <- append(dR, -newlySusceptible)
    }

    ## Enable fatality if delta is non-zero.
    if (delta != 0) dI <- append(dI, -dD)

    ## NOTE: dD is already equal to its sum.
    results <- map(list(dS, dE, dI, dR), sum)
    dS <- results[[1]]
    dE <- results[[2]]
    dI <- results[[3]]
    dR <- results[[4]]
    dN <- sum(dS, dE, dI, dR)

    return(list(c(dN, dS, dE, dI, dR, dD), trueMassAction))
  })
}

solveSusceptibleExposed <-
  function(## Variables
           population, susceptible, exposed, infected, recovered, dead = 0,

           ## Parameters (not alphabetically sorted)
           beta, gamma, sigma, delta = 0, xi = 0,

           ## Simulation options
           muB = 0, muD = 0, vitalDynamics = FALSE,
           trueMassAction = FALSE,

           ## Simulation variables
           timesteps = 25,
           increment = 1)
{
  variables <- c(population, susceptible, exposed, infected, recovered, dead)
  names(variables) <- c("N", "S", "E", "I", "R", "D")
  parameters <- c(beta, gamma, sigma, delta, xi,
                  muB * vitalDynamics,
                  muD * vitalDynamics)
  names(parameters) <- c("beta", "gamma", "sigma", "delta", "xi", "muB", "muD")

  if(delta == 0) stopifnot(dead == 0)

  lsoda(variables,
        ## TODO: FIXME: these need to be updated with reading on lsoda.
        seq(1, length = timesteps, by = increment),
        equationsSusceptibleExposed,
        parameters,
        trueMassAction = as.numeric(trueMassAction)) |>
    as.data.frame()
}

## alias model-specific symbols to the unified solver functions for each model
## type (SE-type and SI-type).
solveSEIR <- solveSEIRS <- solveSEIRD <- solveSusceptibleExposed
