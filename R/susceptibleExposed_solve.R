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
    dR <- c((sigma * I), -(muD * R))

    ## Enable loss of immunity if xi is non-zero.
    if (xi != 0) {
      newlySusceptible <- xi * R
      dS <- append(dS, newlySusceptible)
      dR <- append(dR, -newlySusceptible)
    }

    ## Enable fatality if delta is non-zero.
    if (delta == 0 && D != 0) stop("D (dead) must be zero when delta is zero.")
    if (delta != 0) {
      dD <- delta * I
      dI <- append(dI, -dD)
    }

    results <- map(list(dS, dE, dI, dR), sum)
    dS <- results[[1]]
    dE <- results[[2]]
    dI <- results[[3]]
    dR <- results[[4]]
    dN <- sum(dS, dE, dI, dR)

    dEqns <- c(dN, dS, dE, dI, dR)
    if (delta != 0) {
      dEqns <- append(dEqns, dD)
    }

    return(list(dEqns, trueMassAction))
  })
}

solveSusceptibleExposed <-
  function(## Variables
           population, susceptible, exposed, infected, recovered, dead = 0,

           ## Parameters (not alphabetically sorted)
           beta, gamma, sigma, delta = 0, xi = 0,

           ## Simulation options
           muB = 0, muD = 0, vitalDynamics = FALSE,
           trueMassAction = FALSE, # Formerly, the variable was named "q".

           ## Simulation variables
           numIterations = 100,
           increment = 1)
{
  variables <- c(population, susceptible, exposed, infected, recovered, dead)
  names(variables) <- c("N", "S", "E", "I", "R", "D")
  parameters <- c(beta, gamma, sigma, delta, xi,
                  muB * vitalDynamics,
                  muD * vitalDynamics)
  names(parameters) <- c("beta", "gamma", "sigma", "delta", "xi", "muB", "muD")

  ## NOTE: the variable D must not be passed to lsoda if delta is zero.
  if (delta == 0) variables <- variables[-variables["D"]]
  lsoda(variables,
        seq(0, length = numIterations, by = increment),
        equationsSusceptibleExposed,
        parameters,
        trueMassAction = as.numeric(trueMassAction)) |>
    as.data.frame()
}

## alias model-specific symbols to the unified solver functions for each model
## type (SE-type and SI-type).
solveSEIR <- solveSEIRS <- solveSEIRD <- solveSusceptibleExposed
