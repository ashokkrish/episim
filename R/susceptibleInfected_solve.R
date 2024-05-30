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
    dD <- (delta * I) # Always zero if delta == 0.

    ## Enable loss of immunity if xi is non-zero.
    if (xi != 0) {
      newlySusceptible <- xi * R
      dS <- append(dS, newlySusceptible)
      dR <- append(dR, -newlySusceptible)
    }

    ## Enable fatality if delta is non-zero.
    if (delta != 0) dI <- append(dI, -dD)

    ## NOTE NEXT TODO: the following code using purrr::map can be replaced with
    ## this functional code only using functions included in the base package.
    ## This algorithm also is the stepping stone to unifying
    ## equationsSusceptibleInfected and equationsSusceptibleExposed, and
    ## unifying solveSusceptibleInfected and solveSusceptibleExposed to dispatch
    ## arguments to these functions. The next step would be to make a package
    ## and document the functions for users, and include examples.
    ##
    ## Reduce(`+`,
    ##        lapply(X = list(list(1, 2, 3, 4),
    ##                        list(5, 6, 7, 8),
    ##                        list(9)),
    ##               FUN = Reduce,
    ##               f = `+`))

    ## NOTE: dD is already equal to its sum.
    results <- map(list(dS, dI, dR), sum)
    dS <- results[[1]]
    dI <- results[[2]]
    dR <- results[[3]]
    dN <- sum(dS, dI, dR) # Non-zero if muB != muD.

    return(list(c(dN, dS, dI, dR, dD), trueMassAction))
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
           timesteps = 25,
           increment = 1)
{
  variables <- c(population, susceptible, infected, recovered, dead)
  names(variables) <- c("N", "S", "I", "R", "D")
  parameters <- c(beta, gamma, delta, xi,
                  muB * vitalDynamics,
                  muD * vitalDynamics)
  names(parameters) <- c("beta", "gamma", "delta", "xi", "muB", "muD")

  if(delta == 0) stopifnot(dead == 0)

  lsoda(variables,
        ## TODO: FIXME: these need to be updated with reading on lsoda.
        seq(1, length = timesteps, by = increment),
        equationsSusceptibleInfected,
        parameters,
        trueMassAction = as.numeric(trueMassAction)) |>
    as.data.frame()
}

## alias model-specific symbols to the unified solver functions for each model
## type (SE-type and SI-type).
solveSIR <- solveSIRS <- solveSIRD <- solveSusceptibleInfected
