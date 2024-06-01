## Package namesake: hexagon logo should have Canadian symbology, perhaps with
## "exposure" meaning exposure to the elements and the risk of frostbite.
## Therefore, a potential logo could feature an Inuit person wearing ᐃᓪᒑᒃ (or
## ᐃᒡᒑᒃ), and holding a holographic graph like the plot of compartment values
## over time, or transparent glass etched with the graph.
exposuRe <-
  function(## Variables
           population, susceptible, exposed = 0, infected, recovered, dead = 0,

           ## Parameters (not alphabetically sorted)
           beta, gamma, sigma = 0, delta = 0, xi = 0,

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

  ## Prevent lsoda from being called with conflicting argument values.
  if(delta == 0) stopifnot(dead == 0)
  if(sigma == 0) {
    stopifnot(exposed == 0)
    stopifnot(infected > 0)
    ## Ensure lsoda is called with the correct number of initial state variables
    ## to match the number of differential equations it will return based on the
    ## variables and parameters given to it. I.E., don't call lsoda with the
    ## exposed compartment if the model won't use the compartment.
    variables <- variables[!(names(variables) %in% c("E"))]
  }

  differentialEquations <-
    function(time, variables, parameters, ...,
             trueMassAction = FALSE) {
      ## NOTE: is coersion to a list really necessary for each? Can't named
      ## vectors be appended, or even concatenated with c(), then converted to a
      ## list for use with with()?
      with(append(as.list(variables),
                  as.list(parameters)),
      {
        ## FIXME: I don't like writing this here, but it seems necessary.
        trueMassAction <- get("trueMassAction")

        ## Regardless of exposure
        betaSIN <- beta * ((S * I) / N^trueMassAction)
        dS <- c(muB * N, -betaSIN, -muD * S)
        dD <- delta * I # Always zero if delta == 0.

        ## Define the IR compartments with respect to the E compartment.
        if(sigma == 0) {
          ## Without exposure, there is no E compartment. γ has its meaning as
          ## in an SI-type model.
          compartments <- list(dI = c((betaSIN), -(gamma * I), -(muD * I)),
                               dR = c((gamma * I), -(muD * R)))
        } else {
          ## With exposure, there is is an E compartment. γ has its meaning as
          ## in an SEI-type model.
          compartments <- list(dE = c((betaSIN), -(gamma * E), -(muD * E)),
                               dI = c((gamma * E), -(sigma * I), -(muD * I)),
                               dR = c((sigma * I), -(muD * R)))
        }

        ## Enable loss of immunity if xi is non-zero. FIXME: a second S compartment is being added.
        if (xi != 0) {
          whomAreSusceptibleAgain <- xi * R
          dS <- c(dS, whomAreSusceptibleAgain)
          compartments$dR <- c(compartments$dR, -whomAreSusceptibleAgain)
        }

        ## Enable fatality if delta is non-zero.
        if (delta != 0) compartments$dI <- c(compartments$dI, -dD)

        ## NOTE: dD is already equal to its sum, and is not part of the
        ## calculation of dN. dN is non-zero if muB != muD. Name dS again; it is
        ## helpful while debugging and printing the compartmentSums.
        compartmentSums <- lapply(append(list(dS = dS), compartments), sum)
        dN <- Reduce(`+`, compartmentSums)
        derivatives <- as.numeric(c(dN, compartmentSums, dD))
        names(derivatives) <- c("dN", names(compartmentSums), "dD")
        list(derivatives = derivatives)
      })
    }

  lsoda(variables,
        ## TODO FIXME: these times need to be updated after reading on lsoda.
        seq(1, length = timesteps, by = increment),
        ## MAYBE FIXME: the differential equations don't use the time variable.
        differentialEquations,
        parameters,
        trueMassAction = as.numeric(trueMassAction)) |>
    as.data.frame()
}
