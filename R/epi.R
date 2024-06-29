
#' An interface to eight compartmental epidemic models.
#'
#' ehpi is an application programming interface (for the Shiny application that
#' motivated its creation) or a convenient function [ehpi::epi()] for
#' interactive use in epidemic modelling. The arguments provided to the function
#' determine compartments used in the model that is run.
#'
#' The convention for pronouncing [ehpi::epi()] is "epi epi" as if you were
#' saying "epidemic" twice. If you do want to refer to the package alone, and
#' not the singular function it packages, you can say "eh pee eye", like "API".
#'
#' "eh pee eye" is a tongue-in-cheek Canadianism. You might understand it as,
#' "Eh, P.I.?", or just a Canadian slant on API. The latter was thought of
#' first.
#'
#' While ehpi provides "an API" for the
#' [Episim](https://github.com/ashokkrish/episim) Shiny application, it won't
#' enable the development of applications that provide any feature episim
#' doesn't already provide by wrapping this package. The function is packages
#' was extracted from that project to allow associating well-formatted
#' documentation, vignettes, and examples with the function because that
#' function is expected to be used outside of Episim, interactively, during
#' workshops run by Dr. Krishnamurthy.
#'
#' @param population The initial number of persons in the simulation; if
#'   `vitalDynamics = TRUE` and `muBirth` and `muDeath` are equal, then it is constant.
#' @param susceptible The initial number of people who are susceptible to the
#'   disease (they have no natural immunity to it).
#' @param exposed The initial number of people who have been exposed to the
#'   infection and may become infectious themselves.
#' @param infected The initial number of people who are infectious and may
#'   infect others.
#' @param recovered The initial number of people whom are no longer infectious
#'   and not a risk to others.
#' @param dead The initial number of fatalities (people killed by the disease).
#'
#' @param beta The rate of becoming infectious (if `sigma = 0`, the default), or
#'   the rate of susceptible people being exposed to the infectious.
#' @param gamma The rate of becoming recovered (no longer infectious, if `sigma
#'   = 0`, the default), or the rate of becoming infectious after exposure.
#' @param sigma The rate of becoming recovered (no longer infectious).
#' @param delta The rate of fatality.
#' @param xi The rate of loss of learned immunity.
#' @param muBirth The rate of birth.
#' @param muDeath The rate of death not due to the disease under consideration.
#'
#' @param vitalDynamics Whether vital dynamics is enabled (if `FALSE`, the
#'   default, implies muBirth and muDeath of zero).
#' @param trueMassAction Whether the force of infection is scaled by the
#'   population or not.
#'
#' @param timesteps The number of output rows in the simulation.
#' @param timeIncrement The number of increments of "time" that pass between
#'   each output row.
#'
#' @returns A dataframe with a column for each enabled compartment and a column
#'   for time, with `timesteps` rows. Each row contains the values of the
#'   differential equations for each point in time of the model (per function
#'   arguments).
#'
#' @export
#'
#' @examples
#' epi(population = 500,
#'     susceptible = 489,
#'     exposed = 10,
#'     infected = 1,
#'     recovered = 0,
#'     dead = 29,
#'     beta = 0.005,
#'     gamma = 0.0009,
#'     sigma = 0.000001,
#'     delta = 0.01,
#'     timesteps = 230)
#'
epi <-
  function(## Initial state variables
           population, susceptible, exposed = 0, infected, recovered, dead = 0,

           ## Parameters (not alphabetically sorted)
           beta, gamma, sigma = 0, delta = 0, xi = 0, muBirth = 0, muDeath = 0,

           ## Simulation options
           vitalDynamics = FALSE,
           trueMassAction = FALSE,

           ## Simulation variables
           timesteps = 25,
           timeIncrement = 1)
{
  variables <- c(population, susceptible, exposed, infected, recovered, dead)
  names(variables) <- c("N", "S", "E", "I", "R", "D")
  parameters <- c(beta, gamma, sigma, delta, xi,
                  muBirth * vitalDynamics,
                  muDeath * vitalDynamics)
  names(parameters) <- c("beta", "gamma", "sigma", "delta", "xi", "muBirth", "muDeath")

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
        ## TODO: I don't like writing this here, but it seems necessary.
        trueMassAction <- get("trueMassAction")

        ## Regardless of exposure
        betaSIN <- beta * ((S * I) / N^trueMassAction)
        dS <- c(muBirth * N, -betaSIN, -muDeath * S)
        dD <- delta * I # Always zero if delta == 0.

        ## Define the IR compartments with respect to the E compartment.
        if(sigma == 0) {
          ## Without exposure, there is no E compartment. γ has its meaning as
          ## in an SI-type model.
          compartments <- list(dI = c((betaSIN), -(gamma * I), -(muDeath * I)),
                               dR = c((gamma * I), -(muDeath * R)))
        } else {
          ## With exposure, there is is an E compartment. γ has its meaning as
          ## in an SEI-type model.
          compartments <- list(dE = c((betaSIN), -(gamma * E), -(muDeath * E)),
                               dI = c((gamma * E), -(sigma * I), -(muDeath * I)),
                               dR = c((sigma * I), -(muDeath * R)))
        }

        ## Enable loss of immunity if xi is non-zero.
        if (xi != 0) {
          whomAreSusceptibleAgain <- xi * R
          dS <- c(dS, whomAreSusceptibleAgain)
          compartments$dR <- c(compartments$dR, -whomAreSusceptibleAgain)
        }

        ## Enable fatality if delta is non-zero.
        if (delta != 0) compartments$dI <- c(compartments$dI, -dD)

        ## NOTE: dD is already equal to its sum, and is not part of the
        ## calculation of dN. dN is non-zero if muBirth != muD. Name dS again; it is
        ## helpful while debugging and printing the compartmentSums.
        compartmentSums <- lapply(append(list(dS = dS), compartments), sum)
        dN <- Reduce(`+`, compartmentSums)
        derivatives <- as.numeric(c(dN, compartmentSums, dD))
        names(derivatives) <- c("dN", names(compartmentSums), "dD")
        list(derivatives = derivatives)
      })
    }

  as.data.frame(
    deSolve::lsoda(variables,
                   ## TODO: better accommodate the semantics of date-time
                   ## information to ensure the simulation runs accurately for a
                   ## given amount of time and the desired number of "reporting
                   ## periods."
                   seq(1, length = timesteps, by = timeIncrement),
                   ## MAYBE FIXME: the differential equations don't use the time
                   ## variable: should they?
                   differentialEquations,
                   parameters,
                   trueMassAction = as.numeric(trueMassAction)))
}
