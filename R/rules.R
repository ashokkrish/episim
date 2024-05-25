rules <- tribble(
  ~ model,
  ~ ruleList,
  "SIR",
  list(
    beta = c(sv_between(0, 1)),
    gamma = c(sv_between(0, 1))
  ),
  "SIRS",
  list(
    beta = c(sv_between(0, 1)),
    gamma = c(sv_between(0, 1)),
    xi = c(sv_between(0, 1))
  ),
  "SIRD",
  list(
    beta = c(sv_between(0, 1)),
    gamma = c(sv_between(0, 1)),
    delta = c(sv_between(0, 1))
  ),
  "SEIR",
  list(
    beta = c(sv_between(0, 1)),
    gamma = c(sv_between(0, 1)),
    sigma = c(sv_between(0, 1))
  ),
  "SEIRS",
  list(
    beta = c(sv_between(0, 1)),
    gamma = c(sv_between(0, 1)),
    sigma = c(sv_between(0, 1)),
    xi = c(sv_between(0, 1))
  ),
  "SEIRD",
  list(
    beta = c(sv_between(0, 1)),
    gamma = c(sv_between(0, 1)),
    sigma = c(sv_between(0, 1)),
    delta = c(sv_between(0, 1))
  ),

  # Global rules
  NA,
  list(
    ## Vital dynamics
    muBirth = c(sv_between(0, 0.1)),
    muDeath = c(sv_between(0, 0.1)),

    ## Compartments
    population = c(sv_integer(), sv_gt(0)),
    susceptible = c(sv_integer(), sv_gt(0)),
    exposed = c(sv_integer(), sv_gte(0)),
    infected = c(sv_integer(), sv_gte(0)), # FIXME: this might need GT
    recovered = c(sv_integer(), sv_gte(0)),
    dead = c(sv_integer(), sv_gte(0)),

    ## Simulation options
    ## NOTE: the number of replicates might need to be limited.
    ## replicates = c(sv_integer(), sv_between(0, 100, c(FALSE, TRUE))),
    replicates = c(sv_integer(), sv_gt(0)),
    timesteps = c(sv_gt(0)),

    ## Global rules for parameters
    beta = c(sv_gt(0)),
    gamma = c(sv_gt(0))
  )
)
