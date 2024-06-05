rules <- tribble(
  ~ model, # A regular expression
  ~ ruleList,
  "S.*S", # Waning learned immunity
  list(
    xi = c(sv_gt(0), sv_lte(1))
  ),
  "D", # Death
  list(
    delta = c(sv_between(0, 1)),
    dead = c(sv_integer(), sv_gte(0))
  ),
  "E", # Exposure
  list(
    sigma = c(sv_between(0, 1)),
    exposed = c(sv_integer(), sv_gte(0))
  ),

  # Global rules
  NA,
  list(
    ## Vital dynamics
    muBirth = c(sv_between(0, 1)),
    muDeath = c(sv_between(0, 1)),

    ## Global rules for compartments; only the compartments that are actually
    ## common to all models can be included here, otherwise all models will be
    ## invalidated if an inapplicable compartment has an invalid input (e.g.
    ## the exposed and dead compartments have invalid input while SIR is
    ## selected).
    population = c(sv_integer(), sv_gt(0)),
    susceptible = c(sv_integer(), sv_gt(0)),
    infected = c(sv_integer(), sv_gte(0)),
    ## The above example means you---the current editor---must move the
    ## recovered rule to each model featuring the recovery compartment if ever
    ## you include an SI model (having no recovered compartment), and remove it
    ## from the global rule. Make sense? Godspeed.
    recovered = c(sv_integer(), sv_gte(0)),

    ## Global rules for parameters
    beta = c(sv_gt(0), sv_between(0, 1)),
    gamma = c(sv_gt(0), sv_between(0, 1)),

    ## Simulation options
    replicates = c(sv_integer(), sv_gt(0)),
    timesteps = c(sv_gt(0))
    )
  )
