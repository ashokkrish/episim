birthRateMessage <- gsub("\n[\ \t]+", " ", r"(Must be between {left} and
  {right}; these limits are based on the global average birth rate per woman in
  2021. The limits are based on such data as mentioned in the article
  "Fertility Rate" published by ourworldindata.org. The current average
  global birth rate of 2.4 births per woman is halved to a value of 1.2, given
  muB is multiplied by N to produce the number of new individuals added to the
  susceptible compartment. The upper limit of 5 is based on the pre-industrial
  average birth rate of seven per woman. Allowing an outlier limit of 10 births
  per woman, halving that to achieve the number of women, the upper limit is set
  to five.)")

deathRateMessage <- gsub("\n[\ \t]+", " ", r"(Must be between {left} and
  {right}; these limits are taken from ourworldindata.org's global death rate
  from 1950 to 2021. The upper limit is the global death rate in 1950.)")

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
    muBirth = c(sv_between(0, 5, message_fmt = birthRateMessage)),
    muDeath = c(sv_between(0, 0.01952, message_fmt = deathRateMessage)),

    ## Compartmentsk
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
