#Susceptible ode
Susceptible <- r"(    &\frac{dS}{dt} = )"
#Infection ode
Infection <- r"(    &\frac{dI}{dt} = )"
#Recover ode
Recover <- r"(    &\frac{dR}{dt} = )"
#Exposure ode
Exposure <- r"(    &\frac{dE}{dt} = )"
#Dead ode
Dead <- r"(    &\frac{dD}{dt} = )"


forceOfInfection <- function(trueMassAction)
{
  if (as.numeric(trueMassAction) == 0) {
    r"( \beta { S I})"
  } else {
    r"( \beta \frac{ S I}{N})"
  }
}

SIR_nonVD <- function(forceOfInfection) {
  S = paste0(Susceptible, "- ", forceOfInfection, r"(\\)")
  I = paste0(Infection, forceOfInfection, r"(- \gamma {I}\\)")
  R = paste0(Recover, r"(\gamma {I}\\)")
  return(c(S,I,R))
}

SIRS_nonVD <- function(forceOfInfection) {
  S = paste0(Susceptible, "- ", forceOfInfection, r"(+ \xi{R}\\)")
  I = paste0(Infection, forceOfInfection, r"(- \gamma {I}\\)")
  R = paste0(Recover, r"(\gamma {I} - \xi{R}\\)")
  return(c(S,I,R))
}

SIRD_nonVD <- function(forceOfInfection) {
  S <- paste0(Susceptible, "- ", forceOfInfection, r"(\\)")
  I <- paste0(Infection, forceOfInfection, r"(- \gamma {I} - \delta {I}\\)")
  R <- paste0(Recover, r"(\gamma {I}\\)")
  D <- paste0(Dead, r"(\delta {I})")
  return(c(S,I,R,D))
}

SEIR_nonVD <- function(forceOfInfection){
  S <- paste0(Susceptible, "- ", forceOfInfection, r"(\\)")
  E <- paste0(Exposure, forceOfInfection, r"(- \gamma {E}\\)")
  I <- paste0(Infection, r"(\gamma {E} - \sigma {I}\\)")
  R <- paste0(Recover, r"(\sigma {I}\\)")
  return(c(S,E,I,R))
}

SEIRD_nonVD <- function(forceOfInfection){
  S <- paste0(Susceptible, "- ", forceOfInfection, r"(\\)")
  E <- paste0(Exposure, forceOfInfection, r"(- \gamma {E}\\)")
  I <- paste0(Infection, r"(\gamma {E} - \sigma {I} - \delta {I}\\)")
  R <- paste0(Recover, r"(\sigma {I}\\)")
  D <- paste0(Dead, r"(\delta {I})")
  return(c(S,E,I,R,D))
}

SEIRS_nonVD <- function(forceOfInfection){
  S <- paste0(Susceptible, "- ", forceOfInfection, r"(+ \xi {R}\\)")
  E <- paste0(Exposure, forceOfInfection, r"(- \gamma {E}\\)")
  I <- paste0(Infection, r"(\gamma {E} - \sigma {I}\\)")
  R <- paste0(Recover, r"(\sigma {I} - \xi{R}\\)")
  return(c(S,E,I,R))
}

SIR <- function(mu, forceOfInfection)
{
  if (!mu)
  {
    variables = SIR_nonVD(forceOfInfection)
    S = variables[[1]]
    I = variables[[2]]
    R = variables[[3]]
  } else
  {
    S = paste0(Susceptible, r"(\mu_B N - \mu_D S -)", forceOfInfection,r"(\\)")
    I = paste0(Infection, forceOfInfection, r"(- \gamma I - \mu_D {I}\\)")
    R = paste0(Recover, r"(\gamma {I} - \mu_D {R}\\)")
  }
  equation <- c(S, I, R)
  return(equation)
}


SIRS <- function(mu, forceOfInfection)
{
  if (!mu)
  {
    variables = SIRS_nonVD(forceOfInfection)
    S = variables[[1]]
    I = variables[[2]]
    R = variables[[3]]
  } else
  {
    S = paste0(Susceptible,
               r"( \mu_B {N} -)",
               forceOfInfection,
               r"(+ \xi{R} - \mu_D {S}\\)")
    I = paste0(Infection, forceOfInfection, r"(- \gamma {I} - \mu_D {I}\\)")
    R = paste0(Recover, r"(\gamma {I} - \xi{R} - \mu_D {R}\\)")
  }
  equation <- c(S, I, R)
  return(equation)
}

SIRD <- function(mu, forceOfInfection)
{
  if (!mu)
  {
    variables = SIRD_nonVD(forceOfInfection)
    S = variables[[1]]
    I = variables[[2]]
    R = variables[[3]]
    D = variables[[4]]
  } else
  {
    S <- paste0(Susceptible, r"(\mu_B {N} - \mu_D {S} - )", forceOfInfection, r"(\\)")
    I <- paste0(Infection,
                forceOfInfection,
                r"(- \gamma {I} - \delta {I} - \mu_D {I}\\)")
    R <- paste0(Recover, r"(\gamma {I} - \mu_D {R}\\)")
    D <- paste0(Dead, r"(\delta {I})")
  }
  equation <- c(S, I, R, D)
  return(equation)
}

SEIR <- function(mu, forceOfInfection)
{
  if (!mu)
  {
    variables = SEIR_nonVD(forceOfInfection)
    S = variables[[1]]
    E = variables[[2]]
    I = variables[[3]]
    R = variables[[4]]
  } else
  {
    S <- paste0(Susceptible, r"(\mu_B {N} - \mu_D {S} -)", forceOfInfection, r"(\\)")
    E <- paste0(Exposure, forceOfInfection, r"(- \gamma {E} - \mu_D {E}\\)")
    I <- paste0(Infection, r"(\gamma {E} - \sigma {I} - \mu_D {I}\\)")
    R <- paste0(Recover, r"(\sigma {I} - \mu_D {R}\\)")
  }
  equation <- c(S, E, I, R)
  return(equation)
}

SEIRD <- function(mu, forceOfInfection)
{
  if (!mu)
  {
    variables = SEIRD_nonVD(forceOfInfection)
    S = variables[[1]]
    E = variables[[2]]
    I = variables[[3]]
    R = variables[[4]]
    D = variables[[5]]
  } else
  {
    S <- paste0(Susceptible, r"(\mu_B {N} - \mu_D {S} -)", forceOfInfection, r"(\\)")
    E <- paste0(Exposure, forceOfInfection, r"(- \gamma {E} - \mu_D {E}\\)")
    I <- paste0(Infection,
                r"(\gamma {E} - \sigma {I} -  \delta {I} - \mu_D {I}\\)")
    R <- paste0(Recover, r"(\sigma {I} - \mu_D {R}\\)")
    D <- paste0(Dead, r"(\delta {I})")
  }
  equation <- c(S, E, I, R, D)
  return(equation)
}

SEIRS <- function(mu, forceOfInfection)
{
  if (!mu)
  {
    variables = SEIRS_nonVD(forceOfInfection)
    S = variables[[1]]
    E = variables[[2]]
    I = variables[[3]]
    R = variables[[4]]
  } else
  {
    S <- paste0(Susceptible,
                r"(\mu_B {N} -)",
                forceOfInfection,
                r"(+ \xi{R} - \mu_D {S}\\)")
    E <- paste0(Exposure, forceOfInfection, r"(- \gamma {E} - \mu_D {E}\\)")
    I <- paste0(Infection, r"(\gamma {E} - \sigma {I} - \mu_D {I}\\)")
    R <- paste0(Recover, r"(\sigma {I} - \xi{R} - \mu_D {R}\\)")
  }
  equation <- c(S, E, I, R)
  return(equation)
}
