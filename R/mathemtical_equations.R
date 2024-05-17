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


getForceOfInfection <- function(qValue)
{
  if (qValue == 0){forceOfInfection = r"( \beta { S I})"} else {forceOfInfection = r"( \beta \frac{ S I}{N})" }
  return(forceOfInfection)
}

SIR <- function(mu,forceOfInfection)
{
  if (!mu)
  {
    S = paste0(Susceptible, "- ", forceOfInfection, r"(\\)")
    I = paste0(Infection, forceOfInfection, r"(- \gamma {I}\\)")
    R = paste0(Recover, r"(\gamma {I}\\)")
  } else 
  {
    S = paste0(Susceptible, r"(\mu_B N - \mu_D S -)", forceOfInfection)
    I = paste0(Infection, forceOfInfection, r"(- \gamma I - \mu_D {I}\\)")
    R = paste0(Recover, r"(\gamma {I} - \mu_D {R}\\)")
  }
  equation <- c(S,I,R)
  return(equation)
}


SIRS <- function(mu,forceOfInfection)
{
  if (!mu)
  {
    S = paste0(Susceptible, "- ", forceOfInfection, r"(+ \xi{R}\\)")
    I = paste0(Infection, forceOfInfection, r"(- \gamma {I}\\)")
    R = paste0(Recover, r"(\gamma {I} - \xi{R}\\)" )
  } else 
  {
    S = paste0(Susceptible, r"( \mu_B {N} -)", forceOfInfection, r"(+ \xi{R} - \mu_D {S}\\)")
    I = paste0(Infection, forceOfInfection, r"(- \gamma {I} - \mu_D {I}\\)" )
    R = paste0(Recover, r"(\gamma {I} - \xi{R} - \mu_D {R}\\)")
  }
  equation <- c(S,I,R)
  return(equation)
}

SIRD <- function(mu,forceOfInfection)
{
  if (!mu)
  {
    S <- paste0(Susceptible, "- ", forceOfInfection)
    I <- paste0(Infection, forceOfInfection, r"(- \gamma {I} - \delta {I}\\)")
    R <- paste0(Recover, r"(\gamma {I}\\)")
  }else 
  {
    S <- paste0(Susceptible, r"(\mu_B {N} - \mu_D {S} - )", forceOfInfection)
    I <- paste0(Infection, forceOfInfection, r"(- \gamma {I} - \delta {I} - \mu_D {I}\\)" )
    R <- paste0(Recover, r"(\gamma {I} - \mu_D {R}\\)")
  }
  D <- paste0(Dead, r"(\delta {I})" )
  equation <- c(S,I,R,D)
  return(equation)
}

SEIR <- function(mu,forceOfInfection)
{
  if (!mu)
  {
    S <- paste0(Susceptible, "- ", forceOfInfection)
    E <- paste0(Exposure, forceOfInfection, r"(- \gamma {E}\\)")
    I <- paste0(Infection, r"(\gamma {E} - \sigma {I}\\)" )
    R <- paste0(Recover, r"(\sigma {I}\\)")
  } else 
  {
    S <- paste0(Susceptible, r"(\mu_B {N} - \mu_D {S} -)", forceOfInfection)
    E <- paste0(Exposure, forceOfInfection, r"(- \gamma {E} - \mu_D {E}\\)")
    I <- paste0(Infection, r"(\gamma {E} - \sigma {I} - \mu_D {I}\\)")
    R <- paste0(Recover, r"(\sigma {I} - \mu_D {R}\\)")
  }
  equation <- c(S,E,I,R)
  return(equation)
}

SEIRD <- function(mu,forceOfInfection)
{
  if (!mu)
  {
    S <- paste0(Susceptible, "- ", forceOfInfection)
    E <- paste0(Exposure, forceOfInfection, r"(- \gamma {E}\\)")
    I <- paste0(Infection, r"(\gamma {E} - \sigma {I} - \delta {I}\\)" )
    R <- paste0(Recover, r"(\sigma {I}\\)")
  } else 
  {
    S <- paste0(Susceptible, r"(\mu_B {N} - \mu_D {S} -)", forceOfInfection)
    E <- paste0(Exposure, forceOfInfection, r"(- \gamma {E} - \mu_D {E}\\)")
    I <- paste0(Infection, r"(\gamma {E} - \sigma {I} -  \delta {I} - \mu_D {I}\\)")
    R <- paste0(Recover, r"(\sigma {I} - \mu_D {R}\\)")
  }
  D <- paste0(Dead, r"(\delta {I})" )
  equation <- c(S,E,I,R,D)
  return(equation)
}

SEIRS <- function(mu,forceOfInfection)
{
  if (!mu)
  {
    S <- paste0(Susceptible, "- ", forceOfInfection, r"(+ \xi {R})")
    E <- paste0(Exposure, forceOfInfection, r"(- \gamma {E}\\)")
    I <- paste0(Infection, r"(\gamma {E} - \sigma {I}\\)" )
    R <- paste0(Recover, r"(\sigma {I} - \xi{R}\\)")
  } else 
  {
    S <- paste0(Susceptible, r"(\mu_B {N} -)", forceOfInfection,  r"(+ \xi{R} - \mu_D {S}\\)")
    E <- paste0(Exposure, forceOfInfection, r"(- \sigma {E} - \mu_D {E}\\)")
    I <- paste0(Infection, r"(\sigma {E} - \gamma {I} - \mu_D {I}\\)")
    R <- paste0(Recover, r"(\gamma {I} - \xi{R} - \mu_D {R}\\)")
  }
  equation <- c(S,E,I,R)
  return(equation)
}
