# Define the CSS styles once
css_styles <- HTML(paste0(
  '<style>',
  '  .equation-container {',
  '    display: flex;',
  '    justify-content: flex-start;',
  '  }',
  '  .equation-block {',
  '    margin: 0;',
  '    padding-left: 20px;',
  '    font-size: 20px;', 
  '    text-align: justify;',  # Added property for justified text
  '  }',
  '</style>'
))

# Helper function to generate the LaTeX content
generate_latex <- function(equations) {
  HTML(paste0(
    css_styles,
    '<div class="equation-container">',
    '<div class="equation-block">',
    r"(\begin{align*})",
    paste(equations, collapse = r"(\\)"),
    r"(\end{align*})",
    '</div>',
    '</div>'
  ))
}

css_styles_description <- HTML(paste0(
  '<style>',
  '  .text-container {',
  '    display: block;',
  '    margin: 0;',
  '    padding-left: 20px;',
  '    padding-right: 20px;',
  '    font-size: 25px;', 
  '    text-align: justify;',  # Added property for justified text
  '    font-family: "Times New Roman", Times, serif;',  # Specify Times New Roman font
  '  }',
  '</style>'
))

# Helper function to generate the text content
generate_text_discription <- function(content) {
  HTML(paste0(
    css_styles_description,
    '<div class="text-container">',
    content,
    '</div>'
  ))
}

renderModelLaTeX <- function(modelSelect, vitalDynamics, trueMassAction) {
  do.call(modelSelect,
          list(vitalDynamics, forceOfInfection(trueMassAction))) |>
    generate_latex() |>
    helpText() |>
    withMathJax()
}

renderStochasticModelLaTex <- function(modelSelect, trueMassAction){
  do.call(paste0(modelSelect,"_nonVD"),
          list(forceOfInfection(trueMassAction))) |>
    generate_latex() |>
    helpText() |>
    withMathJax()
}
renderStochasticDescription <- function(distribution){
  if (distribution == 1) { #binomial
    generate_text_discription(binomialDescription) |> helpText() |> withMathJax()
  } else { #uniform
    generate_text_discription(uniformDescription) |> helpText() |> withMathJax()
  }
}
renderR0Equation <- function(modelSelect, trueMassAction){
  do.call(paste0(modelSelect,"_R0"),
          list(trueMassAction)) |>
    generate_latex() |>
    helpText() |>
    withMathJax()
}

R0Result <- function(modelSelect,population, beta, gamma, sigma = 0, delta = 0, xi = 0, 
                     trueMassAction = FALSE){
  do.call(paste0(modelSelect,"_R0Result"),
          list(population, beta, gamma, sigma, delta, xi, 
               trueMassAction)) 
}


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

SIR_R0 <- function(trueMassAction){
  equation <- r"(R_0 = \frac{\beta}{\gamma})"
  if(as.numeric(trueMassAction) == 0){
    equation <- r"(R_0 = \frac{\beta}{\gamma}N)"
  }
  return(equation)
}

SIRS_R0 <- function(trueMassAction){
  equation <- r"(R_0 = \frac{\beta}{\gamma + \xi})"
  if(as.numeric(trueMassAction) == 0){
    equation <- r"(R_0 = \frac{\beta}{\gamma + \xi}N)"
  }
  return(equation)
}

SIRD_R0 <- function(trueMassAction){
  equation <- r"(R_0 = \frac{\beta}{\gamma + \delta})"
  if(as.numeric(trueMassAction) == 0){
    equation <- r"(R_0 = \frac{\beta}{\gamma + \delta}N)"
  }
  return(equation)
}

SEIR_R0 <- function(trueMassAction){
  equation <- r"(R_0 = \frac{\beta}{\sigma})"
  if(as.numeric(trueMassAction) == 0){
    equation <- r"(R_0 = \frac{\beta}{\sigma}N)"
  }
  return(equation)
}

SEIRS_R0 <- function(trueMassAction){
  equation <- r"(R_0 = \frac{\beta}{\sigma + \xi})"
  if(as.numeric(trueMassAction) == 0){
    equation <- r"(R_0 = \frac{\beta}{\sigma + \xi}N)"
  }
  return(equation)
}

SEIRD_R0 <- function(trueMassAction){
  equation <- r"(R_0 = \frac{\beta}{\sigma + \delta})"
  if(as.numeric(trueMassAction) == 0){
    equation <- r"(R_0 = \frac{\beta}{\sigma + \delta}N)"
  }
  return(equation)
}

SIR_R0Result <- function(population, beta, gamma, sigma = 0, delta = 0, xi = 0, 
                         trueMassAction = FALSE){
  equation <- paste0("\\frac{", format(beta, scientific = FALSE), "}{", format(gamma, scientific = FALSE), "} ")
  if (as.numeric(trueMassAction) == 0){
    equation <- paste0("\\frac{", format(beta, scientific = FALSE), "}{", format(gamma, scientific = FALSE), "} \\times ", format(population, scientific = FALSE))
  }
  return(equation)
}

SIRS_R0Result <- function(population, beta, gamma, sigma = 0, delta = 0, xi = 0, 
                         trueMassAction = FALSE){
  equation <- paste0("\\frac{", format(beta, scientific = FALSE), "}{", format(gamma, scientific = FALSE), " + ", format(xi, scientific = FALSE), "}")
  if(as.numeric(trueMassAction) == 0){
    equation <- paste0("\\frac{", format(beta, scientific = FALSE), "}{", format(gamma, scientific = FALSE), " + ", format(xi, scientific = FALSE), "} \\times ", format(population, scientific = FALSE))
  }
  return(equation)
}

SIRD_R0Result <- function(population, beta, gamma, sigma = 0, delta = 0, xi = 0, 
                          trueMassAction = FALSE){
  equation <- paste0("\\frac{", format(beta, scientific = FALSE), "}{", format(gamma, scientific = FALSE), " + ", format(delta, scientific = FALSE), "}")
  if(as.numeric(trueMassAction) == 0){
    equation <- paste0("\\frac{", format(beta, scientific = FALSE), "}{", format(gamma, scientific = FALSE), " + ", format(delta, scientific = FALSE), "} \\times ", format(population, scientific = FALSE))
  }
  return(equation)
}

SEIR_R0Result <- function(population, beta, gamma, sigma = 0, delta = 0, xi = 0, 
                         trueMassAction = FALSE){
  equation <- paste0("\\frac{", format(beta, scientific = FALSE), "}{", format(sigma, scientific = FALSE), "} ")
  if (as.numeric(trueMassAction) == 0){
    equation <- paste0("\\frac{", format(beta, scientific = FALSE), "}{", format(sigma, scientific = FALSE), "} \\times ", format(population, scientific = FALSE))
  }
  return(equation)
}

SEIRS_R0Result <- function(population, beta, gamma, sigma = 0, delta = 0, xi = 0, 
                          trueMassAction = FALSE){
  equation <- paste0("\\frac{", format(beta, scientific = FALSE), "}{", format(sigma, scientific = FALSE), " + ", format(xi, scientific = FALSE), "}")
  if(as.numeric(trueMassAction) == 0){
    equation <- paste0("\\frac{", format(beta, scientific = FALSE), "}{", format(sigma, scientific = FALSE), " + ", format(xi, scientific = FALSE), "} \\times ", format(population, scientific = FALSE))
  }
  return(equation)
}

SEIRD_R0Result <- function(population, beta, gamma, sigma = 0, delta = 0, xi = 0, 
                          trueMassAction = FALSE){
  equation <- paste0("\\frac{", format(beta, scientific = FALSE), "}{", format(sigma, scientific = FALSE), " + ", format(delta, scientific = FALSE), "}")
  if(as.numeric(trueMassAction) == 0){
    equation <- paste0("\\frac{", format(beta, scientific = FALSE), "}{", format(sigma, scientific = FALSE), " + ", format(delta, scientific = FALSE), "} \\times ", format(population, scientific = FALSE))
  }
  return(equation)
}
binomialDescription <- r"(The binomial stochastic epidemic model is a mathematical framework used to simulate the spread of infectious diseases within a population. It is characterized by modeling the probability of disease transmission as a series of Bernoulli trials. In this model, the population is divided into compartments, typically including susceptible (S), infected (I), and recovered (R) individuals. At each time step, the number of new infections is determined by the binomial distribution, reflecting the random nature of contacts between susceptible and infected individuals. The model incorporates parameters such as the infection rate and recovery rate, and it accounts for the inherent randomness in the spread of disease by allowing for variations in the number of new cases in each time period. This stochastic approach provides insights into the variability and unpredictability of epidemic dynamics, complementing deterministic models that offer average outcomes.)"
uniformDescription <- r"(The uniform stochastic epidemic model is a mathematical framework used to simulate the spread of infectious diseases in a population with equal probability of contact between individuals. Unlike other models that might consider varying contact rates, this model assumes that every individual has an equal chance of interacting with any other individual in the population. The population is divided into compartments, typically including susceptible (S), infected (I), and recovered (R) individuals. At each time step, new infections are determined by a uniform probability distribution, reflecting the randomness and uniformity of contacts between susceptible and infected individuals. The model incorporates parameters such as the infection rate and recovery rate, and it captures the inherent variability in the epidemic's progression by allowing for random variations in new infection cases. This stochastic approach highlights the unpredictability of disease spread and complements deterministic models by providing insights into the possible range of epidemic outcomes.)"