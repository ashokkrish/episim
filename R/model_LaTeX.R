SIR_LaTeX <- function(mu) {
  switch(mu + 1, # See the function help for why this addition is used.
    helpText(
      r"(Susceptible $$\frac{dS}{dt} = - \beta \frac{ S I}{N^q}$$)",
      r"(Infectious $$\frac{dI}{dt} = \frac{\beta S I}{N^q} - \gamma I $$)",
      r"(Recovered $$\frac{dR}{dt} = \gamma I $$)",
      r"(Reproductive ratio $$R_0 =  \frac{\beta}{\gamma} S(0)^q$$)",
      "q-Value", br(),
      r"($$ 1, frequency-dependent $$)", br(),
      r"($$ 0, density-dependent $$)"
    ),
    helpText(
      r"(Susceptible $$\frac{dS}{dt} =\mu_B N - \mu_D S - \beta \frac{ S I}{N^q}$$)",
      r"(Infectious $$\frac{dI}{dt} = \frac{\beta S I}{N^q} - \gamma I - \mu_D I$$)",
      r"(Recovered $$\frac{dR}{dt} = \gamma I - \mu_D R $$)",
      r"(Reproductive ratio $$R_0 =  \frac{\beta}{\gamma} S(0)^q$$)",
      "q-Value", br(),
      r"($$ 1, frequency-dependent $$)", br(),
      r"($$ 0, density-dependent $$)"
    )
  )
}

## Khanh TODO: Please write the switch statement with the appropriate LaTeX, in
## raw strings as in the other functions in this file, for this model. NOTE: if,
## and only if, the function needs the value of the mu parameter should you give
## the function the formal parameter; if needed, then make the function call in
## server.R pass input$muValue also.
SIRS_LaTeX <- function() {}

SIRD_LaTeX <- function(mu) {
  switch(mu + 1,
    helpText(
      r"(Susceptible $$\frac{dS}{dt} = - \beta \frac{ S I}{N^q}$$)",
      r"(Infectious $$\frac{dI}{dt} = \frac{\beta S I}{N^q} - \gamma I - \delta I $$)",
      r"(Recovered $$\frac{dR}{dt} = \gamma I $$)",
      r"(Dead $$\frac{dD}{dt} = \delta I $$)",
      r"(Reproductive ratio $$R_0 =  \frac{\beta}{\gamma + \delta} S(0)^q$$)",
      "q-Value", br(), r"($$ 1, frequency-dependent $$)", br(), r"($$ 0, density-dependent $$)"
    ),
    helpText(
      r"(Susceptible $$\frac{dS}{dt} =\mu_B N - \mu_D S - \beta \frac{ S I}{N^q}$$)",
      r"(Infectious $$\frac{dI}{dt} = \frac{\beta S I}{N^q} - \gamma I - \delta I - \mu_D I$$)",
      r"(Recovered $$\frac{dR}{dt} = \gamma I - \mu_D R $$)",
      r"(Dead $$\frac{dD}{dt} = \delta I $$)",
      r"(Reproductive ratio $$R_0 =  \frac{\beta}{\gamma + \delta} S(0)^q$$)",
      "q-Value", br(), r"($$ 1, frequency-dependent $$)", br(), r"($$ 0, density-dependent $$)"
    )
  )
}

SEIR_LaTeX <- function(mu) {
  switch(mu + 1,
    helpText(
      r"(Susceptible $$\frac{dS}{dt} = - \beta \frac{ S I}{N^q}$$)",
      r"(Exposed $$\frac{dE}{dt} = \beta \frac{ S I}{N^q} - \gamma E $$)",
      r"(Infectious $$\frac{dI}{dt} = \gamma E - \sigma I $$)",
      r"(Recovered $$\frac{dR}{dt} = \sigma I $$)",
      r"(Reproductive ratio $$R_0 =  \frac{\beta}{\gamma}$$)",
      "q-Value", br(), r"($$ 1, frequency-dependent $$)", br(), r"($$ 0, density-dependent $$)"
    ),
    helpText(
      r"(Susceptible $$\frac{dS}{dt} =\mu_B N - \mu_D S - \beta \frac{ S I}{N^q}$$)",
      r"(Exposed $$\frac{dE}{dt} = \beta \frac{ S I}{N^q} - \gamma E - \mu_D E$$)",
      r"(Infectious $$\frac{dI}{dt} = \gamma E - \sigma I - \mu_D I $$)",
      r"(Recovered $$\frac{dR}{dt} = \sigma I - \mu_D R $$)",
      r"(Reproductive ratio $$R_0 =  \frac{\beta}{\gamma}$$)",
      "q-Value", br(), r"($$ 1, frequency-dependent $$)", br(), r"($$ 0, density-dependent $$)"
    )
  )
}

SEIRD_LaTeX <- function(mu) {
  switch(mu + 1,
    helpText(
      r"(Susceptible $$\frac{dS}{dt} = - \beta \frac{ S I}{N^q}$$)",
      r"(Exposed $$\frac{dE}{dt} = \beta \frac{ S I}{N^q} - \gamma E $$)",
      r"(Infectious $$\frac{dI}{dt} = \gamma E - \sigma I - \delta I $$)",
      r"(Recovered $$\frac{dR}{dt} = \sigma I $$)",
      r"(Dead $$ \frac{dD}{dt} = \delta I $$)",
      r"(Reproductive ratio $$R_0 =  \frac{\beta}{\gamma}$$)",
      "q-Value", br(), r"($$ 1, frequency-dependent $$)", br(), r"($$ 0, density-dependent $$)"
    ),
    helpText(
      r"(Susceptible $$\frac{dS}{dt} =\mu_B N - \mu_D S - \beta \frac{ S I}{N^q}$$)",
      r"(Exposed $$\frac{dE}{dt} = \beta \frac{ S I}{N^q} - \gamma E - \mu_D E$$)",
      r"(Infectious $$\frac{dI}{dt} = \gamma E - \sigma I -  \delta I- \mu_D I $$)",
      r"(Recovered $$\frac{dR}{dt} = \sigma I - \mu_D R $$)",
      r"(Dead $$ \frac{dD}{dt} = \delta I $$)",
      r"(Reproductive ratio $$R_0 =  \frac{\beta}{\gamma}$$)",
      "q-Value", br(), r"($$ 1, frequency-dependent $$)", br(), r"($$ 0, density-dependent $$)"
    )
  )
}
