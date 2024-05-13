SIR_LaTeX <- function(mu) {
  switch(mu + 1, 
    helpText(
      r"(Susceptible $$\frac{dS}{dt} = - \beta \frac{ S I}{N^q}$$)",
      r"(Infectious $$\frac{dI}{dt} = \frac{\beta S I}{N^q} - \gamma {I} $$)",
      r"(Recovered $$\frac{dR}{dt} = \gamma {I} $$)",
      r"(Reproductive ratio $$R_0 =  \frac{\beta}{\gamma} S(0)^q$$)",
      r"(q-Value $$\begin{equation} x = \begin{cases} 1 & \text{, } frequency − dependent \\\\ 0 & \text{, } density − dependent \end{cases}\end{equation} $$)"
    ),
    helpText(
      r"(Susceptible $$\frac{dS}{dt} =\mu_B N - \mu_D S - \beta \frac{ S I}{N^q}$$)",
      r"(Infectious $$\frac{dI}{dt} = \frac{\beta S I}{N^q} - \gamma {I} - \mu_D I$$)",
      r"(Recovered $$\frac{dR}{dt} = \gamma {I} - \mu_D R $$)",
      r"(Reproductive ratio $$R_0 =  \frac{\beta}{\gamma} S(0)^q$$)",
      r"(q-Value $$\begin{equation} x = \begin{cases} 1 & \text{, } frequency − dependent \\\\ 0 & \text{, } density − dependent \end{cases}\end{equation} $$)"
    )
  )
}

## Khanh TODO: Please write the switch statement with the appropriate LaTeX, in
## raw strings as in the other functions in this file, for this model. NOTE: if,
## and only if, the function needs the value of the mu parameter should you give
## the function the formal parameter; if needed, then make the function call in
## server.R pass input$muValue also.
SIRS_LaTeX <- function(mu) {
  switch(mu + 1, 
         helpText(
           r"(Susceptible $$\frac{dS}{dt} = - \beta \frac{ S I}{N} + \xi{R}$$)",
           r"(Infectious $$\frac{dI}{dt} = \frac{\beta S I}{N} - \gamma {I}$$)",
           r"(Recovered $$\frac{dR}{dt} = \gamma {I} - \xi{R} $$)",
           r"(Reproductive ratio $$R_0 =  \frac{\beta}{\gamma} S(0)^q$$)", #need to double check 
           r"(q-Value $$\begin{equation} x = \begin{cases} 1 & \text{, } frequency − dependent \\\\ 0 & \text{, } density − dependent \end{cases}\end{equation} $$)"
           ),
         helpText(
           # want to make sure it is \nu{S}
           r"(Susceptible $$\frac{dS}{dt} = \mu{N}- \beta \frac{ S I}{N} + \xi{R} - \nu{S}$$)", 
           r"(Infectious $$\frac{dI}{dt} = \frac{\beta S I}{N} - \gamma {I} - \nu{I}$$)",
           r"(Recovered $$\frac{dR}{dt} = \gamma {I} - \xi{R} - \nu{R}$$)",
           r"(Reproductive ratio $$R_0 =  \frac{\beta}{\gamma} S(0)^q$$)", #need to double check 
           r"(q-Value $$\begin{equation} x = \begin{cases} 1 & \text{, } frequency − dependent \\\\ 0 & \text{, } density − dependent \end{cases}\end{equation} $$)"
           )
         )
}

SIRD_LaTeX <- function(mu) {
  switch(mu + 1,
    helpText(
      r"(Susceptible $$\frac{dS}{dt} = - \beta \frac{ S I}{N^q}$$)",
      r"(Infectious $$\frac{dI}{dt} = \frac{\beta S I}{N^q} - \gamma {I} - \delta {I} $$)",
      r"(Recovered $$\frac{dR}{dt} = \gamma {I} $$)",
      r"(Dead $$\frac{dD}{dt} = \delta {I} $$)",
      r"(Reproductive ratio $$R_0 =  \frac{\beta}{\gamma + \delta} S(0)^q$$)",
      r"(q-Value $$\begin{equation} x = \begin{cases} 1 & \text{, } frequency − dependent \\\\ 0 & \text{, } density − dependent \end{cases}\end{equation} $$)"
    ),
    helpText(
      r"(Susceptible $$\frac{dS}{dt} =\mu_B N - \mu_D S - \beta \frac{ S I}{N^q}$$)",
      r"(Infectious $$\frac{dI}{dt} = \frac{\beta S I}{N^q} - \gamma {I} - \delta {I} - \mu_D I$$)",
      r"(Recovered $$\frac{dR}{dt} = \gamma {I} - \mu_D R $$)",
      r"(Dead $$\frac{dD}{dt} = \delta {I} $$)",
      r"(Reproductive ratio $$R_0 =  \frac{\beta}{\gamma + \delta} S(0)^q$$)",
      r"(q-Value $$\begin{equation} x = \begin{cases} 1 & \text{, } frequency − dependent \\\\ 0 & \text{, } density − dependent \end{cases}\end{equation} $$)"
    )
  )
}

SEIR_LaTeX <- function(mu) {
  switch(mu + 1,
    helpText(
      r"(Susceptible $$\frac{dS}{dt} = - \beta \frac{ S I}{N^q}$$)",
      r"(Exposed $$\frac{dE}{dt} = \beta \frac{ S I}{N^q} - \gamma {E} $$)",
      r"(Infectious $$\frac{dI}{dt} = \gamma {E} - \sigma {I} $$)",
      r"(Recovered $$\frac{dR}{dt} = \sigma {I} $$)",
      r"(Reproductive ratio $$R_0 =  \frac{\beta}{\gamma}$$)",
      r"(q-Value $$\begin{equation} x = \begin{cases} 1 & \text{, } frequency − dependent \\\\ 0 & \text{, } density − dependent \end{cases}\end{equation} $$)"
    ),
    helpText(
      r"(Susceptible $$\frac{dS}{dt} =\mu_B N - \mu_D S - \beta \frac{ S I}{N^q}$$)",
      r"(Exposed $$\frac{dE}{dt} = \beta \frac{ S I}{N^q} - \gamma {E} - \mu_D {E}$$)",
      r"(Infectious $$\frac{dI}{dt} = \gamma E - \sigma I - \mu_D I $$)",
      r"(Recovered $$\frac{dR}{dt} = \sigma I - \mu_D R $$)",
      r"(Reproductive ratio $$R_0 =  \frac{\beta}{\gamma}$$)",
     r"(q-Value $$\begin{equation} x = \begin{cases} 1 & \text{, } frequency − dependent \\\\ 0 & \text{, } density − dependent \end{cases}\end{equation} $$)"
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
      r"(q-Value $$\begin{equation} x = \begin{cases} 1 & \text{, } frequency − dependent \\\\ 0 & \text{, } density − dependent \end{cases}\end{equation} $$)"
    ),
    helpText(
      r"(Susceptible $$\frac{dS}{dt} =\mu_B N - \mu_D S - \beta \frac{ S I}{N^q}$$)",
      r"(Exposed $$\frac{dE}{dt} = \beta \frac{ S I}{N^q} - \gamma E - \mu_D E$$)",
      r"(Infectious $$\frac{dI}{dt} = \gamma E - \sigma I -  \delta I- \mu_D I $$)",
      r"(Recovered $$\frac{dR}{dt} = \sigma I - \mu_D R $$)",
      r"(Dead $$ \frac{dD}{dt} = \delta I $$)",
      r"(Reproductive ratio $$R_0 =  \frac{\beta}{\gamma}$$)",
      r"(q-Value $$\begin{equation} x = \begin{cases} 1 & \text{, } frequency − dependent \\\\ 0 & \text{, } density − dependent \end{cases}\end{equation} $$)"
    )
  )
}
