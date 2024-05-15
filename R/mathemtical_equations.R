
# SIR model with no vital dynamic
SIR_nonVD_equation <- c(r"(    &\frac{dS}{dt} = - \beta \frac{ S I}{N^q})", 
                        r"(    &\frac{dI}{dt} = \frac{\beta S I}{N^q} - \gamma {I})", 
                        r"(    &\frac{dR}{dt} = \gamma {I})", 
                        r"(    &R_0 =  \frac{\beta}{\gamma} S(0)^q)", 
                        r"(    &\begin{cases} 1 & \text{, frequency-dependent} \\\\ 0 & \text{, density-dependent} \end{cases})")
# SIR model with vital dynamic
SIR_VD_equation <- c(r"(    &\frac{dS}{dt} = \mu_B N - \mu_D S - \beta \frac{ S I}{N^q})", 
                     r"(    &\frac{dI}{dt} = \frac{\beta S I}{N^q} - \gamma I - \mu_D {I})", 
                     r"(    &\frac{dR}{dt} = \gamma I - \mu_D {R})", 
                     r"(    &R_0 =  \frac{\beta}{\gamma} S(0)^q)", 
                     r"(    &\begin{cases} 1 & \text{, frequency-dependent} \\\\ 0 & \text{, density-dependent} \end{cases})")

# SIRS with no vital dynamic
SIRS_nonVD_equation <- c(r"(    &\frac{dS}{dt} = - \beta \frac{ S I}{N} + \xi{R}\\)",
                         r"(    &\frac{dI}{dt} = \frac{\beta S I}{N} - \gamma {I}\\)", 
                         r"(    &\frac{dR}{dt} = \gamma {I} - \xi{R}\\)", 
                         r"(    &R_0 =  \frac{\beta}{\gamma} S(0)^q\\)", 
                         r"(    &\begin{cases} 1 & \text{, frequency-dependent} \\\\ 0 & \text{, density-dependent} \end{cases}\\)")
# SIRS with vital dynamic
SIRS_VD_equation <- c( r"(    &\frac{dS}{dt} = \mu{N}- \beta \frac{ S I}{N} + \xi{R} - \nu{S}\\)",  
                       r"(    &\frac{dI}{dt} = \frac{\beta S I}{N} - \gamma {I} - \nu{I}\\)", 
                       r"(    &\frac{dR}{dt} = \gamma {I} - \xi{R} - \nu{R}\\)", 
                       r"(    &R_0 =  \frac{\beta}{\gamma} S(0)^q\\)", 
                       r"(    &\begin{cases} 1 & \text{, frequency-dependent} \\\\ 0 & \text{, density-dependent} \end{cases}\\)")

# SIRD model with no vital dynamic
SIRD_nonVD_equation <- c( r"(    &\frac{dS}{dt} = - \beta \frac{ S I}{N^q}\\)", 
                          r"(    &\frac{dI}{dt} = \frac{\beta S I}{N^q} - \gamma {I} - \delta {I}\\)", 
                          r"(    &\frac{dR}{dt} = \gamma {I}\\)", 
                          r"(    &\frac{dD}{dt} = \delta {I}\\)", 
                          r"(    &R_0 =  \frac{\beta}{\gamma} S(0)^q\\)", 
                          r"(    &\begin{cases} 1 & \text{, frequency-dependent} \\\\ 0 & \text{, density-dependent} \end{cases}\\)")
# SIRD model with vital dynamic
SIRD_VD_equation <- c(r"(    &\frac{dS}{dt} =\mu_B {N} - \mu_D {S} - \beta \frac{ S I}{N^q}\\)",  
                      r"(    &\frac{dI}{dt} = \frac{\beta S I}{N^q} - \gamma {I} - \delta {I} - \mu_D {I}\\)", 
                      r"(    &\frac{dR}{dt} = \gamma {I} - \mu_D {R}\\)",
                      r"(    &\frac{dD}{dt} = \delta {I}\\)", 
                      r"(    &R_0 =  \frac{\beta}{\gamma} S(0)^q\\)", 
                      r"(    &\begin{cases} 1 & \text{, frequency-dependent} \\\\ 0 & \text{, density-dependent} \end{cases}\\)")

# SEIR model with no vital dynamic
SEIR_nonVD_equation <- c(r"(    &\frac{dS}{dt} = - \beta \frac{ S I}{N^q}\\)", 
                         r"(    &\frac{dE}{dt} = \beta \frac{ S I}{N^q} - \gamma {E}\\)", 
                         r"(    &\frac{dI}{dt} = \gamma {E} - \sigma {I}\\)", 
                         r"(    &\frac{dR}{dt} = \sigma {I}\\)", 
                         r"(    &R_0 =  \frac{\beta}{\gamma} S(0)^q\\)", 
                         r"(    &\begin{cases} 1 & \text{, frequency-dependent} \\\\ 0 & \text{, density-dependent} \end{cases}\\)")
# SEIR model with vital dynamic
SEIR_VD_equation <- c(r"(    &\frac{dS}{dt} = \mu_B {N} - \mu_D {S} - \beta \frac{ S I}{N^q}\\)", 
                      r"(    &\frac{dE}{dt} = \beta \frac{ S I}{N^q} - \gamma {E} - - \mu_D {E}\\)", 
                      r"(    &\frac{dI}{dt} = \gamma {E} - \sigma {I} - \mu_D {I}\\)", 
                      r"(    &\frac{dR}{dt} = \sigma {I}) - \mu_D {R}\\)",  
                      r"(    &R_0 =  \frac{\beta}{\gamma} S(0)^q\\)", 
                      r"(    &\begin{cases} 1 & \text{, frequency-dependent} \\\\ 0 & \text{, density-dependent} \end{cases}\\)")

# SEIRD model with no vital dynamic
SEIRD_nonVD_equation <- c(r"(    &\frac{dS}{dt} = - \beta \frac{ S I}{N^q}\\)", 
                          r"(    &\frac{dE}{dt} = \beta \frac{ S I}{N^q} - \gamma {E}\\)",
                          r"(    &\frac{dI}{dt} = \gamma {E} - \sigma {I} - \delta {I}\\)",
                          r"(    &\frac{dR}{dt} = \sigma {I}\\)",  
                          r"(    &\frac{dD}{dt} = \delta {I}\\)",
                          r"(    &R_0 =  \frac{\beta}{\gamma} S(0)^q\\)", 
                          r"(    &\begin{cases} 1 & \text{, frequency-dependent} \\\\ 0 & \text{, density-dependent} \end{cases}\\)")
# SEIRD model with vital dynamic
SEIRD_VD_equation <- c(r"(    &\frac{dS}{dt} = \mu_B {N} - \mu_D {S} - \beta \frac{ S I}{N^q}\\)",
                       r"(    &\frac{dE}{dt} = \beta \frac{ S I}{N^q} - \gamma {E} - - \mu_D {E}\\)",
                       r"(    &\frac{dI}{dt} = \gamma {E} - \sigma {I} -  \delta {I} - \mu_D {I}\\)", 
                       r"(    &\frac{dR}{dt} = \sigma {I}) - \mu_D {R}\\)", 
                       r"(    &\frac{dD}{dt} = \delta {I}\\)", 
                       r"(    &R_0 =  \frac{\beta}{\gamma} S(0)^q\\)", 
                       r"(    &\begin{cases} 1 & \text{, frequency-dependent} \\\\ 0 & \text{, density-dependent} \end{cases}\\)")