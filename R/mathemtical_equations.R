#SIR PMA with no vital dynamic
SIR_PMA_nonVD <- c(r"(    &\frac{dS}{dt} = - \beta { S I}\\)",
                   r"(    &\frac{dI}{dt} = {\beta S I} - \gamma {I}\\)")
#SIR PMA with vital dynamic
SIR_PMA_VD <- c(r"(    &\frac{dS}{dt} = \mu_B N - \mu_D S - \beta { S I}\\)", 
                r"(    &\frac{dI}{dt} = {\beta S I} - \gamma I - \mu_D {I}\\)")
#SIR TMA with no vital dynamic
SIR_TMA_nonVD <- c(r"(    &\frac{dS}{dt} = - \beta \frac{ S I}{N}\\)", 
                   r"(    &\frac{dI}{dt} = \frac{\beta S I}{N} - \gamma {I}\\)")
#SIR TMA with no vital dynamic
SIR_TMA_VD <- c(r"(    &\frac{dS}{dt} = \mu_B N - \mu_D S - \beta \frac{ S I}{N}\\)", 
                r"(    &\frac{dI}{dt} = \frac{\beta S I}{N} - \gamma I - \mu_D {I}\\)")

# SIR model with no vital dynamic
SIR_nonVD_equation <- c(r"(    &\frac{dR}{dt} = \gamma {I}\\)", 
                        r"(    &R_0 =  \frac{\beta}{\gamma} S(0)^q\\)") #Need to verify the equation 

# SIR model with vital dynamic
SIR_VD_equation <- c(r"(    &\frac{dR}{dt} = \gamma I - \mu_D {R}\\)", 
                     r"(    &R_0 =  \frac{\beta}{\gamma} S(0)^q\\)") #Need to verify the equation 

#-----------------------------------------------------------------------------------------------------
#SIRS PMA with no vital dynamic
SIRS_PMA_nonVD <- c(r"(    &\frac{dS}{dt} = - \beta{ S I} + \xi{R}\\)",
                    r"(    &\frac{dI}{dt} = {\beta S I} - \gamma {I}\\)")
#SIRS PMA with vital dynamic
SIRS_PMA_VD <- c(r"(    &\frac{dS}{dt} = \mu{N}- \beta { S I} + \xi{R} - \nu{S}\\)",  
                r"(    &\frac{dI}{dt} = {\beta S I} - \gamma {I} - \nu{I}\\)")
#SIRS TMA with no vital dynamic
SIRS_TMA_nonVD <- c(r"(    &\frac{dS}{dt} = - \beta \frac{ S I}{N} + \xi{R}\\)",
                   r"(    &\frac{dI}{dt} = \frac{\beta S I}{N} - \gamma {I}\\)")
#SIR TMA with no vital dynamic
SIRS_TMA_VD <- c(r"(    &\frac{dS}{dt} = \mu{N}- \beta \frac{ S I}{N} + \xi{R} - \nu{S}\\)",  
                r"(    &\frac{dI}{dt} = \frac{\beta S I}{N} - \gamma {I} - \nu{I}\\)")
# SIRS with no vital dynamic
SIRS_nonVD_equation <- c(r"(    &\frac{dR}{dt} = \gamma {I} - \xi{R}\\)", 
                         r"(    &R_0 =  \frac{\beta}{\gamma} S(0)^q\\)") #Need to verify the equation 
# SIRS with vital dynamic
SIRS_VD_equation <- c(r"(    &\frac{dR}{dt} = \gamma {I} - \xi{R} - \nu{R}\\)", 
                       r"(    &R_0 =  \frac{\beta}{\gamma} S(0)^q\\)") #Need to verify the equation 

#-----------------------------------------------------------------------------------------------------
#SIRD PMA with no vital dynamic
SIRD_PMA_nonVD <- c(r"(    &\frac{dS}{dt} = - \beta { S I}\\)", 
                    r"(    &\frac{dI}{dt} = {\beta S I} - \gamma {I} - \delta {I}\\)")
#SIRD PMA with vital dynamic
SIRD_PMA_VD <- c(r"(    &\frac{dS}{dt} =\mu_B {N} - \mu_D {S} - \beta { S I}\\)",  
                 r"(    &\frac{dI}{dt} = {\beta S I} - \gamma {I} - \delta {I} - \mu_D {I}\\)")
#SIRS TMA with no vital dynamic
SIRD_TMA_nonVD <- c( r"(    &\frac{dS}{dt} = - \beta \frac{ S I}{N^q}\\)", 
                     r"(    &\frac{dI}{dt} = \frac{\beta S I}{N^q} - \gamma {I} - \delta {I}\\)")
#SIR TMA with no vital dynamic
SIRD_TMA_VD <- c(r"(    &\frac{dS}{dt} =\mu_B {N} - \mu_D {S} - \beta \frac{ S I}{N^q}\\)",  
                 r"(    &\frac{dI}{dt} = \frac{\beta S I}{N^q} - \gamma {I} - \delta {I} - \mu_D {I}\\)")
# SIRD model with no vital dynamic
SIRD_nonVD_equation <- c(r"(    &\frac{dR}{dt} = \gamma {I}\\)", 
                         r"(    &\frac{dD}{dt} = \delta {I}\\)", 
                         r"(    &R_0 =  \frac{\beta}{\gamma} S(0)^q\\)") #Need to verify the equation 
# SIRD model with vital dynamic
SIRD_VD_equation <- c(r"(    &\frac{dR}{dt} = \gamma {I} - \mu_D {R}\\)",
                      r"(    &\frac{dD}{dt} = \delta {I}\\)", 
                      r"(    &R_0 =  \frac{\beta}{\gamma} S(0)^q\\)") #Need to verify the equation 

#-----------------------------------------------------------------------------------------------------
#SEIR PMA with no vital dynamic
SEIR_PMA_nonVD <- c(r"(    &\frac{dS}{dt} = - \beta { S I}\\)", 
                    r"(    &\frac{dE}{dt} = \beta { S I} - \gamma {E}\\)")
#SEIR PMA with vital dynamic
SEIR_PMA_VD <- c(r"(    &\frac{dS}{dt} = \mu_B {N} - \mu_D {S} - \beta { S I}\\)", 
                 r"(    &\frac{dE}{dt} = \beta { S I} - \gamma {E} - \mu_D {E}\\)")
#SEIR TMA with no vital dynamic
SEIR_TMA_nonVD <- c(r"(    &\frac{dS}{dt} = - \beta \frac{ S I}{N^q}\\)", 
                    r"(    &\frac{dE}{dt} = \beta \frac{ S I}{N^q} - \gamma {E}\\)")
#SEIR TMA with no vital dynamic
SEIR_TMA_VD <- c(r"(    &\frac{dS}{dt} = \mu_B {N} - \mu_D {S} - \beta \frac{ S I}{N^q}\\)", 
                 r"(    &\frac{dE}{dt} = \beta \frac{ S I}{N^q} - \gamma {E} - \mu_D {E}\\)")
# SEIR model with no vital dynamic
SEIR_nonVD_equation <- c(r"(    &\frac{dI}{dt} = \gamma {E} - \sigma {I}\\)", 
                         r"(    &\frac{dR}{dt} = \sigma {I}\\)", 
                         r"(    &R_0 =  \frac{\beta}{\gamma} S(0)^q\\)") #Need to verify the equation 
# SEIR model with vital dynamic
SEIR_VD_equation <- c(r"(    &\frac{dI}{dt} = \gamma {E} - \sigma {I} - \mu_D {I}\\)", 
                      r"(    &\frac{dR}{dt} = \sigma {I}) - \mu_D {R}\\)",  
                      r"(    &R_0 =  \frac{\beta}{\gamma} S(0)^q\\)") #Need to verify the equation 

#-----------------------------------------------------------------------------------------------------
#SEIRD PMA with no vital dynamic
SEIRD_PMA_nonVD <- c(r"(    &\frac{dS}{dt} = - \beta { S I}\\)", 
                     r"(    &\frac{dE}{dt} = \beta { S I} - \gamma {E}\\)")
#SEIRD PMA with vital dynamic
SEIRD_PMA_VD <- c(r"(    &\frac{dS}{dt} = \mu_B {N} - \mu_D {S} - \beta { S I}\\)",
                  r"(    &\frac{dE}{dt} = \beta { S I} - \gamma {E} - \mu_D {E}\\)")
#SEIRD TMA with no vital dynamic
SEIRD_TMA_nonVD <- c(r"(    &\frac{dS}{dt} = - \beta \frac{ S I}{N^q}\\)", 
                     r"(    &\frac{dE}{dt} = \beta \frac{ S I}{N^q} - \gamma {E}\\)")
#SEIRD TMA with no vital dynamic
SEIRD_TMA_VD <- c(r"(    &\frac{dS}{dt} = \mu_B {N} - \mu_D {S} - \beta \frac{ S I}{N^q}\\)",
                  r"(    &\frac{dE}{dt} = \beta \frac{ S I}{N^q} - \gamma {E} - \mu_D {E}\\)")
# SEIRD model with no vital dynamic
SEIRD_nonVD_equation <- c(r"(    &\frac{dI}{dt} = \gamma {E} - \sigma {I} - \delta {I}\\)",
                          r"(    &\frac{dR}{dt} = \sigma {I}\\)",  
                          r"(    &\frac{dD}{dt} = \delta {I}\\)",
                          r"(    &R_0 =  \frac{\beta}{\gamma} S(0)^q\\)") #Need to verify the equation 
# SEIRD model with vital dynamic
SEIRD_VD_equation <- c(r"(    &\frac{dI}{dt} = \gamma {E} - \sigma {I} -  \delta {I} - \mu_D {I}\\)", 
                       r"(    &\frac{dR}{dt} = \sigma {I}) - \mu_D {R}\\)", 
                       r"(    &\frac{dD}{dt} = \delta {I}\\)", 
                       r"(    &R_0 =  \frac{\beta}{\gamma} S(0)^q\\)") #Need to verify the equation 