library(shiny)
library(shinyjs)
library(shinyhelper)
library(shinyWidgets)
library(deSolve)
library(ggplot2)
library(tidyverse)
server <- function(input, output,session) {
#  iv <- InputValidator$new()
  
#  iv$add_rule("alpha", sv_required())
#  iv$add_rule("alpha", sv_gte(0))
  
#  iv$add_rule("beta", sv_required())
#  iv$add_rule("beta", sv_gte(0))
  
#  iv$add_rule("gamma", sv_required())
#  iv$add_rule("gamma", sv_gte(0))
  
#  iv$add_rule("sigma", sv_required())
#  iv$add_rule("sigma", sv_gte(0))
  
#  iv$add_rule("delta", sv_required())
#  iv$add_rule("delta", sv_gte(0))
  
#  iv$add_rule("lambda", sv_required())
#  iv$add_rule("lambda", sv_gte(0))
  
#  iv$add_rule("date", sv_required())
  
#  iv$add_rule("timestep", sv_required())
#  iv$add_rule("timestep", sv_integer())
#  iv$add_rule("timestep", sv_gt(0))
  
#  iv$enable()
     
     #Reset vital dynamics when not checked off
     observe({
          input$muValue
          updateSliderInput(session, "muBirth", value =0)
     })
     observe({
          input$muValue
          updateSliderInput(session, "muDeath", value =0)
     })
     
     observeEvent(input$qValue,{
          # SIR - TMA
          if((input$qValue == "1")&&(input$modelSelect == "SIR"))
          {
               updateSliderInput(session, "betaSIR", value = 0.4)
               updateSliderInput(session, "gammaSIR", value = 0.04)
               updateNumericInput(session, "populationSIR", value = 1000)
               updateNumericInput(session, "susceptibleSIR", value = 997)
               updateNumericInput(session, "infectedSIR", value = 3)
               updateNumericInput(session, "recoveredSIR", value = 0)
               updateNumericInput(session, "timesteps", value = 25)
          }
          # SIR - SMA
          if((input$qValue == "0")&&(input$modelSelect == "SIR"))
          {
               updateSliderInput(session, "betaSIR", value = 0.001)
               updateSliderInput(session, "gammaSIR", value = 0.1)
               updateNumericInput(session, "populationSIR", value = 500)
               updateNumericInput(session, "susceptibleSIR", value = 499)
               updateNumericInput(session, "infectedSIR", value = 1)
               updateNumericInput(session, "recoveredSIR", value = 0)
               updateNumericInput(session, "timesteps", value = 50)
          }
          # SIRD - TMA
          if((input$qValue == "1")&&(input$modelSelect == "SIRD"))
          {
               updateSliderInput(session, "betaSIRD", value = 0.4)
               updateSliderInput(session, "gammaSIRD", value = 0.04)
               updateSliderInput(session, "deltaSIRD", value = 0)
               updateNumericInput(session, "populationSIRD", value = 1000)
               updateNumericInput(session, "susceptibleSIRD", value = 997)
               updateNumericInput(session, "infectedSIRD", value = 3)
               updateNumericInput(session, "recoveredSIRD", value = 0)
               updateNumericInput(session, "timesteps", value = 25)
          }
          #SIRD - PMA
          if((input$qValue == "0")&&(input$modelSelect == "SIRD"))
          {
               updateSliderInput(session, "betaSIRD", value = 0.001)
               updateSliderInput(session, "gammaSIRD", value = 0.1)
               updateSliderInput(session, "deltaSIRD", value = 0)
               updateNumericInput(session, "populationSIRD", value = 500)
               updateNumericInput(session, "susceptibleSIRD", value = 499)
               updateNumericInput(session, "infectedSIRD", value = 1)
               updateNumericInput(session, "recoveredSIRD", value = 0)
               updateNumericInput(session, "timesteps", value = 50)
          }
          #SEIR - TMA
          if((input$qValue == "1")&&(input$modelSelect == "SEIR"))
          {
               updateSliderInput(session, "beta", value = 0.35)
               updateSliderInput(session, "gamma", value = 0.1429)
               updateSliderInput(session, "sigma", value = 0.0476)
               updateNumericInput(session, "population", value = 500)
               updateNumericInput(session, "susceptible", value = 499)
               updateNumericInput(session, "exposed", value = 0)
               updateNumericInput(session, "infected", value = 1)
               updateNumericInput(session, "recovered", value = 0)
               updateNumericInput(session, "timesteps", value = 20)
          }
          # SEIR - PMA
          if((input$qValue == "0")&&(input$modelSelect == "SEIR"))
          {
               updateSliderInput(session, "beta", value = 0.5)
               updateSliderInput(session, "gamma", value = 0.5)
               updateSliderInput(session, "sigma", value = 0.1)
               updateNumericInput(session, "population", value = 53)
               updateNumericInput(session, "susceptible", value = 50)
               updateNumericInput(session, "exposed", value = 3)
               updateNumericInput(session, "infected", value = 0)
               updateNumericInput(session, "recovered", value = 0)
               updateNumericInput(session, "timesteps", value = 25)
          }
          #SEIRD - TMA
          if((input$qValue == "1")&&(input$modelSelect == "SEIRD"))
          {
               updateSliderInput(session, "betaSEIRD", value = 0.35)
               updateSliderInput(session, "gammaSEIRD", value = 0.1429)
               updateSliderInput(session, "sigmaSEIRD", value = 0.0476)
               updateSliderInput(session, "deltaSEIRD", value = 0)
               updateNumericInput(session, "populationSEIRD", value = 500)
               updateNumericInput(session, "susceptibleSEIRD", value = 499)
               updateNumericInput(session, "exposedSEIRD", value = 0)
               updateNumericInput(session, "infectedSEIRD", value = 1)
               updateNumericInput(session, "recoveredSEIRD", value = 0)
               updateNumericInput(session, "timesteps", value = 20)
          }
          #SEIRD - PMA
          if((input$qValue == "0")&&(input$modelSelect == "SEIRD"))
          {
               updateSliderInput(session, "betaSEIRD", value = 0.5)
               updateSliderInput(session, "gammaSEIRD", value = 0.5)
               updateSliderInput(session, "sigmaSEIRD", value = 0.1)
               updateSliderInput(session, "deltaSEIRD", value = 0)
               updateNumericInput(session, "populationSEIRD", value = 53)
               updateNumericInput(session, "susceptibleSEIRD", value = 50)
               updateNumericInput(session, "exposedSEIRD", value = 3)
               updateNumericInput(session, "infectedSEIRD", value = 0)
               updateNumericInput(session, "recoveredSEIRD", value = 0)
               updateNumericInput(session, "timesteps", value = 50)
          }
       if(((input$qValue == "1")||(input$qValue == "0"))&&(input$modelSelect == "SIR-Stochastic"))
          {
         updateNumericInput(session, "populationSIR_Stoc", value = 50)
         updateNumericInput(session, "timesteps", value = 10)
       }
     })
     
     
     
     #############################################
     #####      PLOT - SIR-Stochastic        #####
     #############################################
     
     
     
     output$plotSIR_Stoc <- renderPlot({
       input$go
       isolate({
       validate(
         need(input$populationSIR_Stoc > 0, "Total Population (N) must be greather than 0."),
         need(input$susceptibleSIR_Stoc > 0, "Susceptible (S) must be greater than 0."),
         need(input$infectedSIR_Stoc >= 0, "Infected (I) must be greater than 0."),
         need(input$recoveredSIR_Stoc >= 0, label = "Recovered (R)"),
         need(input$populationSIR_Stoc == (input$susceptibleSIR_Stoc + input$infectedSIR_Stoc + input$recoveredSIR_Stoc), "All inputs must equal to Total Population.")
       )
          
          num.sims <- input$stochasticSIR                               
          N <- input$populationSIR_Stoc                                    
          vacc.prop <- 0                         
          init.immune <- input$recoveredSIR_Stoc
          init.infect <- input$infectedSIR_Stoc
          R0 <- 3.5                                    
          infper <- 1                                  # infectious period in days
          ## latper <- 0                               # latent period in days
          ### R0 = beta*N/(1/infper) = beta*N*infper
          ### beta = R0/(N*infper)
          beta <- R0/(1000)                        # transmission coefficient
          step <- 0.1                                  # time steps (in days)
          timeseq <- seq(0, 10, by = step)             # discrete time intervals to simulate
          
          plot(0,0, type ="n", xlim = c(min(timeseq), max(timeseq)),
               ylim = c(0, N+10), bty = "n", xlab = "time (days)", ylab = "# hosts", lwd = 1)
          
          cum.incidence <- rep(init.infect,num.sims)
          
          for(ss in 1:num.sims)              # do num.sims outbreaks
          {
               sim <- data.frame(S = N - init.infect - init.immune, I = init.infect, R = init.immune)
               
               for(ii in 2:length(timeseq))                        # run through time series
               {
                    p.trans <- 1 - exp(-beta*step*sim$I[ii-1])     # probability of S -> I per unit S
                    new.inf <- rbinom(1, sim$S[ii-1], p.trans)     # number new infections
                    
                    p.recov <- 1 - exp(-1/infper*step)             # probability of I -> R
                    new.recov <- rbinom(1, sim$I[ii-1], p.recov)   # number new recoveries
                    
                    temp.S <- sim$S[ii-1] - new.inf
                    temp.I <- sim$I[ii-1] + new.inf - new.recov
                    temp.R <- sim$R[ii-1] + new.recov
                    
                    sim <- rbind(sim, c(temp.S, temp.I, temp.R))
                    cum.incidence[ss] <- cum.incidence[ss] + new.inf
               }
               lines(timeseq, sim[,"S"],col = "blue")
               lines(timeseq, sim[,"I"],col = "red")
               lines(timeseq, sim[,"R"],col = "green")
          }
          
       })
          
     })
     
     #############################################
     #####            PLOT - SIR             #####
     #############################################
     sir_equations <- function(time, variables, parameters) {
          S <- variables[1]
          I <- variables[2]
          R <- variables[3]
          N <- variables[4]
          q <- variables[5]
          dS <-  (input$muBirth * N) - (input$muDeath * S) - (input$betaSIR * ((S * I) / (N ^ q)))
          dI <-  (input$betaSIR *((S * I) / (N ^ q))) - (input$gammaSIR * I) - (input$muDeath * I)
          dR <-  (input$gammaSIR * I) - (input$muDeath * R)
          dN <- dS + dI + dR
          list(c(dS, dI, dR, dN, q))
     }
     
     sir_values <- reactive({
       
          req(input$timesteps, input$betaSIR, input$gammaSIR, input$muBirth, input$muDeath)
       

       validate(
         need(input$populationSIR > 0, "Total Population (N) must be greather than 0."),
          need(input$susceptibleSIR > 0, "Susceptible (S) must be greater than 0."),
          need(input$infectedSIR >= 0, "Infected (I) must be greater than 0."),
          need(input$recoveredSIR >= 0, label = "Recovered (R)"),
          #need(input$timesteps > 0 , "Timesteps must be greater than 0."),
         need(input$populationSIR == (input$susceptibleSIR + input$infectedSIR + input$recoveredSIR), "All inputs must equal to Total Population.")
       )
          ode(
               y = c(
                    S = input$susceptibleSIR,
                    I = input$infectedSIR,
                    R = input$recoveredSIR,
                    N = input$populationSIR,
                    q = as.integer(input$qValue)
               ),
               times = seq(0, input$timesteps, by = 1),
               func = sir_equations,
               parms = c(
                    beta = input$betaSIR,
                    gamma = input$gammaSIR,
                    muB = input$muBirth,
                    muD = input$muDeath
               )
               
          )
     })
     
     
     output$plotSIR <- renderPlot({
       input$go
       isolate({
         val <- as.data.frame(sir_values())
          ggplot(val, aes(x = time)) +
               theme(axis.line = element_line(color="black"), axis.text=element_text(size=14),
                     axis.title.x = element_text(size=16,face="bold"), 
                     axis.title.y = element_text(size=16,face="bold")) +
               ggtitle("SIR Epidemic Model") +
               theme(plot.title = element_text(size = 22, face="bold")) +
               theme(legend.position="bottom") +
               ylab("Number of People") +
               xlab("Time") +
               scale_x_continuous(expand = c(0, 0)) + 
               scale_y_continuous(expand = c(0, 0)) +
               geom_line(aes(y = S, color = "Blue"), size = 1.5) +
               geom_line(aes(y = I, color = "Red"), size = 1.5) +
               geom_line(aes(y = R, color = "Green"), size = 1.5) +
               scale_color_identity(name= "SIR", breaks = c("Blue", "Red", "Green"), 
                                    labels = c("Susceptible", "Infected", "Recovered"), guide = "legend")
          })
         })
     
     output$SIRPhasePlane <- renderPlot({
       input$go
       isolate({
          val <- as.data.frame(sir_values())
          ggplot(val, aes(x = S)) +
               geom_line(aes(y = I, color = "Blue"), size = 1.5) +
               theme(axis.line = element_line(color="black"), axis.text=element_text(size=14),
                     axis.title.x = element_text(size=16,face="bold"), 
                     axis.title.y = element_text(size=16,face="bold")) +
               theme(legend.position="bottom") +
               ggtitle("SI Phase Plane") +
               theme(plot.title = element_text(size = 22,face="bold")) +
               ylab("Infected (I)") +
               scale_x_continuous(expand = c(0, 0)) + 
               scale_y_continuous(expand = c(0, 0)) +
               xlab("Susceptible (S)")+
               scale_color_identity(breaks = "Blue", 
                                    labels = "Susceptible")
       })
     })
     
     
     output$tableSIR <- renderTable({
       input$go
       isolate({
          val <- as.data.frame(sir_values())
          val <- val[-c(6)]
          return(val)
       })
     })
     
     #############################################
     #####            PLOT - SIRD            #####
     #############################################
     sird_equations <- function(time, variables, parameters) {
          S <- variables[1]
          I <- variables[2]
          R <- variables[3]
          D <- variables[4]
          N <- variables[5]
          q <- variables[6]
          dS <- (input$muBirth * N) - (input$muDeath * S) - (input$betaSIRD * ((S * I) / (N ^ q)))
          dI <- (input$betaSIRD *((S * I) / (N ^ q))) - (input$gammaSIRD * I) - (input$deltaSIRD * I) - (input$muDeath * I)
          dR <- (input$gammaSIRD * I) - (input$muDeath * R)
          dD <- (input$deltaSIRD * I)
          dN <- dS + dI + dR
          list(c(dS, dI, dR, dD, dN, q))
     }
     
     sird_values <- reactive({
          req(input$timesteps, input$betaSIRD, input$gammaSIRD, input$deltaSIRD,input$muBirth, input$muDeath)
       
       validate(
         need(input$populationSIRD > 0, "Total Population (N) must be greater than 0."),
         need(input$susceptibleSIRD > 0, "Susceptible (S) must be greater than 0."),
         need(input$infectedSIRD >= 0, "Infected (I) must be greater than 0."),
         need(input$recoveredSIRD >= 0, label = "Recovered (R)"),
         need(input$deadSIRD >= 0, label = "Dead (D)"),
         #need(input$timesteps > 0, "Timesteps must be greater than 0."),
         need(input$populationSIRD == (input$susceptibleSIRD + input$infectedSIRD + input$recoveredSIRD), "All inputs must equal to Total Population.")
       )
       
          ode(
               y = c(
                    S = input$susceptibleSIRD,
                    I = input$infectedSIRD,
                    R = input$recoveredSIRD,
                    D = input$deadSIRD,
                    N = input$populationSIRD,
                    q = as.integer(input$qValue)
               ),
               times = seq(0, input$timesteps, by = 1),
               func = sird_equations,
               parms = c(
                    beta = input$betaSIRD,
                    gamma = input$gammaSIRD,
                    delta = input$deltaSIRD,
                    muB = input$muBirth,
                    muD = input$muDeath
               )
          )
     })
     
     output$plotSIRD <- renderPlot({
       input$go
       isolate({
          val <- as.data.frame(sird_values())
          ggplot(val, aes(x = time)) +
               theme(axis.line = element_line(color="black"), axis.text=element_text(size=14),
                     axis.title.x = element_text(size=16,face="bold"), 
                     axis.title.y = element_text(size=16,face="bold")) +
               ggtitle("SIRD Epidemic Model") +
               theme(plot.title = element_text(size = 22,face="bold")) +
               theme(legend.position="bottom") +
               ylab("Number of People") +
               xlab("Time") +
               scale_x_continuous(expand = c(0, 0)) + 
               scale_y_continuous(expand = c(0, 0)) +
               geom_line(aes(y = S, color = "Blue"), size = 1.5) +
               geom_line(aes(y = I, color = "Red"), size = 1.5) +
               geom_line(aes(y = R, color = "Green"), size = 1.5) +
               geom_line(aes(y = D, color = "Orange"), size = 1.5) +
               scale_color_identity(name= "SIRD", breaks = c("Blue", "Red", "Green", "Orange"), 
                                    labels = c("Susceptible", "Infected", "Recovered", "Dead"), guide = "legend")
          
       })
          })
     
     output$SIRDPhasePlane <- renderPlot({
       input$go
       isolate({
          val <- as.data.frame(sird_values())
          ggplot(val, aes(x = S)) +
               geom_line(aes(y = I, color = "Blue"), size = 1.5) +
               theme(axis.line = element_line(color="black"), axis.text=element_text(size=14),
                     axis.title.x = element_text(size=16,face="bold"), 
                     axis.title.y = element_text(size=16,face="bold")) +
               theme(legend.position="bottom") +
               ggtitle("SI Phase Plane") +
               theme(plot.title = element_text(size = 22,face = "bold")) +
               ylab("Infected (I)") +
               scale_x_continuous(expand = c(0, 0)) + 
               scale_y_continuous(expand = c(0, 0)) +
               xlab("Susceptible (S)")+
               scale_color_identity(breaks = "Blue", 
                                    labels = "Susceptible")
       })
     })
     
     output$tableSIRD <- renderTable({
       input$go
       isolate({
          val <- as.data.frame(sird_values())
          val <- val[-c(7)]
          return(val)
       })
     })
     
     #############################################
     #####            PLOT - SEIR            #####
     #############################################
     seir_equations <- function(time, variables, parameters) {
          S <- variables[1]
          E <- variables[2]
          I <- variables[3]
          R <- variables[4]
          N <- variables[5]
          q <- variables[6]
          dS <-  (input$muBirth * N) - (input$muDeath * S) - (input$beta * ((S * I) / (N ^ q)))
          dE <-  (input$beta * ((S * I) / (N ^ q))) - (input$gamma * E) -(input$muDeath * E)
          dI <-  (input$gamma * E) - (I * input$sigma) - (input$muDeath * I)
          dR <-  (I * input$sigma) - (input$muDeath * R)
          dN <- dS + dE + dI + dR
          list(c(dS, dE, dI, dR, dN, q))
     }
     
     seir_values <- reactive({
          req(input$timesteps, input$beta, input$gamma,input$muBirth, input$muDeath)
  
       validate(
         need(input$population > 0, "Total Population (N) must be greater than 0."),
         need(input$exposed >= 0, label = "Exposed (E)"),
         need(input$susceptible > 0, "Susceptible (S) must be greater than 0."),
         need(input$infected >= 0, "Infected (I) must be greater than 0."),
         need(input$recovered >= 0, label = "Recovered (R)"),
         #need(input$timesteps > 0, "Timesteps must be greater than 0."),
         need(input$population == (input$exposed + input$susceptible + input$infected + input$recovered), "All inputs must equal to Total Population.")
       )
       
          ode(
               y = c(
                    S = input$susceptible,
                    E = input$exposed,
                    I = input$infected,
                    R = input$recovered,
                    N = input$population,
                    q = as.integer(input$qValue)
               ),
               times = seq(0, input$timesteps, by = 1),
               func = seir_equations,
               parms = c(
                    beta = input$beta,
                    gamma = input$gamma,
                    muB = input$muBirth,
                    muD = input$muDeath
               )
               
          )
     })
     
     output$plotSEIR <- renderPlot({
       input$go
       isolate({
          val <- as.data.frame(seir_values())
          
          ggplot(val, aes(x = time)) +
               ggtitle("SEIR Epidemic Model") +
               theme(axis.line = element_line(color="black"),
                     axis.title.x = element_text(size=16,face="bold"), 
                     axis.title.y = element_text(size=16,face="bold")) +
               theme(legend.position="bottom") +
               theme(plot.title = element_text(size = 22,face = "bold"), axis.text=element_text(size=14)) +
               ylab("Number of People") +
               xlab("Time") +
               scale_x_continuous(expand = c(0, 0)) + 
               scale_y_continuous(expand = c(0, 0)) +
               geom_line(aes(x = time, y = S, color = "Blue"), size = 1.5) +
               geom_line(aes(x = time, y = E, color = "Brown"), size = 1.5) +
               geom_line(aes(x = time, y = I, color = "Red"), size = 1.5) +
               geom_line(aes(x = time, y = R, color = "Green"), size = 1.5)+
               scale_color_identity(name= "SEIR", breaks = c("Blue", "Brown","Red", "Green"), 
                                    labels = c("Susceptible", "Exposed","Infected", "Recovered"), guide = "legend")
          
       })
          })
     
     output$SEIRPhasePlane <- renderPlot({
       input$go
       isolate({
          val <- as.data.frame(seir_values())
          ggplot(val, aes(x = S)) +
               geom_line(aes(y = I, color = "Blue"), size = 1.5) +
               theme(axis.line = element_line(color="black"), axis.text=element_text(size=14),
                     axis.title.x = element_text(size=16,face="bold"), 
                     axis.title.y = element_text(size=16,face="bold")) +
               ggtitle("SEIR Phase Plane") +
               theme(plot.title = element_text(size = 22, face="bold")) +
               theme(legend.position="bottom") +
               ylab("Infected (I)") +
               xlab("Susceptible (S)")+
               scale_x_continuous(expand = c(0, 0)) + 
               scale_y_continuous(expand = c(0, 0)) +
               scale_color_identity(breaks = "Blue", 
                                    labels = "Susceptible")
       })
     })
     
     output$tableSEIR <- renderTable({
       input$go
       isolate({
          valSEIR <- as.data.frame(seir_values())
          valSEIR <- valSEIR[-c(7)]
          return(valSEIR)
          
       })
     })
     
     #############################################
     #####            PLOT - SEIRD          #####
     #############################################
     seird_equations <- function(time, variables, parameters) {
          S <- variables[1]
          E <- variables[2]
          I <- variables[3]
          R <- variables[4]
          D <- variables[5]
          N <- variables[6]
          q <- variables[7]
          dS <- (input$muBirth * N) - (input$muDeath * S) - (input$betaSEIRD * ((S * I) / (N ^ q)))
          dE <- (input$beta * ((S * I) / (N ^ q))) - (input$gammaSEIRD * E) -(input$muDeath * E)
          dI <- (input$gammaSEIRD * E) - (I * input$sigmaSEIRD) - (input$deltaSEIRD * I) - (input$muDeath * I)
          dR <- (I * input$sigma) - (input$muDeath * R)
          dD <- (input$deltaSEIRD * I)
          dN <- dS + dE + dI + dR
          list(c(dS, dE, dI, dR, dD, dN, q))
     }
     
     seird_values <- reactive({
          req(input$timesteps, input$betaSEIRD, input$gammaSEIRD,input$muBirth, input$muDeath)
       
       validate(
         need(input$populationSEIRD > 0, "Total Population (N) must be greater than 0."),
         need(input$exposedSEIRD >= 0, label = "Exposed (E)"),
         need(input$susceptibleSEIRD > 0, "Susceptible (S) must be greater than 0."),
         need(input$infectedSEIRD >= 0, "Infected (I) must be greater than 0."),
         need(input$recoveredSEIRD >= 0, label = "Recovered (R)"),
         need(input$deadSEIRD >= 0, label = "Dead (D)"),
         #need(input$timesteps > 0, "Timesteps must be greater than 0."),
         need(input$populationSEIRD == (input$exposedSEIRD + input$susceptibleSEIRD + input$infectedSEIRD + input$recoveredSEIRD), "All inputs must equal to Total Population.")
       )
       
          ode(
               y = c(
                    S = input$susceptibleSEIRD,
                    E = input$exposedSEIRD,
                    I = input$infectedSEIRD,
                    R = input$recoveredSEIRD,
                    D = input$deadSEIRD,
                    N = input$populationSEIRD,
                    q = as.integer(input$qValue)
               ),
               times = seq(0, input$timesteps, by = 1),
               func = seird_equations,
               parms = c(
                    beta = input$betaSEIRD,
                    gamma = input$gammaSEIRD,
                    sigma = input$sigmaSEIRD,
                    delta = input$deltaSEIRD,
                    muB = input$muBirth,
                    muD = input$muDeath
               )
          )
     })
     
     output$plotSEIRD <- renderPlot({
       input$go
       isolate({
          val <- as.data.frame(seird_values())
          ggplot(val, aes(x = time)) +
               ggtitle("SEIRD Epidemic Model") +
               theme(axis.line = element_line(color="black"), axis.text=element_text(size=14),
                     axis.title.x = element_text(size=16,face="bold"), 
                     axis.title.y = element_text(size=16,face="bold")) +
               theme(plot.title = element_text(size = 22, face= "bold")) +
               theme(legend.position="bottom") +
               ylab("Number of People") +
               xlab("Time") +
               scale_x_continuous(expand = c(0, 0)) + 
               scale_y_continuous(expand = c(0, 0)) +
               geom_line(aes(x = time, y = S, color = "Blue"), size = 1.5) +
               geom_line(aes(x = time, y = E, color = "Brown"), size = 1.5) +
               geom_line(aes(x = time, y = I, color = "Red"), size = 1.5) +
               geom_line(aes(x = time, y = R, color = "Green"), size = 1.5)+
               geom_line(aes(x = time, y = D, color = "Orange"), size = 1.5)+
               scale_color_identity(name= "SEIRD", breaks = c("Blue", "Brown","Red", "Green", "Orange"), 
                                    labels = c("Susceptible", "Exposed","Infected", "Recovered", "Dead"), guide = "legend")
          
       })
          })
     
     output$SEIRDPhasePlane <- renderPlot({
       input$go
       isolate({
          val <- as.data.frame(seird_values())
          ggplot(val, aes(x = S)) +
               geom_line(aes(y = I, color = "Blue"), size = 1.5) +
               theme(axis.line = element_line(color="black"), axis.text=element_text(size=14),
                     axis.title.x = element_text(size=16,face="bold"), 
                     axis.title.y = element_text(size=16,face="bold")) +
               theme(legend.position="bottom") +
               ggtitle("SEIRD Phase Plane") +
               theme(plot.title = element_text(size = 22, face="bold")) +
               ylab("Infected (I)") +
               xlab("Susceptible (S)")+
               scale_x_continuous(expand = c(0, 0)) + 
               scale_y_continuous(expand = c(0, 0)) +
               scale_color_identity(breaks = "Blue", 
                                    labels = "Susceptible")
       })
     })
     
     output$tableSEIRD <- renderTable({
       input$go
       isolate({
          valSEIRD <- as.data.frame(seird_values())
          valSEIRD <- valSEIRD[-c(8)]
          return(valSEIRD)
       })
     })
     
     observe(
       hideTab(inputId = 'tabSet', target = 'Plot')
       )
     observe(
       hideTab(inputId = 'tabSet', target = 'Phase Plane')
       )
     observe(
       hideTab(inputId = 'tabSet', target = 'Output Summary')
       )
     observe(
       hideTab(inputId = 'tabSet', target = 'Mathematical Model')
       )
     observe(
       {
       hide(id = "qValue")
       hide(id = "muValue")
       hide(id = "timesteps")
     }
     )
     observe({
       toggle(id = "qValue", condition = (input$modelSelect == "SIR" || input$modelSelect == "SIRD" || input$modelSelect == "SEIR" || input$modelSelect == "SEIRD" || input$modelSelect == "SIR-Stochastic"))
       toggle(id = "muValue", condition = (input$modelSelect == "SIR" || input$modelSelect == "SIRD" || input$modelSelect == "SEIR" || input$modelSelect == "SEIRD" || input$modelSelect == "SIR-Stochastic"))
       toggle(id = "timesteps", condition = (input$modelSelect == "SIR" || input$modelSelect == "SIRD" || input$modelSelect == "SEIR" || input$modelSelect == "SEIRD" || input$modelSelect == "SIR-Stochastic"))
       
     })
     observeEvent(input$go,{
       showTab(inputId = 'tabSet', target = 'Plot')
       })
     observeEvent(input$go,{
       showTab(inputId = 'tabSet', target = 'Phase Plane')
       })
     observeEvent(input$go,{
       showTab(inputId = 'tabSet', target = 'Output Summary')
       })
     observeEvent(input$go,{
       showTab(inputId = 'tabSet', target = 'Mathematical Model')
       }) 
     observeEvent(input$resetAll,{
       hideTab(inputId = 'tabSet', target = 'Plot')
       })
       observeEvent(input$resetAll,{
       hideTab(inputId = 'tabSet', target = 'Phase Plane')
       })
       observeEvent(input$resetAll,{
         hideTab(inputId = 'tabSet', target = 'Output Summary')
           })
       observeEvent(input$resetAll,{
       hideTab(inputId = 'tabSet', target = 'Mathematical Model')
         })
       observeEvent(input$resetAll,{
         updateSliderInput(session, "betaSIR", value = 0)
         updateSliderInput(session, "gammaSIR", value = 0)
         updateNumericInput(session, "populationSIR", value = 0)
         updateNumericInput(session, "susceptibleSIR", value = 0)
         updateNumericInput(session, "infectedSIR", value = 0)
         updateNumericInput(session, "recoveredSIR", value = 0)
         updateNumericInput(session, "timesteps", value = 0)
       })
       observeEvent(input$resetAll,{
         updateSliderInput(session, "betaSIRD", value = 0)
         updateSliderInput(session, "gammaSIRD", value = 0)
         updateSliderInput(session, "deltaSIRD", value = 0)
         updateNumericInput(session, "populationSIRD", value = 0)
         updateNumericInput(session, "susceptibleSIRD", value = 0)
         updateNumericInput(session, "infectedSIRD", value = 0)
         updateNumericInput(session, "recoveredSIRD", value = 0)
         updateNumericInput(session, "timesteps", value = 0)
       })
       observeEvent(input$resetAll,{
         updateSliderInput(session, "beta", value = 0)
         updateSliderInput(session, "gamma", value = 0)
         updateSliderInput(session, "sigma", value = 0)
         updateNumericInput(session, "population", value = 0)
         updateNumericInput(session, "susceptible", value = 0)
         updateNumericInput(session, "exposed", value = 0)
         updateNumericInput(session, "infected", value = 0)
         updateNumericInput(session, "recovered", value = 0)
         updateNumericInput(session, "timesteps", value = 0)
       })
       observeEvent(input$resetAll,{
         updateSliderInput(session, "betaSEIRD", value = 0)
         updateSliderInput(session, "gammaSEIRD", value = 0)
         updateSliderInput(session, "sigmaSEIRD", value = 0)
         updateSliderInput(session, "deltaSEIRD", value = 0)
         updateNumericInput(session, "populationSEIRD", value = 0)
         updateNumericInput(session, "susceptibleSEIRD", value = 0)
         updateNumericInput(session, "exposedSEIRD", value = 0)
         updateNumericInput(session, "infectedSEIRD", value = 0)
         updateNumericInput(session, "recoveredSEIRD", value = 0)
         updateNumericInput(session, "timesteps", value = 0)
       })
       
}