`%ifModelSelectionUpdateInputs%` <- function(modelSelectionString, params) {
  if (input$modelSelect == modelSelectionString) {
    updateNumericInputsByNameAndValue(params)
  }
}

# MAYBE: Replace the following two function definitions with a function
# generator that will write functions to take a list with named components and
# apply a function using the names of the components (coerced to character) and
# the values, with any given function? Probably overkill, since I only need two.
updateNumericInputsByNameAndValue <- function(params) {
  mapply(params, params,
    USE.NAMES = TRUE,
    \(name, value) updateNumericInput(as.character(name), value)
  )
}

addInputValidationRules <- function(ruleList) {
  mapply(ruleList, ruleList, USE.NAMES = TRUE,
         \(name, value) iv$add_rule(as.character(name), value))
}

## # MAYBE DONE: this might work, but the use of `...`/dots is probably wrong.
## makeFunctionAppliedToNamesAndValuesOfList <- function(anon) {
##   \(pairlist) mapply(pairlist, pairlist, USE.NAMES = TRUE, anon)
## }
### EXAMPLE:
## updateNumericInputsByNameAndValue <-
##   makeFunctionAppliedToNamesAndValuesOfList(\(name, value) {
##     updateNumericInput(name, value)
##   })

server <- function(input, output, session) {
  ## Data validation
  iv <- InputValidator$new()

  addInputValidationRules(
    list(
      ## muValue
      muBirth = sv_required(),
      muBirth = sv_gte(0),
      muBirth = sv_lte(0.1),
      muDeath = sv_required(),
      muDeath = sv_gte(0),
      muDeath = sv_lte(0.1),

      ## SIR-Stochastic
      stochasticSIR = sv_required(),
      stochasticSIR = sv_integer(),
      stochasticSIR = sv_gt(0),
      stochasticSIR = sv_lte(100),
      betaSIR_Stoc = sv_required(),
      betaSIR_Stoc = sv_gte(0),
      betaSIR_Stoc = sv_lte(1),
      gammaSIR_Stoc = sv_required(),
      gammaSIR_Stoc = sv_gte(0),
      gammaSIR_Stoc = sv_lte(5),
      populationSIR_Stoc = sv_required(),
      populationSIR_Stoc = sv_gt(0),
      susceptibleSIR_Stoc = sv_required(),
      susceptibleSIR_Stoc = sv_gt(0),
      infectedSIR_Stoc = sv_required(),
      infectedSIR_Stoc = sv_gte(0),
      recoveredSIR_Stoc = sv_required(),
      recoveredSIR_Stoc = sv_gte(0),

      ## SIR
      betaSIR = sv_required(),
      betaSIR = sv_gte(0),
      betaSIR = sv_lte(1),
      gammaSIR = sv_required(),
      gammaSIR = sv_gte(0),
      gammaSIR = sv_lte(5),
      populationSIR = sv_required(),
      populationSIR = sv_gt(0),
      susceptibleSIR = sv_required(),
      susceptibleSIR = sv_gt(0),
      infectedSIR = sv_required(),
      infectedSIR = sv_gte(0),
      recoveredSIR = sv_required(),
      recoveredSIR = sv_gte(0),

      ## SIRD
      betaSIRD = sv_required(),
      betaSIRD = sv_gte(0),
      betaSIRD = sv_lte(0.5),
      gammaSIRD = sv_required(),
      gammaSIRD = sv_gte(0),
      gammaSIRD = sv_lte(0.5),
      deltaSIRD = sv_required(),
      deltaSIRD = sv_gte(0),
      deltaSIRD = sv_lte(0.5),
      populationSIRD = sv_required(),
      populationSIRD = sv_gt(0),
      susceptibleSIRD = sv_required(),
      susceptibleSIRD = sv_gt(0),
      infectedSIRD = sv_required(),
      infectedSIRD = sv_gte(0),
      recoveredSIRD = sv_required(),
      recoveredSIRD = sv_gte(0),
      deadSIRD = sv_required(),
      deadSIRD = sv_gte(0),

      ## SEIR,
      beta = sv_required(),
      beta = sv_gte(0),
      beta = sv_lte(1),
      gamma = sv_required(),
      gamma = sv_gte(0),
      gamma = sv_lte(3),
      sigma = sv_required(),
      sigma = sv_gte(0),
      sigma = sv_lte(0.5),
      population = sv_required(),
      population = sv_gt(0),
      susceptible = sv_required(),
      susceptible = sv_gt(0),
      exposed = sv_required(),
      exposed = sv_gte(0),
      infected = sv_required(),
      infected = sv_gte(0),
      recovered = sv_required(),
      recovered = sv_gte(0),

      ## SEIRD
      betaSEIRD = sv_required(),
      betaSEIRD = sv_gte(0),
      betaSEIRD = sv_lte(1),
      gammaSEIRD = sv_required(),
      gammaSEIRD = sv_gte(0),
      gammaSEIRD = sv_lte(3),
      sigmaSEIRD = sv_required(),
      sigmaSEIRD = sv_gte(0),
      sigmaSEIRD = sv_lte(0.5),
      deltaSEIRD = sv_required(),
      deltaSEIRD = sv_gte(0),
      deltaSEIRD = sv_lte(0.5),
      populationSEIRD = sv_required(),
      populationSEIRD = sv_gt(0),
      susceptibleSEIRD = sv_required(),
      susceptibleSEIRD = sv_gt(0),
      exposedSEIRD = sv_required(),
      exposedSEIRD = sv_gte(0),
      infectedSEIRD = sv_required(),
      infectedSEIRD = sv_gte(0),
      recoveredSEIRD = sv_required(),
      recoveredSEIRD = sv_gte(0),
      deadSEIRD = sv_required(),
      deadSEIRD = sv_gte(0),

      ## Timesteps
      timesteps = sv_required(),
      timesteps = sv_integer(),
      timesteps = sv_gt(0)
    )
  )

  iv$enable()

  ## Reset vital dynamics when not checked off
  observe({
    input$muValue
    updateNumericInput(session, "muBirth", value = 0)
  })

  observe({
    input$muValue
    updateNumericInput(session, "muDeath", value = 0)
  })

  observeEvent(input$qValue, {
    ## SIR - TMA
    if ((input$qValue == "1") && (input$modelSelect == "SIR")) {
      updateNumericInputsByNameAndValue(list(
        betaSIR = 0.4,
        gammaSIR = 0.04,
        populationSIR = 1000,
        susceptibleSIR = 997,
        infectedSIR = 3,
        recoveredSIR = 0,
        timesteps = 25
      ))
      updateCheckboxInput(session, "muValue", value = FALSE)
    }

    ## SIR - PMA
    if ((input$qValue == "0") && (input$modelSelect == "SIR")) {
      updateNumericInputsByNameAndValue(list(
        betaSIR = 0.001,
        gammaSIR = 0.1,
        populationSIR = 500,
        susceptibleSIR = 499,
        infectedSIR = 1,
        recoveredSIR = 0,
        timesteps = 50
      ))
      updateCheckboxInput(session, "muValue", value = FALSE)
    }

    # SIRD - TMA
    if ((input$qValue == "1") && (input$modelSelect == "SIRD")) {
      updateNumericInputsByNameAndValue(list(
        betaSIRD = 0.4,
        gammaSIRD = 0.04,
        deltaSIRD = 0.05,
        populationSIRD = 1000,
        susceptibleSIRD = 997,
        infectedSIRD = 3,
        recoveredSIRD = 0,
        timesteps = 25
      ))
      updateCheckboxInput(session, "muValue", value = FALSE)
    }

    # SIRD - PMA
    if ((input$qValue == "0") && (input$modelSelect == "SIRD")) {
      updateNumericInputsByNameAndValue(list(
        betaSIRD = 0.001,
        gammaSIRD = 0.1,
        deltaSIRD = 0.05,
        populationSIRD = 500,
        susceptibleSIRD = 499,
        infectedSIRD = 1,
        recoveredSIRD = 0,
        timesteps = 50
      ))
      updateCheckboxInput(session, "muValue", value = FALSE)
    }

    # SEIR - TMA
    if ((input$qValue == "1") && (input$modelSelect == "SEIR")) {
      updateNumericInputsByNameAndValue(list(
        beta = 0.35,
        gamma = 0.1429,
        sigma = 0.0476,
        population = 500,
        susceptible = 499,
        exposed = 0,
        infected = 1,
        recovered = 0,
        timesteps = 20
      ))
      updateCheckboxInput(session, "muValue", value = FALSE)
    }

    # SEIR - PMA
    if ((input$qValue == "0") && (input$modelSelect == "SEIR")) {
      updateNumericInputsByNameAndValue(list(
        beta = 0.5,
        gamma = 0.5,
        sigma = 0.1,
        population = 53,
        susceptible = 50,
        exposed = 3,
        infected = 0,
        recovered = 0,
        timesteps = 25
      ))
      updateCheckboxInput(session, "muValue", value = FALSE)
    }

    # SEIRD - TMA
    if ((input$qValue == "1") && (input$modelSelect == "SEIRD")) {
      updateNumericInputsByNameAndValue(list(
        betaSEIRD = 0.35,
        gammaSEIRD = 0.1429,
        sigmaSEIRD = 0.0476,
        deltaSEIRD = 0.05,
        populationSEIRD = 500,
        susceptibleSEIRD = 499,
        exposedSEIRD = 0,
        infectedSEIRD = 1,
        recoveredSEIRD = 0,
        timesteps = 20
      ))
      updateCheckboxInput(session, "muValue", value = FALSE)
    }

    # SEIRD - PMA
    if ((input$qValue == "0") && (input$modelSelect == "SEIRD")) {
      updateNumericInputsByNameAndValue(list(
        betaSEIRD = 0.5,
        gammaSEIRD = 0.5,
        sigmaSEIRD = 0.1,
        deltaSEIRD = 0.05,
        populationSEIRD = 53,
        susceptibleSEIRD = 50,
        exposedSEIRD = 3,
        infectedSEIRD = 0,
        recoveredSEIRD = 0,
        timesteps = 50
      ))
      updateCheckboxInput(session, "muValue", value = FALSE)
    }

    if (((input$qValue == "1") || (input$qValue == "0")) && (input$modelSelect == "SIR-Stochastic")) {
      updateNumericInputsByNameAndValue(list(
        stochasticSIR = 50,
        betaSIR_Stoc = 0.00178,
        gammaSIR_Stoc = 2.73,
        populationSIR_Stoc = 1000,
        susceptibleSIR_Stoc = 990,
        infectedSIR_Stoc = 10,
        recoveredSIR_Stoc = 0,
        timesteps = 10
      ))
      updateCheckboxInput(session, "muValue", value = FALSE)
    }
  })

  #############################################
  #####      PLOT - SIR-Stochastic        #####
  #############################################
  validationString <- "Enter a valid value for parameters"
  # TODO: write a function "needs", which takes a list of unquoted conditions
  # and a singular error/validation string, as above. The function ensures that
  # each need is met, and any unmet need is thereby declared to the user with
  # the given validation string.

  output$plotSIR_Stoc <- renderPlot({
    input$go
    isolate({
      validate(
        need(input$stochasticSIR > 0, validationString),
        need(input$betaSIR_Stoc >= 0, validationString),
        need(input$gammaSIR_Stoc >= 0, validationString),
        need(input$populationSIR_Stoc > 0, validationString),
        need(input$susceptibleSIR_Stoc > 0, validationString),
        need(input$infectedSIR_Stoc >= 0, validationString),
        need(input$recoveredSIR_Stoc >= 0, validationString),
        need(input$timesteps > 0, validationString),
        need(input$populationSIR_Stoc == (input$susceptibleSIR_Stoc + input$infectedSIR_Stoc + input$recoveredSIR_Stoc), "Number of persons living in all epidemic compartments must sum to the Population Count (N)")
      )

      num.sims <- input$stochasticSIR
      N <- input$populationSIR_Stoc
      vacc.prop <- 0
      init.immune <- input$recoveredSIR_Stoc
      init.infect <- input$infectedSIR_Stoc
      R0 <- 3.5
      infper <- 1 # infectious period in days
      ## latper <- 0                               # latent period in days
      ### R0 = beta*N/(1/infper) = beta*N*infper
      ### beta = R0/(N*infper)
      beta <- R0 / (1000) # transmission coefficient
      step <- 0.1 # time steps (in days)
      timeseq <- seq(0, 10, by = step) # discrete time intervals to simulate

      plot(0, 0,
        type = "n", xlim = c(min(timeseq), max(timeseq)),
        ylim = c(0, N + 10), bty = "n", xlab = "time (days)", ylab = "# hosts", lwd = 1
      )

      cum.incidence <- rep(init.infect, num.sims)

      for (ss in 1:num.sims) # do num.sims outbreaks
      {
        sim <- data.frame(S = N - init.infect - init.immune, I = init.infect, R = init.immune)

        for (ii in 2:length(timeseq)) # run through time series
        {
          p.trans <- 1 - exp(-beta * step * sim$I[ii - 1]) # probability of S -> I per unit S
          new.inf <- rbinom(1, sim$S[ii - 1], p.trans) # number new infections

          p.recov <- 1 - exp(-1 / infper * step) # probability of I -> R
          new.recov <- rbinom(1, sim$I[ii - 1], p.recov) # number new recoveries

          temp.S <- sim$S[ii - 1] - new.inf
          temp.I <- sim$I[ii - 1] + new.inf - new.recov
          temp.R <- sim$R[ii - 1] + new.recov

          sim <- rbind(sim, c(temp.S, temp.I, temp.R))
          cum.incidence[ss] <- cum.incidence[ss] + new.inf
        }
        lines(timeseq, sim[, "S"], col = "blue")
        lines(timeseq, sim[, "I"], col = "red")
        lines(timeseq, sim[, "R"], col = "green")
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
    dS <- (input$muBirth * N) - (input$muDeath * S) - (input$betaSIR * ((S * I) / (N^q)))
    dI <- (input$betaSIR * ((S * I) / (N^q))) - (input$gammaSIR * I) - (input$muDeath * I)
    dR <- (input$gammaSIR * I) - (input$muDeath * R)
    dN <- dS + dI + dR
    list(c(dS, dI, dR, dN, q))
  }

  sir_values <- reactive({
    req(input$timesteps, input$betaSIR, input$gammaSIR, input$muBirth, input$muDeath)


    validate(
      need(input$betaSIR >= 0, "Enter a valid value for parameters"),
      need(input$gammaSIR >= 0, "Enter a valid value for parameters"),
      need(input$populationSIR > 0, "Enter a valid value for parameters"),
      need(input$susceptibleSIR > 0, "Enter a valid value for parameters"),
      need(input$infectedSIR >= 0, "Enter a valid value for parameters"),
      need(input$recoveredSIR >= 0, "Enter a valid value for parameters"),
      need(input$timesteps > 0, "Enter a valid value for parameters"),
      need(input$populationSIR == (input$susceptibleSIR + input$infectedSIR + input$recoveredSIR), "Number of persons living in all epidemic compartments must sum to the Population Count (N)")
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
        theme(
          axis.line = element_line(color = "black"), axis.text = element_text(size = 14),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 16, face = "bold")
        ) +
        ggtitle("SIR Epidemic Model") +
        theme(plot.title = element_text(size = 22, face = "bold")) +
        theme(legend.position = "bottom") +
        ylab("Number of People") +
        xlab("Time") +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        geom_line(aes(y = S, color = "Blue"), linewidth = 1.5) +
        geom_line(aes(y = I, color = "Red"), linewidth = 1.5) +
        geom_line(aes(y = R, color = "Green"), linewidth = 1.5) +
        scale_color_identity(
          name = "SIR", breaks = c("Blue", "Red", "Green"),
          labels = c("Susceptible", "Infected", "Recovered"), guide = "legend"
        )
    })
  })

  output$SIRPhasePlane <- renderPlot({
    input$go
    isolate({
      val <- as.data.frame(sir_values())
      ggplot(val, aes(x = S)) +
        geom_line(aes(y = I, color = "Blue"), linewidth = 1.5) +
        theme(
          axis.line = element_line(color = "black"), axis.text = element_text(size = 14),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 16, face = "bold")
        ) +
        theme(legend.position = "bottom") +
        ggtitle("SI Phase Plane") +
        theme(plot.title = element_text(size = 22, face = "bold")) +
        ylab("Infected (I)") +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        xlab("Susceptible (S)") +
        scale_color_identity(
          breaks = "Blue",
          labels = "Susceptible"
        )
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
    dS <- (input$muBirth * N) - (input$muDeath * S) - (input$betaSIRD * ((S * I) / (N^q)))
    dI <- (input$betaSIRD * ((S * I) / (N^q))) - (input$gammaSIRD * I) - (input$deltaSIRD * I) - (input$muDeath * I)
    dR <- (input$gammaSIRD * I) - (input$muDeath * R)
    dD <- (input$deltaSIRD * I)
    dN <- dS + dI + dR
    list(c(dS, dI, dR, dD, dN, q))
  }

  sird_values <- reactive({
    req(input$timesteps, input$betaSIRD, input$gammaSIRD, input$deltaSIRD, input$muBirth, input$muDeath)

    validate(
      need(input$betaSIRD >= 0, "Enter a valid value for parameters"),
      need(input$gammaSIRD >= 0, "Enter a valid value for parameters"),
      need(input$deltaSIRD >= 0, "Enter a valid value for parameters"),
      need(input$populationSIRD > 0, "Enter a valid value for parameters"),
      need(input$susceptibleSIRD > 0, "Enter a valid value for parameters"),
      need(input$infectedSIRD >= 0, "Enter a valid value for parameters"),
      need(input$recoveredSIRD >= 0, "Enter a valid value for parameters"),
      need(input$deadSIRD >= 0, "Enter a valid value for parameters"),
      need(input$timesteps > 0, "Timesteps must be greater than 0."),
      need(input$populationSIRD == (input$susceptibleSIRD + input$infectedSIRD + input$recoveredSIRD), "Number of persons living in all epidemic compartments must sum to the Population Count (N)")
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
        theme(
          axis.line = element_line(color = "black"), axis.text = element_text(size = 14),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 16, face = "bold")
        ) +
        ggtitle("SIRD Epidemic Model") +
        theme(plot.title = element_text(size = 22, face = "bold")) +
        theme(legend.position = "bottom") +
        ylab("Number of People") +
        xlab("Time") +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        geom_line(aes(y = S, color = "Blue"), linewidth = 1.5) +
        geom_line(aes(y = I, color = "Red"), linewidth = 1.5) +
        geom_line(aes(y = R, color = "Green"), linewidth = 1.5) +
        geom_line(aes(y = D, color = "Orange"), linewidth = 1.5) +
        scale_color_identity(
          name = "SIRD", breaks = c("Blue", "Red", "Green", "Orange"),
          labels = c("Susceptible", "Infected", "Recovered", "Dead"), guide = "legend"
        )
    })
  })

  output$SIRDPhasePlane <- renderPlot({
    input$go
    isolate({
      val <- as.data.frame(sird_values())
      ggplot(val, aes(x = S)) +
        geom_line(aes(y = I, color = "Blue"), linewidth = 1.5) +
        theme(
          axis.line = element_line(color = "black"), axis.text = element_text(size = 14),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 16, face = "bold")
        ) +
        theme(legend.position = "bottom") +
        ggtitle("SI Phase Plane") +
        theme(plot.title = element_text(size = 22, face = "bold")) +
        ylab("Infected (I)") +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        xlab("Susceptible (S)") +
        scale_color_identity(
          breaks = "Blue",
          labels = "Susceptible"
        )
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
    dS <- (input$muBirth * N) - (input$muDeath * S) - (input$beta * ((S * I) / (N^q)))
    dE <- (input$beta * ((S * I) / (N^q))) - (input$gamma * E) - (input$muDeath * E)
    dI <- (input$gamma * E) - (I * input$sigma) - (input$muDeath * I)
    dR <- (I * input$sigma) - (input$muDeath * R)
    dN <- dS + dE + dI + dR
    list(c(dS, dE, dI, dR, dN, q))
  }

  seir_values <- reactive({
    req(input$timesteps, input$beta, input$gamma, input$muBirth, input$muDeath)

    validate(
      need(input$beta >= 0, "Enter a valid value for parameters"),
      need(input$gamma >= 0, "Enter a valid value for parameters"),
      need(input$sigma >= 0, "Enter a valid value for parameters"),
      need(input$population > 0, "Enter a valid value for parameters"),
      need(input$exposed >= 0, "Enter a valid value for parameters"),
      need(input$susceptible > 0, "Enter a valid value for parameters"),
      need(input$infected >= 0, "Enter a valid value for parameters"),
      need(input$recovered >= 0, "Enter a valid value for parameters"),
      need(input$timesteps > 0, "Enter a valid value for parameters."),
      need(input$population == (input$exposed + input$susceptible + input$infected + input$recovered), "Number of persons living in all epidemic compartments must sum to the Population Count (N)")
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
        theme(
          axis.line = element_line(color = "black"),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 16, face = "bold")
        ) +
        theme(legend.position = "bottom") +
        theme(plot.title = element_text(size = 22, face = "bold"), axis.text = element_text(size = 14)) +
        ylab("Number of People") +
        xlab("Time") +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        geom_line(aes(x = time, y = S, color = "Blue"), linewidth = 1.5) +
        geom_line(aes(x = time, y = E, color = "Brown"), linewidth = 1.5) +
        geom_line(aes(x = time, y = I, color = "Red"), linewidth = 1.5) +
        geom_line(aes(x = time, y = R, color = "Green"), linewidth = 1.5) +
        scale_color_identity(
          name = "SEIR", breaks = c("Blue", "Brown", "Red", "Green"),
          labels = c("Susceptible", "Exposed", "Infected", "Recovered"), guide = "legend"
        )
    })
  })

  output$SEIRPhasePlane <- renderPlot({
    input$go
    isolate({
      val <- as.data.frame(seir_values())
      ggplot(val, aes(x = S)) +
        geom_line(aes(y = I, color = "Blue"), linewidth = 1.5) +
        theme(
          axis.line = element_line(color = "black"), axis.text = element_text(linewidth = 14),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 16, face = "bold")
        ) +
        ggtitle("SEIR Phase Plane") +
        theme(plot.title = element_text(size = 22, face = "bold")) +
        theme(legend.position = "bottom") +
        ylab("Infected (I)") +
        xlab("Susceptible (S)") +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        scale_color_identity(
          breaks = "Blue",
          labels = "Susceptible"
        )
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
    dS <- (input$muBirth * N) - (input$muDeath * S) - (input$betaSEIRD * ((S * I) / (N^q)))
    dE <- (input$beta * ((S * I) / (N^q))) - (input$gammaSEIRD * E) - (input$muDeath * E)
    dI <- (input$gammaSEIRD * E) - (I * input$sigmaSEIRD) - (input$deltaSEIRD * I) - (input$muDeath * I)
    dR <- (I * input$sigma) - (input$muDeath * R)
    dD <- (input$deltaSEIRD * I)
    dN <- dS + dE + dI + dR
    list(c(dS, dE, dI, dR, dD, dN, q))
  }

  seird_values <- reactive({
    req(input$timesteps, input$betaSEIRD, input$gammaSEIRD, input$muBirth, input$muDeath)

    validate(
      need(input$betaSEIRD >= 0, "Enter a valid value for parameters"),
      need(input$gammaSEIRD >= 0, "Enter a valid value for parameters"),
      need(input$sigmaSEIRD >= 0, "Enter a valid value for parameters"),
      need(input$deltaSEIRD >= 0, "Enter a valid value for parameters"),
      need(input$populationSEIRD > 0, "Enter a valid value for parameters"),
      need(input$exposedSEIRD >= 0, "Enter a valid value for parameters"),
      need(input$susceptibleSEIRD > 0, "Enter a valid value for parameters"),
      need(input$infectedSEIRD >= 0, "Enter a valid value for parameters"),
      need(input$recoveredSEIRD >= 0, "Enter a valid value for parameters"),
      need(input$deadSEIRD >= 0, "Enter a valid value for parameters"),
      need(input$timesteps > 0, "Enter a valid value for parameters"),
      need(input$populationSEIRD == (input$exposedSEIRD + input$susceptibleSEIRD + input$infectedSEIRD + input$recoveredSEIRD), "Number of persons living in all epidemic compartments must sum to the Population Count (N)")
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
        theme(
          axis.line = element_line(color = "black"), axis.text = element_text(size = 14),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 16, face = "bold")
        ) +
        theme(plot.title = element_text(size = 22, face = "bold")) +
        theme(legend.position = "bottom") +
        ylab("Number of People") +
        xlab("Time") +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        geom_line(aes(x = time, y = S, color = "Blue"), linewidth = 1.5) +
        geom_line(aes(x = time, y = E, color = "Brown"), linewidth = 1.5) +
        geom_line(aes(x = time, y = I, color = "Red"), linewidth = 1.5) +
        geom_line(aes(x = time, y = R, color = "Green"), linewidth = 1.5) +
        geom_line(aes(x = time, y = D, color = "Orange"), linewidth = 1.5) +
        scale_color_identity(
          name = "SEIRD", breaks = c("Blue", "Brown", "Red", "Green", "Orange"),
          labels = c("Susceptible", "Exposed", "Infected", "Recovered", "Dead"), guide = "legend"
        )
    })
  })

  output$SEIRDPhasePlane <- renderPlot({
    input$go
    isolate({
      val <- as.data.frame(seird_values())
      ggplot(val, aes(x = S)) +
        geom_line(aes(y = I, color = "Blue"), linewidth = 1.5) +
        theme(
          axis.line = element_line(color = "black"), axis.text = element_text(size = 14),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 16, face = "bold")
        ) +
        theme(legend.position = "bottom") +
        ggtitle("SEIRD Phase Plane") +
        theme(plot.title = element_text(size = 22, face = "bold")) +
        ylab("Infected (I)") +
        xlab("Susceptible (S)") +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        scale_color_identity(
          breaks = "Blue",
          labels = "Susceptible"
        )
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




  # Hide output when no Model is selected
  observe({
    hideTab(inputId = "tabSet", target = "Plot")
    hideTab(inputId = "tabSet", target = "Phase Plane")
    hideTab(inputId = "tabSet", target = "Output Summary")
    hideTab(inputId = "tabSet", target = "Mathematical Model")
  })

  # Hide given elements when no Model is selected
  observe({
    hide(id = "qValue")
    hide(id = "muValue")
    hide(id = "timesteps")
    hide(id = "stochasticSelect")
  })

  # Shows given elements when a Model is selected
  observe({
    toggle(id = "qValue", condition = (input$modelSelect == "SIR" || input$modelSelect == "SIRD" || input$modelSelect == "SEIR" || input$modelSelect == "SEIRD" || input$modelSelect == "SIR-Stochastic"))
    toggle(id = "muValue", condition = (input$modelSelect == "SIR" || input$modelSelect == "SIRD" || input$modelSelect == "SEIR" || input$modelSelect == "SEIRD" || input$modelSelect == "SIR-Stochastic"))
    toggle(id = "timesteps", condition = (input$modelSelect == "SIR" || input$modelSelect == "SIRD" || input$modelSelect == "SEIR" || input$modelSelect == "SEIRD" || input$modelSelect == "SIR-Stochastic"))
    toggle(id = "stochasticSelect", condition = (input$modelSelect == "SIR" || input$modelSelect == "SIRD" || input$modelSelect == "SEIR" || input$modelSelect == "SEIRD" || input$modelSelect == "SIR-Stochastic"))
  })

  # Shows output once Run Simulation button is activated
  observeEvent(input$go, {
    showTab(inputId = "tabSet", target = "Plot")
    showTab(inputId = "tabSet", target = "Phase Plane")
    showTab(inputId = "tabSet", target = "Output Summary")
    showTab(inputId = "tabSet", target = "Mathematical Model")
  })

  ## Reset the reactive UI elements to their default values. First, hide
  ## specific tabs in the tabset (TODO: determine if the whole tabset can be
  ## hidden instead of looping through targets within the tabSet). Next, update
  ## the model selection dropdown to the default unselected item. Third, set the
  observeEvent(input$resetAll, {
    for (tab in c("Plot", "Phase Plane", "Output Summary", "Mathematical Model")) {
      hideTab(inputId = "tabSet", tab)
    }

    # Model Select
    updatePickerInput(session, "modelSelect", selected = "Please choose a model")

    lapply(
      X = list(
        `SIR-Stochastic` = list(stochasticSIR = 50, betaSIR_Stoc = 0.00178, gammaSIR_Stoc = 2.73, populationSIR_Stoc = 1000, susceptibleSIR_Stoc = 990, infectedSIR_Stoc = 10, recoveredSIR_Stoc = 0),
        SIRD = list(betaSIRD = 0.001, gammaSIRD = 0.1, deltaSIRD = 0.05, populationSIRD = 500, susceptibleSIRD = 499, infectedSIRD = 1, recoveredSIRD = 0),
        SEIR = list(beta = 0.5, gamma = 0.5, sigma = 0.1, population = 53, susceptible = 50, exposed = 3, infected = 0, recovered = 0),
        SEIRD = list(betaSEIRD = 0.5, gammaSEIRD = 0.5, sigmaSEIRD = 0.1, deltaSEIRD = 0.05, populationSEIRD = 53, susceptibleSEIRD = 50, exposedSEIRD = 3, infectedSEIRD = 0, recoveredSEIRD = 0),
        SIR = list(betaSIR = 0.001, gammaSIR = 0.1, populationSIR = 500, susceptibleSIR = 499, infectedSIR = 1, recoveredSIR = 0),
        SIRD = list(betaSIRD = 0.001, gammaSIRD = 0.1, deltaSIRD = 0.05, populationSIRD = 500, susceptibleSIRD = 499, infectedSIRD = 1, recoveredSIRD = 0)
      ),
      FUN = updateNumericInputsByNameAndValue
    )

    # Model Formulation
    updateRadioButtons(session, "qValue", selected = "0")

    # Stochastic Select
    updateRadioButtons(session, "stochasticSelect", selected = "Deterministic")

    # Vital Dynamics
    updateCheckboxInput(session, "muValue", value = FALSE)

    # Number of Timesteps
    updateNumericInput(session, "timesteps", value = 100)
  })

  # Resetting values when choosing a new Model
  observeEvent(input$modelSelect, {
    # Hides output
    for(tab in c("Plot", "Phase Plane", "Output Summary", "Mathematical Model")) {
      hideTab(inputId = "tabSet", tab)
    }

    # Model Formulation
    updateRadioButtons(session, "qValue", selected = "0")

    # Stochastic Select
    updateRadioButtons(session, "stochasticSelect", selected = "Deterministic")

    # Vital Dynamics
    updateCheckboxInput(session, "muValue", value = FALSE)

    "SIR-Stochastic" %ifModelSelection%
      list(
        stochasticSIR = 50,
        betaSIR_Stoc = 0.00178,
        gammaSIR_Stoc = 2.73,
        populationSIR_Stoc = 1000,
        susceptibleSIR_Stoc = 990,
        infectedSIR_Stoc = 10,
        recoveredSIR_Stoc = 0,
        timesteps = 10
      )

    "SIRD" %ifModelSelection%
      list(
        betaSIRD = 0.001,
        gammaSIRD = 0.1,
        deltaSIRD = 0.05,
        populationSIRD = 500,
        susceptibleSIRD = 499,
        infectedSIRD = 1,
        recoveredSIRD = 0,
        timesteps = 50
      )

    "SEIR" %ifModelSelection%
      list(
        beta = 0.5,
        gamma = 0.5,
        sigma = 0.1,
        population = 53,
        susceptible = 50,
        exposed = 3,
        infected = 0,
        recovered = 0,
        timesteps = 25
      )

    "SEIRD" %ifModelSelection%
      list(
        betaSEIRD = 0.5,
        gammaSEIRD = 0.5,
        sigmaSEIRD = 0.1,
        deltaSEIRD = 0.05,
        populationSEIRD = 53,
        susceptibleSEIRD = 50,
        exposedSEIRD = 3,
        infectedSEIRD = 0,
        recoveredSEIRD = 0,
        timesteps = 50
      )
  })
}
