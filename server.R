## TODO: see if it is tenable to elide the session argument from the widget
## updating functions.

server <- function(input, output, session) {
  ## MAYBE TODO: move these three utility functions back out of the server
  ## function and instead ensure that their discover their own environment, and
  ## don't complain about missing objects during definition.
  `%ifModelSelectionUpdateInputs%` <- function(modelSelectionString, params) {
    if (input$modelSelect == modelSelectionString) {
      updateNumericInputsByNameAndValue(params)
    }
  }

  updateNumericInputsByNameAndValue <- function(params) {
    ## I wish this was less than eighty characters so it could be on one line.
    invisible(lapply(params,
      updateNumericInput,
      session = session,
      inputId = names(params)
    ))
  }

  addThenEnableValidatorRules <- function(validator, inputIdRulePairs) {
    function(rules, inputId) {
      lapply(rules, validator$add_rule, inputId = inputId)
      validator$add_rule(rule = sv_required(), inputId = inputId)
    } |>
    mapply(inputIdRulePairs, names(inputIdRulePairs))

    validator$enable()
    invisible(validator)
  }

  ## Data validation
  rules <- list(
    ## Vital statistics
    muBirth = c(sv_between(0, 0.1)),
    muDeath = c(sv_between(0, 0.1)),

    ## SIR_Stochastic
    stochasticSIR = c(sv_integer(), sv_between(0, 100, c(FALSE, TRUE))),
    betaSIR_Stoc        = c(sv_between(0, 1)),
    gammaSIR_Stoc       = c(sv_between(0, 5)),
    populationSIR_Stoc  = c(sv_gt(0)),
    susceptibleSIR_Stoc = c(sv_gt(0)),
    infectedSIR_Stoc    = c(sv_gte(0)),
    recoveredSIR_Stoc   = c(sv_gte(0)),

    ## SIR
    betaSIR        = c(sv_between(0, 1)),
    gammaSIR       = c(sv_between(0, 5)),
    populationSIR  = c(sv_gt(0)),
    susceptibleSIR = c(sv_gt(0)),
    infectedSIR    = c(sv_gte(0)),
    recoveredSIR   = c(sv_gte(0)),

    ## SIRD
    betaSIRD        = c(sv_between(0, 0.5)),
    gammaSIRD       = c(sv_between(0, 0.5)),
    deltaSIRD       = c(sv_between(0, 0.5)),
    populationSIRD  = c(sv_gt(0)),
    susceptibleSIRD = c(sv_gt(0)),
    infectedSIRD    = c(sv_gte(0)),
    recoveredSIRD   = c(sv_gte(0)),
    deadSIRD        = c(sv_gte(0)),

    ## SEIR,
    beta        = c(sv_between(0, 1)),
    gamma       = c(sv_between(0, 3)),
    sigma       = c(sv_between(0, 0.5)),
    population  = c(sv_gt(0)),
    susceptible = c(sv_gt(0)),
    exposed     = c(sv_gte(0)),
    infected    = c(sv_gte(0)),
    recovered   = c(sv_gte(0)),

    ## SEIRD
    betaSEIRD        = c(sv_between(0, 1)),
    gammaSEIRD       = c(sv_between(0, 3)),
    sigmaSEIRD       = c(sv_between(0, 0.5)),
    deltaSEIRD       = c(sv_between(0, 0.5)),
    populationSEIRD  = c(sv_gt(0)),
    susceptibleSEIRD = c(sv_gt(0)),
    exposedSEIRD     = c(sv_gte(0)),
    infectedSEIRD    = c(sv_gte(0)),
    recoveredSEIRD   = c(sv_gte(0)),
    deadSEIRD        = c(sv_gte(0)),

    timesteps = c(sv_integer(), sv_gt(0)))

  inputValidator <- addThenEnableValidatorRules(InputValidator$new(), rules)

  ## TODO: change the observation to set these values when the widget is changed
  ## to the off state. Reset vital dynamics when not checked off (meaning off or
  ## on? selected or deselected? yes or no?)
  observe({
    input$muValue
    updateNumericInput(session, "muBirth", value = 0)
    updateNumericInput(session, "muDeath", value = 0)
  })

  ## FIXME: are there any missing parameters? I used my editor's undo
  ## feature after ridding these conditional updates of duplicated blocks
  ## (where the condition of PMA or TMA was irrelevant, or models had the
  ## same parameter values). I undid that change because I was unsure of
  ## it once I noticed that some of these do have different parameters
  ## (like delta).
  observeEvent(input$qValue, {
    ## In all cases the following input must be reset to FALSE.
    updateCheckboxInput(session, "muValue", value = FALSE)

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
    }

    ## SIRD - TMA
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
    }

    ## SIRD - PMA
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
    }

    ## SEIR - TMA
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
    }

    ## SEIR - PMA
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
    }

    ## SEIRD - TMA
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
    }

    ## SEIRD - PMA
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
    }

    ## SIR-Stochastic
    if (input$modelSelect == "SIR-Stochastic") {
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
        #
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

        ## MAYBE FIXME: linewidth is unused, apparently. See if removing it
        ## doesn't break anything.
        geom_line(aes(y = I, color = "Blue")) +
        theme(
          axis.line = element_line(color = "black"),
          axis.text = element_text(linewidth = 14),
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
    if (input$modelSelect %in% c("SIR", "SIRD", "SEIR", "SEIRD", "SIR-Stochastic")) {
      toggle("qValue")
      toggle("muValue")
      toggle("timesteps")
      toggle("stochasticSelect")
    }
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

    # NOTE: The names of the components in X are unimportant to the execution of
    # the following statement. The names merely document which set of parameters
    # are applicable to a given model; the parameter values are the defaults for
    # the model.
    lapply(
      list(
        `SIR-Stochastic` =
          list(
            stochasticSIR = 50,
            betaSIR_Stoc = 0.00178,
            gammaSIR_Stoc = 2.73,
            populationSIR_Stoc = 1000,
            susceptibleSIR_Stoc = 990,
            infectedSIR_Stoc = 10,
            recoveredSIR_Stoc = 0
          ),
        SIRD =
          list(
            betaSIRD = 0.001,
            gammaSIRD = 0.1,
            deltaSIRD = 0.05,
            populationSIRD = 500,
            susceptibleSIRD = 499,
            infectedSIRD = 1,
            recoveredSIRD = 0
          ),
        SEIR =
          list(
            beta = 0.5,
            gamma = 0.5,
            sigma = 0.1,
            population = 53,
            susceptible = 50,
            exposed = 3,
            infected = 0,
            recovered = 0
          ),
        SEIRD =
          list(
            betaSEIRD = 0.5,
            gammaSEIRD = 0.5,
            sigmaSEIRD = 0.1,
            deltaSEIRD = 0.05,
            populationSEIRD = 53,
            susceptibleSEIRD = 50,
            exposedSEIRD = 3,
            infectedSEIRD = 0,
            recoveredSEIRD = 0
          ),
        SIR =
          list(
            betaSIR = 0.001,
            gammaSIR = 0.1,
            populationSIR = 500,
            susceptibleSIR = 499,
            infectedSIR = 1,
            recoveredSIR = 0
          ),
        SIRD =
          list(
            betaSIRD = 0.001,
            gammaSIRD = 0.1,
            deltaSIRD = 0.05,
            populationSIRD = 500,
            susceptibleSIRD = 499,
            infectedSIRD = 1,
            recoveredSIRD = 0
          )
      ),
      updateNumericInputsByNameAndValue
    )

    ## Total mass action or pseudo mass action
    updateRadioButtons(session, "qValue", selected = "0")

    updateRadioButtons(session, "stochasticSelect", selected = "Deterministic")
    updateCheckboxInput(session, "muValue", value = FALSE) # Vital Dynamics
    updateNumericInput(session, "timesteps", value = 100)
  })

  ## Whenever the model selection changes the widgets throughout the application
  ## are reset to their default values.
  observeEvent(input$modelSelect, {
    # Hide output tabs when the application is reset.
    tabs <- c("Plot", "Phase Plane", "Output Summary", "Mathematical Model")
    for (tab in tabs) hideTab(inputId = "tabSet", tab)

    ## Total mass action or pseudo mass action
    updateRadioButtons(session, "qValue", selected = "0")

    updateRadioButtons(session, "stochasticSelect", selected = "Deterministic")
    updateCheckboxInput(session, "muValue", value = FALSE) # Vital Dynamics


    "SIR-Stochastic" %ifModelSelectionUpdateInputs%
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

    "SIRD" %ifModelSelectionUpdateInputs%
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

    "SEIR" %ifModelSelectionUpdateInputs%
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

    "SEIRD" %ifModelSelectionUpdateInputs%
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
