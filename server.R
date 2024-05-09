## TODO: see if it is tenable to elide the session argument from the widget
## updating functions.

server <- function(input, output, session) {
  ## MAYBE TODO: move these three utility functions back out of the server
  ## function and instead ensure that their discover their own environment, and
  ## don't complain about missing objects during definition.
  `%ifModelSelectionUpdateInputs%` <- function(modelSelectionString, params) {
    reactive({
      if (input$modelSelect == modelSelectionString) {
        updateNumericInputsByNameAndValue(params)
      }
    })
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
    mapply(\(rules, inputId) {
      lapply(rules, validator$add_rule, inputId = inputId)
      validator$add_rule(rule = sv_required(), inputId = inputId)
    },
    inputIdRulePairs,
    names(inputIdRulePairs))

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

  sir_stochastic_plot()

  sir_plot()

  sird_plot()

  seir_plot()

  seird_plot()

  # Hide output when no Model is selected
  observe({
    hideTab(inputId = "tabSet", target = "Plot")
    hideTab(inputId = "tabSet", target = "Phase Plane")
    hideTab(inputId = "tabSet", target = "Output Summary")
    hideTab(inputId = "tabSet", target = "Mathematical Model")
  })

  # Toggle the visibility of the given input widgets when any model is selected.
  observe({
    lapply(
      list("modelConfiguration", "actionButtons"),
      if (input$modelSelect %in% c("SIR", "SIRS", "SIRD", "SEIR", "SEIRD")) {
        show
      } else {
        hide
      }
    )
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
