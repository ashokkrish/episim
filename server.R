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
    lapply(params, updateNumericInput,
           session = session,
           inputId = names(params))
  }

  ## TODO: The value needs to depend on the model selection.
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

    `SIR-Stochastic` = list(
      stochasticModelVariableNumberOfReplicates = c(sv_integer(), sv_between(0, 100, c(FALSE, TRUE))),
      beta        = c(sv_between(0, 1)),
      gamma       = c(sv_between(0, 5)),
      population  = c(sv_gt(0)),
      susceptible = c(sv_gt(0)),
      infected    = c(sv_gte(0)),
      recovered   = c(sv_gte(0))
    ),

    SIR = list(
      beta        = c(sv_between(0, 1)),
      gamma       = c(sv_between(0, 5)),
      population  = c(sv_gt(0)),
      susceptible = c(sv_gt(0)),
      infected    = c(sv_gte(0)),
      recovered   = c(sv_gte(0))
    ),

    SIRD = list(
      beta        = c(sv_between(0, 0.5)),
      gamma       = c(sv_between(0, 0.5)),
      delta       = c(sv_between(0, 0.5)),
      population  = c(sv_gt(0)),
      susceptible = c(sv_gt(0)),
      infected    = c(sv_gte(0)),
      recovered   = c(sv_gte(0)),
      dead        = c(sv_gte(0))
    ),

    SEIR = list(
      beta        = c(sv_between(0, 1)),
      gamma       = c(sv_between(0, 3)),
      sigma       = c(sv_between(0, 0.5)),
      population  = c(sv_gt(0)),
      susceptible = c(sv_gt(0)),
      exposed     = c(sv_gte(0)),
      infected    = c(sv_gte(0)),
      recovered   = c(sv_gte(0))
    ),

    SEIRD = list(
      beta        = c(sv_between(0, 1)),
      gamma       = c(sv_between(0, 3)),
      sigma       = c(sv_between(0, 0.5)),
      delta       = c(sv_between(0, 0.5)),
      population  = c(sv_gt(0)),
      susceptible = c(sv_gt(0)),
      exposed     = c(sv_gte(0)),
      infected    = c(sv_gte(0)),
      recovered   = c(sv_gte(0)),
      dead        = c(sv_gte(0))
    ),

    timesteps = c(sv_integer(), sv_gt(0)))

  inputValidator <- addThenEnableValidatorRules(InputValidator$new(), rules)


  ## FIXME: when a value is set by the user, and then Vital Dynamics is
  ## disabled, the effective value should be zero to control how the models
  ## operate. If Vital Dynamics is re-enabled the previous value should be
  ## restored unless the user has reset all values using the reset action
  ## button. NOTE: use a simple variable to store the previous value; that
  ## variable must also be reset to zero when the entire application is reset so
  ## that the "historical" value the user entered is forgotten.
  observeEvent(input$muValue, {
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
        beta = 0.4,
        gamma = 0.04,
        population = 1000,
        susceptible = 997,
        infected = 3,
        recovered = 0,
        timesteps = 25
      ))
    }

    ## SIR - PMA
    if ((input$qValue == "0") && (input$modelSelect == "SIR")) {
      updateNumericInputsByNameAndValue(list(
        beta = 0.001,
        gamma = 0.1,
        population = 500,
        susceptible = 499,
        infected = 1,
        recovered = 0,
        timesteps = 50
      ))
    }

    ## SIRD - TMA
    if ((input$qValue == "1") && (input$modelSelect == "SIRD")) {
      updateNumericInputsByNameAndValue(list(
        beta = 0.4,
        gamma = 0.04,
        delta = 0.05,
        population = 1000,
        susceptible = 997,
        infected = 3,
        recovered = 0,
        timesteps = 25
      ))
    }

    ## SIRD - PMA
    if ((input$qValue == "0") && (input$modelSelect == "SIRD")) {
      updateNumericInputsByNameAndValue(list(
        beta = 0.001,
        gamma = 0.1,
        delta = 0.05,
        population = 500,
        susceptible = 499,
        infected = 1,
        recovered = 0,
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
        beta = 0.35,
        gamma = 0.1429,
        sigma = 0.0476,
        delta = 0.05,
        population = 500,
        susceptible = 499,
        exposed = 0,
        infected = 1,
        recovered = 0,
        timesteps = 20
      ))
    }

    ## SEIRD - PMA
    if (input$qValue == "0" && input$modelSelect == "SEIRD") {
      updateNumericInputsByNameAndValue(list(
        beta = 0.5,
        gamma = 0.5,
        sigma = 0.1,
        delta = 0.05,
        population = 53,
        susceptible = 50,
        exposed = 3,
        infected = 0,
        recovered = 0,
        timesteps = 50
      ))
    }

    ## SIR-Stochastic
    if (input$modelSelect == "SIR" && input$stochasticSelect == 1) {
      updateNumericInputsByNameAndValue(list(
        stochasticModelVariableNumberOfReplicates = 50,
        beta = 0.00178,
        gamma = 2.73,
        population = 1000,
        susceptible = 990,
        infected = 10,
        recovered = 0,
        timesteps = 10
      ))
    }
  })

  # Toggle the visibility of the given input widgets and action buttons when any
  # model is selected.
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

  ## Toggle the visibility of the outputPanel based on the user's interactions
  ## with the actionButtons.
  observeEvent(input$go, {
    show("outputPanel")
    ## The LaTeX is rendered dynamically based upon the selected model.
    ## Rendering occurs every time the GO action button is pressed. This will
    ## allow more flexibility later on, if needed.
    output$modelLaTeX <- renderUI(
      tagList(
        withMathJax(
          switch(input$modelSelect,
                 SIR = SIR_LaTeX(input$muValue),
                 SIRS = SIRS_LaTeX(input$muValue),
                 SIRD = SIRD_LaTeX(input$muValue),
                 SEIR = SEIR_LaTeX(input$muValue),
                 SEIRD = SEIRD_LaTeX(input$muValue)))))
  })

  ## TODO: resetting the application should set the widget values to those which
  ## are defined for the model in the spreadsheet (hosted on Google Sheets).
  observeEvent(input$resetAll, {
    hide("outputPanel")

    # Model Select
    ## updatePickerInput(session, "modelSelect", selected = 0)
    updatePickerInput("modelSelect", selected = 0)

    # NOTE: The names of the components in X are unimportant to the execution of
    # the following statement. The names merely document which set of parameters
    # are applicable to a given model; the parameter values are the defaults for
    # the model.
    lapply(
      list(
        SIR =
          list(
            beta = 0.001,
            gamma = 0.1,
            population = 500,
            susceptible = 499,
            infected = 1,
            recovered = 0
          ),
        `SIR-Stochastic` = # Parameter and variable values when stochastic.
          list(
            stochasticModelVariableNumberOfReplicates = 50, # No. replicates
            beta = 0.00178,
            gamma = 2.73,
            population = 1000,
            susceptible = 990,
            infected = 10,
            recovered = 0
          ),
        SIRD =
          list(
            beta = 0.001,
            gamma = 0.1,
            delta = 0.05,
            population = 500,
            susceptible = 499,
            infected = 1,
            recovered = 0
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
        ## FIXME Khanh: Need to verify the value of each
        ## I temporarily leaves the value of each similar to SIR but
        ## I will change after having vertification
        SIRS =
          list(
            betaSIRS = 0.001,
            gammaSIRS = 0.1,
            xiSIRS = 0.1, # Value of xi needed here
            populationSIRS = 500,
            susceptibleSIRS = 499,
            infectedSIRS = 1,
            recoveredSIRS = 0
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

  ## Whenever the model selection changes (whether the parameters and variables
  ## are set manually or taken from pre-defined models), the widget values
  ## throughout the application are updated as appropriate.
  observeEvent(input$modelSelect, {
    # Hide output tabs when the model changes.
    hide("outputPanel")

    ## Total mass action or pseudo mass action
    updateRadioButtons(session, "qValue", selected = "0")

    updateRadioButtons(session, "stochasticSelect", selected = "Deterministic")

    ## TODO: refactor the application to use a more descriptive variable name:
    ## `vitalDynamicsSelect', perhaps.
    updateCheckboxInput(session, "muValue", value = FALSE) # Vital Dynamics

    ## TODO: Obtain the parameters that should be used based off of the current
    ## model selection. When a pre-defined model is selected, resetting the
    ## application state should reset the widget values to those defined by the
    ## model (which allows users to modify the parameters of a pre-defined model
    ## from some alternate starting point than our own pre-defined widget value
    ## defaults for manual input).
    ###
    ## parameters <- spreadsheet |>
    ##   filter(model == input$modelSelect) |>
    ##   select(parameters)
    ## updateNumericInputsByNameAndValue(as.list(parameters))

    ## TODO: these should be extracted into a data.frame which can be updated
    ## using any spreadsheet application.
    "SIR-Stochastic" %ifModelSelectionUpdateInputs%
      list(
        stochasticModelVariableNumberOfReplicates = 50,
        beta = 0.00178,
        gamma = 2.73,
        population = 1000,
        susceptible = 990,
        infected = 10,
        recovered = 0,
        timesteps = 10
      )

    "SIRD" %ifModelSelectionUpdateInputs%
      list(
        beta = 0.001,
        gamma = 0.1,
        delta = 0.05,
        population = 500,
        susceptible = 499,
        infected = 1,
        recovered = 0,
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
        beta = 0.5,
        gamma = 0.5,
        sigma = 0.1,
        delta = 0.05,
        population = 53,
        susceptible = 50,
        exposed = 3,
        infected = 0,
        recovered = 0,
        timesteps = 50
      )
  })
}
