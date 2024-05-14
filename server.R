## TODO: see if it is tenable to elide the session argument from the widget
## updating functions.

server <- function(input, output, session) {
  ## MAYBE TODO: move these three utility functions back out of the server
  ## function and instead ensure that their discover their own environment, and
  ## don't complain about missing objects during definition.
  `%ifModelSelectionUpdateInputs%` <- function(modelSelectionString, params) {
    reactive({
      if (input$modelSelect == modelSelectionString) {
        updateNumericInputs(params)
      }
    })
  }

  updateNumericInputs <- function(params) {
    lapply(params, updateNumericInput,
           session = session,
           inputId = names(params))
  }

  ## FIXME: this is not working with `rules' at the moment.
  addThenEnableValidatorRules <- function(validator, ruleLists) {
    mapply(
      \(rules, inputId) {
        lapply(rules, validator$add_rule, inputId = inputId)
        validator$add_rule(rule = sv_required(), inputId = inputId)
      },
      ruleLists,
      names(ruleLists)
    )

    validator$enable()
    invisible(validator)
  }

  ## Data validation
  rules <- list(
    `Rules applying to all models` = list(
      ## Vital statistics
      muBirth = c(sv_between(0, 0.1)),
      muDeath = c(sv_between(0, 0.1)),
      population = c(sv_gt(0)),
      susceptible = c(sv_gt(0)),
      exposed = c(sv_gte(0)),
      infected = c(sv_gte(0)),
      recovered = c(sv_gte(0)),
      dead = c(sv_gte(0))
    ),

    ## TODO: When the SIR model is stochastic we want these validation rules applied.
    ## stochasticModelVariableNumberOfReplicates = c(sv_integer(), sv_between(0, 100, c(FALSE, TRUE))),
    SIR = list(beta = c(sv_between(0, 1)),
               gamma = c(sv_between(0, 5))),

    SIRS = list(beta = c(sv_between(0, 1)),
                gamma = c(sv_between(0, 5))),

    SIRD = list(beta = c(sv_between(0, 0.5)),
                gamma = c(sv_between(0, 0.5)),
                delta = c(sv_between(0, 0.5))),

    SEIR = list(beta = c(sv_between(0, 1)),
                gamma = c(sv_between(0, 3)),
                sigma = c(sv_between(0, 0.5))),

    SEIRS = list(beta = c(sv_between(0, 1)),
                 gamma = c(sv_between(0, 3)),
                 sigma = c(sv_between(0, 0.5)),
                 xi = c(sv_between(0, 0.5))),

    SEIRD = list(beta = c(sv_between(0, 1)),
                 gamma = c(sv_between(0, 3)),
                 sigma = c(sv_between(0, 0.5)),
                 delta = c(sv_between(0, 0.5))),

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

  observeEvent(input$massActionSelect, {
    ## In all cases the following input must be reset to FALSE.
    updateCheckboxInput(session, "muValue", value = FALSE)

    ## SIR - TMA
    if ((input$massActionSelect == "1") && (input$modelSelect == "SIR")) {
      updateNumericInputs(list(
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
    if ((input$massActionSelect == "0") && (input$modelSelect == "SIR")) {
      updateNumericInputs(list(
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
    if ((input$massActionSelect == "1") && (input$modelSelect == "SIRD")) {
      updateNumericInputs(list(
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
    if ((input$massActionSelect == "0") && (input$modelSelect == "SIRD")) {
      updateNumericInputs(list(
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
    if ((input$massActionSelect == "1") && (input$modelSelect == "SEIR")) {
      updateNumericInputs(list(
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
    if ((input$massActionSelect == "0") && (input$modelSelect == "SEIR")) {
      updateNumericInputs(list(
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
    if ((input$massActionSelect == "1") && (input$modelSelect == "SEIRD")) {
      updateNumericInputs(list(
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
    if (input$massActionSelect == "0" && input$modelSelect == "SEIRD") {
      updateNumericInputs(list(
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
      updateNumericInputs(list(
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
      if (input$modelSelect %in% c("SIR", "SIRS", "SIRD", "SEIR", "SEIRS", "SEIRD")) {
        show
      } else {
        hide
      }
    )
  })

  ## Toggle the visibility of the outputPanel based on the user's interactions
  ## with the "go" actionButton.
  observeEvent(input$go, {
    show("outputPanel")

    ## The solve and render functions are defined in files named like
    ## `input$modelSelect_solve.R' in the R subfolder.
    ## FIXME: this is not sufficient, they need arguments. This needs a
    ## dispatching function which will pass the arguments.
    call(paste0(solveAndRender, input$modelSelect))

    ## The LaTeX is rendered dynamically based upon the selected model.
    ## Rendering occurs every time the GO action button is pressed. This will
    ## allow more flexibility later on, if needed.
    output$modelLaTeX <- renderUI(renderModelLaTeX(input$muValue))
  })

  ## TODO: resetting the application should set the widget values to those which
  ## are defined for the model in the spreadsheet (hosted on Google Sheets).
  observeEvent(input$resetAll, {
    hide("outputPanel")
    updatePickerInput("modelSelect", selected = 0)
    updateRadioButtons("massActionSelect", selected = 0)
    updateRadioButtons("stochasticSelect", selected = 0)
    updateCheckboxInput("muValue", value = FALSE) # Vital Dynamics
    updateNumericInput("timesteps", value = 100)
  })

  ## Whenever the model selection changes (whether the parameters and variables
  ## are set manually or taken from pre-defined models), the widget values
  ## throughout the application are updated as appropriate.
  observeEvent(input$modelSelect, {
    hide("outputPanel")

    updateRadioButtons(session, "massActionSelect", selected = 0)
    updateRadioButtons(session, "stochasticSelect", selected = 0)

    ## TODO: refactor the application to use a more descriptive variable name:
    ## `vitalDynamicsSelect', perhaps.
    updateCheckboxInput(session, "muValue", value = FALSE) # Vital Dynamics

    filter(defaultParameterValues,
           modelType == input$modelSelect,
           stochastic == input$stochasticSelect) |>
      as.list() |>
      ## TODO: the function must handle NAs appropriately, such that a widget is
      ## hidden when the value is NA rather than set to an invalid state.
      updateNumericInputs()
  })
}
