server <- function(input, output, session) {
  observe_helpers(withMathJax = TRUE, help_dir = "www/markdown")

  model <- reactive(str_split_1(req(input$modelSelect), "_"))
  compartmentalModel <- reactive(model()[[1]])
  publication <- reactive(model()[[2]])

  observe({print(paste("The compartmental model selection is:", compartmentalModel()))})

  exposedCompartmentInModel <- reactive({ str_detect(compartmentalModel(), "E") })
  deadCompartmentInModel <- reactive({ str_detect(compartmentalModel(), "D") })

  observe({
    if (exposedCompartmentInModel()) {
      hideTab(inputId = "tabs", target = "phasePlane")
    } else {
      showTab(inputId = "tabs", target = "phasePlane")
    }
  })

  ## Ensure that stochasticity is only enabled for the model for which it is
  ## implemented. There is also a conditionalPanel surrounding the "stochastic"
  ## radio buttons in ui.R.
  observe({
    if (compartmentalModel() != "SIR") {
      updateRadioButtons(inputId = "stochastic", selected = 0)
      disable(selector = "input[name='stochastic'][value='1']")
    }
    else {
      enable(selector = "input[name='stochastic'][value='1']")
      if (input$stochastic == 1) {
        if (input$trueMassAction == 1)
          disable(selector = "input[name='distribution'][value='0']")
        else
          enable(selector = "input[name='distribution'][value='0']")
      }
    }
  })

  observe(do.call(ifelse(input$stochastic == 1, "hideTab", "showTab"),
                  args = list(inputId = "tabs", target = "outputSummary")))

  rules <- tribble(
    ~ model, # A regular expression
    ~ ruleList,
    "S.*S", # Waning learned immunity
    list(
      xi = c(sv_gt(0), sv_lte(1))
    ),
    "D", # Death
    list(
      delta = c(sv_between(0, 1)),
      dead = c(sv_integer(), sv_gte(0))
    ),
    "E", # Exposure
    list(
      sigma = c(sv_gt(0), sv_lte(1)),
      exposed = c(sv_integer(), sv_gte(0))
    ),

    ## Global rules
    NA,
    list(
      ## Vital dynamics
      muBirth = c(sv_between(0, 1)),
      muDeath = c(sv_between(0, 1)),

      ## Global rules for compartments; only the compartments that are actually
      ## common to all models can be included here, otherwise all models will be
      ## invalidated if an inapplicable compartment has an invalid input (e.g.
      ## the exposed and dead compartments have invalid input while SIR is
      ## selected).
      population = c(sv_integer(), sv_gt(0)),
      susceptible = c(sv_integer(), sv_gt(0)),
      infected = c(sv_integer(), sv_gte(0)),
      ## The above example means you---the current editor---must move the
      ## recovered rule to each model featuring the recovery compartment if ever
      ## you include an SI model (having no recovered compartment), and remove it
      ## from the global rule. Make sense? Godspeed.
      recovered = c(sv_integer(), sv_gte(0)),

      ## Global rules for parameters
      beta = c(sv_gt(0), sv_lte(1)),
      gamma = c(sv_gt(0)),

      ## Simulation options
      replicates = c(sv_integer(), sv_gt(0)),
      timesteps = c(sv_gt(0))
    )
  )

  globalValidator <-
    addRuleListToValidator(InputValidator$new(),
                           filter(rules, is.na(model))[, 2][[1]][[1]])

  validatorsNeedingConditions <-
  filter(rules, !is.na(model)) %>%
    rowwise() %>%
    mutate(vld = list(addRuleListToValidator(InputValidator$new(), ruleList)),
           lambda = list(eval(bquote(\() grepl(.(model),
                                               compartmentalModel())))),
           .keep = "none")

  isolate({
    validatorsNeedingConditions %>%
      pwalk(function(vld, lambda) {
        vld$condition(lambda)
        globalValidator$add_validator(vld)
      })
  })

  rules <- tribble(
  ~ model, # A regular expression
  ~ ruleList,
  "S.*S", # Waning learned immunity
  list(
    xi = c(sv_gt(0), sv_lte(1))
  ),
  "D", # Death
  list(
    delta = c(sv_between(0, 1)),
    dead = c(sv_integer(), sv_gte(0))
  ),
  "E", # Exposure
  list(
    sigma = c(sv_gt(0), sv_lte(1)),
    exposed = c(sv_integer(), sv_gte(0))
  ),

  # Global rules
  NA,
  list(
    ## Vital dynamics
    muBirth = c(sv_between(0, 1)),
    muDeath = c(sv_between(0, 1)),

    ## Global rules for compartments; only the compartments that are actually
    ## common to all models can be included here, otherwise all models will be
    ## invalidated if an inapplicable compartment has an invalid input (e.g.
    ## the exposed and dead compartments have invalid input while SIR is
    ## selected).
    population = c(sv_integer(), sv_gt(0)),
    susceptible = c(sv_integer(), sv_gt(0)),
    infected = c(sv_integer(), sv_gte(0)),
    ## The above example means you---the current editor---must move the
    ## recovered rule to each model featuring the recovery compartment if ever
    ## you include an SI model (having no recovered compartment), and remove it
    ## from the global rule. Make sense? Godspeed.
    recovered = c(sv_integer(), sv_gte(0)),

    ## Global rules for parameters
    beta = c(sv_gt(0), sv_lte(1)),
    gamma = c(sv_gt(0)),

    ## Simulation options
    replicates = c(sv_integer(), sv_gt(0)),
    timesteps = c(sv_gt(0))
    )
  )

  globalValidator <- addRuleListToValidator(
    InputValidator$new(),
    filter(rules, is.na(model))[, 2][[1]][[1]]
  )

  validatorsAndLambdas <- {
    filter(rules, !is.na(model)) |>
      rowwise() |>
      mutate(vld = list(addRuleListToValidator(InputValidator$new(), ruleList)),
            ## FIXME: the value of the selection should be a character vector...
            lambda = list(eval(bquote(\() grepl(.(model), compartmentalModel())))),
            .keep = "none")
  }

  isolate(
    mapply(
      FUN = \(validator, lambda) {
        validator$condition(lambda)
        globalValidator$add_validator(validator)
      },
      validatorsAndLambdas[[1]],
      validatorsAndLambdas[[2]]
    )
  )

  ## FIXME: All models have at least one set of default values, a "fallback".
  ## Some models have more than one set which is specific to the model and its
  ## configuration.
  defaults <- reactive({
    modelSpecific <-
      filter(defaultInputValues,
             modelType == req(compartmentalModel()),
             readablePublicationName == req(publication()),
             trueMassAction == input$trueMassAction,
             vitalDynamics == input$vitalDynamics)

    defaultValueSets <- dim(modelSpecific)[1]
    if (defaultValueSets != 1) {
      ## If there is exactly one set of default values for this exact model
      ## (i.e. with its configuration) return that, otherwise return the
      ## fallback set of values.
      warning(sprintf("There are %s observations/sets of default values for ",
                      defaultValueSets),
              "this exact configuration.")

      ## Overwrite the object
      modelSpecific <- filter(defaultInputValues,
                            modelType == input$modelSelect,
                            is.na(trueMassAction),
                            is.na(vitalDynamics),
                            is.na(stochastic))
    }

    modelSpecific |>
      select(!c(vitalDynamics,
                trueMassAction,
                stochastic)) |>
      select(where(\(x) all(!is.na(x))))
  })

  observe({
    toggleClass(id = "vital-dynamics-well",
                class = "well",
                condition = input$vitalDynamics == 1)
  })

  observe({
    toggleClass(id = "model-stochasticity-well",
                class = "well",
                condition = input$stochastic == 1)
  })

  observe({
    print(defaults())
  })

  observe({
    if (all(input$totalMassAction == 1, input$stochastic == 1)) {
      updateRadioButtons(inputId = "distribution",
                         selected = 1)
    }
  }) |> bindEvent(input$stochastic)

  ## Whenever the model selection changes, the widget values throughout the
  ## application are updated according to the defaults specified for the model
  ## configuration in the defaultInputValues.xlsx spreadsheet in the data/
  ## project subfolder.
  observe({
    shiny::validate(need(dim(defaults())[1] == 1,
                         message = "ERROR: defaults() has length != 1"))

    output$commonParameters <- renderUI({
      rateLabels <-
        if (isolate(exposedCompartmentInModel()))
          c(beta = "exposure", gamma = "infection")
        else
          c(beta = "infection", gamma = "recovery")

      rateValues <-
        select(defaults(), beta, gamma) |>
        mutate(across(c(beta, gamma), as.numeric))

      div(
        id = "beta-and-gamma",
        numericInputWithMathJax("beta",
                                rateLabels["beta"],
                                if (input$freeze == FALSE)
                                  rateValues$beta
                                else
                                  isolate(input$beta)),
        numericInputWithMathJax("gamma",
                                rateLabels["gamma"],
                                if (input$freeze == FALSE)
                                  rateValues$gamma
                                else
                                  isolate(input$gamma))
      )
    })

    if (input$freeze == FALSE)
      updateNumericInputs(defaults(), session)
  }) |> bindEvent({ defaults() })

  ## A reactive value like input, but with hidden and irrelevant inputs removed.
  greedy_visibleInputs <- reactive({
    shiny::validate(need(compartmentalModel(), "A model must be selected."))
    ## TODO: this could be improved, but I'm not sure how. This is kinda awful
    ## and should be built-in to Shiny.
    relevantInputs <-
      reactiveValuesToList(
        reactiveValues(
          modelSelect = input$modelSelect,
          trueMassAction = input$trueMassAction,
          vitalDynamics = input$vitalDynamics,
          muBirth = input$muBirth,
          muDeath = input$muDeath,
          stochastic = input$stochastic,
          distribution = input$distribution,
          replicates = input$replicates,
          rerun = input$rerun,
          timesteps = input$timesteps,
          beta = input$beta,
          gamma = input$gamma,
          delta = input$delta,
          sigma = input$sigma,
          xi = input$xi,
          population = input$population,
          susceptible = input$susceptible,
          exposed = input$exposed,
          infected = input$infected,
          recovered = input$recovered,
          dead = input$dead))
    visibleInputs <-
      relevantInputs[!(names(relevantInputs) %in% input$hiddenInputs)]
    stopifnot(is.list(visibleInputs))
    append(visibleInputs, input$replicates)
  })

  ## NOTE: prevent the reactive value from invalidating renderModel too often,
  ## especially when a slider or numeric input is scrolling through values and
  ## hasn't truly idled on one value yet. See the following link for more
  ## information: https://shiny.posit.co/r/reference/shiny/1.7.2/debounce.html.
  visibleInputs <- debounce(greedy_visibleInputs, 500)

  observe({ print(req(visibleInputs())) })

  settings <- reactive({
    compartments <- strsplit(compartmentalModel(), "")[[1]]

    # Generate list of colors
    plotSettings_colors <- sapply(compartments, function(compartment) {
      input[[paste0(compartment, "PlotSettings_color")]]
    }, simplify = FALSE, USE.NAMES = FALSE)

    list(
      plotSettings_title = input$plotSettings_title,
      phasePlanePlotSettings_title = input$phasePlanePlotSettings_title,
      plotSettings_xAxisLabel = input$plotSettings_xAxisLabel,
      plotSettings_yAxisLabel = input$plotSettings_yAxisLabel,
      phasePlanePlotSettings_xAxisLabel = input$phasePlanePlotSettings_xAxisLabel,
      phasePlanePlotSettings_yAxisLabel = input$phasePlanePlotSettings_yAxisLabel,
      plotSettings_colors = plotSettings_colors,
      phasePlanePlotSettings_color = input$phasePlanePlotSettings_color
    )
  })

  renderModel <- reactive({
    msg <- "The compartment values (except D) must sum to N before simulating."
    shiny::validate(need(compartmentsEqualPopulation(), message = msg),
                    need(recoveryRatePositive(),
                         message = "Recovery rate must be positive."))

    msg <- "Reactive value propagating... please wait."
    if (exposedCompartmentInModel()) shiny::validate(need(input$exposed,
                                                          message = msg))
    if (deadCompartmentInModel()) shiny::validate(need(input$dead,
                                                       message = msg))
    plotterType <- "normal"
    modelResults <-
      if (input$stochastic == 1) {
        if (input$distribution == 0) {
          doCall(uniformSI, args = visibleInputs())
        } else {
          plotterType <- "binomial"
          doCall(binomialSI, args = visibleInputs())
        }
      } else {
        {
          doCall(ehpi:::epi, args = visibleInputs())
        } |>
          select(c(time, N, matches(str_split_1(compartmentalModel(), ""))))
      }

    plotSettings <- settings()[grep("^plotSettings_", names(settings()))]
    names(plotSettings) <- sub("^plotSettings_", "", names(plotSettings))

    phasePlanePlotSettings <- settings()[grep("^phasePlanePlotSettings_", names(settings()))]
    names(phasePlanePlotSettings) <- sub("^phasePlanePlotSettings_", "", names(phasePlanePlotSettings))

    model <- list(data = modelResults, selectedModel = compartmentalModel(), plotterType = plotterType)
    mainPlot <- ggplotly(plotter(model, plotSettings)) %>%
      layout(xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE))

    phaseplanePlot <- ggplotly(phasePlanePlotterSI(model, phasePlanePlotSettings)) %>%
      layout(xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE))

    subPlots <- subPlotter(model) |>
      map(\(plot, index) {
        column(
          6, ggplotly(plot) %>%
            layout(xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE)),
          br()
        )
      })

    modelDataTable <- if (plotterType == "binomial") {
      numericColumns <- sapply(modelResults, is.numeric)
      modelResults[numericColumns] <- round(modelResults[numericColumns], 2)
      datatable(modelResults, rownames = FALSE)
    } else {
      datatable(round(modelResults, 2), rownames = FALSE)
    }
    #modelDataTable <- datatable(round(modelResults, 2), rownames = FALSE)

    # FIX: vital dynamics error
    modelLatex <- div(
       generate_latex(c(r"(\textbf{MATHEMATICAL MODELS})")) |> helpText() |> withMathJax(),
       if (input$stochastic == 1) {
         doCall(renderStochasticModelLaTex, args = visibleInputs())
         tagList(
            doCall(renderStochasticModelLaTex, args = visibleInputs()),
            generate_latex(c(r"(\textbf{MODEL EXPLANATION})")) |> helpText() |> withMathJax(),
            doCall(renderStochasticDescription, args = visibleInputs())
          )
        } else {
          doCall(renderModelLaTeX, args = visibleInputs())
        },
        r"(\textbf{Compartmental Models})",
        img(
          src = paste0("images/", req(compartmentalModel()), ".svg"),
          contentType = "image/svg",
          width = "45%",
          alt = gsub("\n[\t\ ]+?", " ",
                     r"(The diagram of the model compartments failed to load, or
                      the accessibility text is being read by a screen
                      reader.)"))
      )

    return(list(
      modelLatex = modelLatex,
      modelResults = modelResults,
      mainPlot = mainPlot,
      subPlots = subPlots,
      phaseplanePlot = phaseplanePlot,
      modelDataTable = modelDataTable
    ))
  })

  defaultColors <- c(
    "#9467bd", "#2ca02c", "#ff7f0e", "#1f77b4", "#d62728",
    "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
  )

  output$plot <- renderUI({
    renderModel()$mainPlot
  })
  output$plotSettings_colorPickers <- renderUI({
    compartments <- strsplit(compartmentalModel(), "")[[1]]
    numCompartments <- length(compartments)

    lapply(1:numCompartments, function(i) {
      colourInput(
        inputId = paste0(compartments[i], "PlotSettings_color"),
        label = strong(paste(compartments[i], "Plot Color")),
        value = defaultColors[i]
      )
    })
  })

  output$phasePlanePlotSettings_colorPickers <- renderUI({
    # this is an si phase plane so only 1 color
    colourInput(
      inputId = "phasePlanePlotSettings_color",
      label = strong("Phase Plane Plot Color"),
      value = defaultColors[1]
    )
  })

  output$phasePlanePlot <- renderUI(renderModel()$phaseplanePlot)
  output$subPlots <- renderUI(div(fluidRow(renderModel()$subPlots)))

  output$outputSummary <- renderUI(div(
    style = "display: flex; flex-direction: column;",
    renderModel()$modelDataTable, downloadButton("downloadData", "Download as Excel",
      style = "align-self: flex-start; margin-top: 1vh;"
    )))
  output$downloadData <-
    downloadHandler(
      \() paste0(compartmentalModel(), "_Model_Summary", Sys.Date(), ".xlsx"),
      \(file) write_xlsx(renderModel()$modelResults, file)
    )
  output$mathematicalModel <- renderUI(renderModel()$modelLatex)

  updateTextAndColourInputs <- function() {
    for (id in names(input)) {
      if (grepl("Settings_", id)) {
        updateTextInput(session, id, value = "")
        ## TODO: this does not work, need to find away to reset
        ## colors without relying on the settings reactive
        updateColourInput(session, id, value = "")
      }
    }
  }

  ## When the user presses the reset button the numeric inputs are reset to the
  ## default values available for the model compartments' parameters and
  ## variables, and the selected model options.
  ##
  ## DONT try to combine these; the UX-logic is as it should be with these two
  ## observers.
  observe({
    isolate(updateNumericInputs(defaults(), session))
    updateTextAndColourInputs()
  }) |> bindEvent(input$resetNumericInputs)
  ## DONT try to combine these; the UX-logic is as it should be with these two
  ## observers.
  observe({
    if (input$freeze == TRUE) {
      updateTextAndColourInputs()
      isolate(updateNumericInputs(defaults(), session))
    }
  }) |> bindEvent(input$freeze)

  ## DONT change the return value in the affirmative case (NULL) is ill-advised.
  ## You should know what you're doing before mucking about with this.
  inputsValid <- reactive({
    input # Depend on all inputs.
    if (globalValidator$is_valid()) {
      NULL
    } else {
      paste("All the required inputs for this model must be valid.",
            "Check the warnings and errors on the input widgets.")
    }
  })

  compartmentsEqualPopulation <- reactive({
    shiny::validate(need(compartmentalModel(), "A model must be selected."),
                    need(input$population, "A population is required."),
                    need(input$susceptible, "A number of people must be susceptible."),
                    need(input$exposed, "A number of people must be exposed."),
                    need(input$infected, "A number of people must be infected."),
                    need(input$recovered, "A number of people must be recovered."))
    variables <- c(input$population,
                   input$susceptible,
                   input$exposed,
                   input$infected,
                   input$recovered)
    names(variables) <- c("N", "S", "E", "I", "R")
    applicableVariables <-
      variables[names(variables) %in%
                  unique(str_split(req(compartmentalModel()), "")[[1]])]
    boolean <- !(input$population == sum(applicableVariables))
    stopifnot(length(boolean) == 1)
    message <- "Population must be equal to the sum of the initial compartments values!"
    feedbackDanger("population", boolean, message)
    if(boolean) NULL else message
  })

  ## TODO: if a SI or SEI model without an R compartment is ever introduced then
  ## this needs to be modified.
  recoveryRatePositive <- reactive({
    print(exposedCompartmentInModel())
    if (exposedCompartmentInModel()) {
      message <- "Sigma must be greater than zero when a recovered compartment exists."
      boolean <- req(input$sigma) == 0
      feedbackDanger("sigma", boolean, message)
    } else {
      message <- "Gamma must be greater than zero when a recovered compartment exists."
      boolean <- req(input$gamma) == 0
      feedbackDanger("gamma", boolean, message)
    }
    if (boolean) NULL else message
  })
}
