server <- function(input, output, session) {
  ## TODO: re-enable the button after binomial implementation is completed.
  ## disable(selector = "input[name='distribution'][value='1']")

  observe_helpers(withMathJax = TRUE,
                  help_dir = "www/markdown")

  ## Restyle some elements with JavaScript.
  runjs(r"[$(document).ready($('#modelSelect + button').removeClass('btn-light'))]")
  r"($(document)
     .ready($('div:has(> #modelSelect)')
            .css({
            'border':'var(--bs-border-width) solid #8D959E',
            'border-radius':'var(--bs-border-radius)',
            'transition':'border-color 0.15s ease-in-out,box - shadow 0.15 s ease - in - out '
            })
     )
    )" |>
     gsub(pattern = r"(\n\s*)", replacement = "") |>
     runjs()

  ## NOTE: Disable the stochastic radio button and the sidebar to toggle
  ## between sub-apps, respectively. TODO: When these are implemented the
  ## respective line can be removed.
  #disable(selector = "#stochastic > div:nth-child(2) > label:nth-child(2) > input:nth-child(1)")
  disable(selector = "button.collapse-toggle") # sidebar button
  runjs(r"--($('button.collapse-toggle').hide())--") # sidebar button

  observe({
    if (grepl("E", input$modelSelect)) {
      hideTab(inputId = "tabs", target = "phasePlane")
    } else {
      showTab(inputId = "tabs", target = "phasePlane")
    }
  })
  
  observe({
    print(input$modelSelect)
    if (input$modelSelect != "SIR") { 
      disable(selector = "input[name='stochastic'][value='1']")
    }
  })

  observe({
    if (input$stochastic == 1 && input$distribution == 0) {
      disable(selector = "input[name='trueMassAction'][value='1']")
    } else {
      enable(selector = "input[name='trueMassAction'][value='1']")
    }
  })
  
  observe({
    if (input$stochastic == 1) {
      hideTab(inputId = "tabs", target = "outputSummary")
    } else {
      showTab(inputId = "tabs", target = "outputSummary")
    }
  })


  globalValidator <- addRuleListToValidator(
    InputValidator$new(),
    filter(rules, is.na(model))[, 2][[1]][[1]]
  )

  validatorsAndLambdas <-
    filter(rules, !is.na(model)) |>
    rowwise() |>
    mutate(vld = list(addRuleListToValidator(InputValidator$new(), ruleList)),
           lambda = list(eval(bquote(\() grepl(.(model), input$modelSelect)))),
           .keep = "none")

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

  ## All models have at least one set of default values, a "fallback". Some
  ## models have more than one set which is specific to the model and its
  ## configuration.
  defaults <- reactive({
    filter(defaultInputValues,
           modelType == req(input$modelSelect),
           trueMassAction == input$trueMassAction,
           vitalDynamics == input$vitalDynamics) ->
      modelSpecific

    if (dim(modelSpecific)[1] > 1) {
      ## NOTE: stochastic filtering works as intended.
      configurationSpecific <-
        modelSpecific |>
        mutate(stochasticTruthy = map(stochastic, shiny::isTruthy)) |>
        filter(stochasticTruthy == input$stochastic,
               stochastic == input$distribution)

      # If there is exactly one set of default values for this exact model (i.e.
      # with its configuration) return that, otherwise return the fallback set
      # of values.
      firstDimensionLength <- dim(configurationSpecific)[1]
      if (firstDimensionLength != 1) {
        if (firstDimensionLength > 1) {
          warning(
            print(sprintf(
              r"(There are %s configuration-specific sets of default values.
That's an error!
A warning is returned because a fallback set of values was used instead.)",
firstDimensionLength)))
        }

        ## Return the set of fallback values.
        filter(modelSpecific,
               is.na(stochastic),
               vitalDynamics == input$vitalDynamics,
               trueMassAction == input$trueMassAction) |>
          ## NOTE: deselect the model options columns and empty columns because
          ## only parameters and variables are used to update the numeric input
          ## widgets.
          select(!c(trueMassAction,
                    vitalDynamics,
                    stochastic)) |>
        select(where(\(x) all(!is.na(x))))
      } else {
        ## There is exactly one observation of default values for this exact
        ## model configuration; return it after removing the model configuration
        ## columns used for filtering (retain just variables and parameter
        ## columns).
        configurationSpecific |>
          select(!c(trueMassAction,
                    vitalDynamics,
                    stochastic,
                    stochasticTruthy)) |>
          select(where(\(x) all(!is.na(x))))
      }
    } else {
      warning("dim(modelSpecific)[1] == 1: ", dim(modelSpecific)[1] == 1)
      ## There is exactly one set of default values for this model, and it can
      ## be returned immediately.
      defaultInputValues |>
        filter(vitalDynamics == 0,
               trueMassAction == 0,
               is.na(stochastic),
               modelType == input$modelSelect) |>
        select(!c(vitalDynamics,
                  trueMassAction,
                  stochastic)) |>
        select(where(\(x) all(!is.na(x))))
    }
  })

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
                                if (input$freezeUpdatingOfInputWidgetValuesWithDefaults == FALSE)
                                  rateValues$beta
                                else
                                  isolate(input$beta)),
        numericInputWithMathJax("gamma",
                                rateLabels["gamma"],
                                if (input$freezeUpdatingOfInputWidgetValuesWithDefaults == FALSE)
                                  rateValues$gamma
                                else
                                  isolate(input$gamma))
      )
    })

    if (input$freezeUpdatingOfInputWidgetValuesWithDefaults == FALSE)
      updateNumericInputs(defaults(), session)
  }) |> bindEvent({ defaults() })

  ## A reactive value like input, but with hidden and irrelevant inputs removed.
  greedy_visibleInputs <- reactive({
    shiny::validate(need(input$modelSelect, "A model must be selected."))
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
    visibleInputs
  })

  ## NOTE: prevent the reactive value from invalidating renderModel too often,
  ## especially when a slider or numeric input is scrolling through values and
  ## hasn't truly idled on one value yet. See the following link for more
  ## information: https://shiny.posit.co/r/reference/shiny/1.7.2/debounce.html.
  visibleInputs <- debounce(greedy_visibleInputs, 500)

  settings <- reactive({
    modelSelect <- input$modelSelect
    compartments <- strsplit(modelSelect, "")[[1]]

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
          doCall(exposuRe, args = visibleInputs())
        } |>
          select(c(time, N, matches(str_split_1(input$modelSelect, ""))))
      }
      
    plotSettings <- settings()[grep("^plotSettings_", names(settings()))]
    names(plotSettings) <- sub("^plotSettings_", "", names(plotSettings))

    phasePlanePlotSettings <- settings()[grep("^phasePlanePlotSettings_", names(settings()))]
    names(phasePlanePlotSettings) <- sub("^phasePlanePlotSettings_", "", names(phasePlanePlotSettings))

    model <- list(data = modelResults, selectedModel = input$modelSelect, plotterType = plotterType)
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
       } else if (input$stochastic == 0){
         doCall(renderModelLaTeX, args = visibleInputs()) },
       generate_latex(c(r"(\textbf{COMPARTMENT DIAGRAM})")) |> helpText() |> withMathJax(),
       tagList(img(
         src = paste0("images/", input$modelSelect, ".svg"),
         contentType = "image/svg",
         width = "45%",
         alt = gsub("\n[\t\ ]+?", " ", r"(The diagram of the model compartments
                      failed to load, or the accessibility text is being read by
                      a screen reader.)")
       ))
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
    modelSelect <- input$modelSelect
    compartments <- strsplit(modelSelect, "")[[1]]
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
      \() paste0(input$modelSelect, "_Model_Summary", Sys.Date(), ".xlsx"),
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
    if (input$freezeUpdatingOfInputWidgetValuesWithDefaults == TRUE) {
      updateTextAndColourInputs()
      isolate(updateNumericInputs(defaults(), session))
    }
  }) |> bindEvent(input$freezeUpdatingOfInputWidgetValuesWithDefaults)

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

  exposedCompartmentInModel <-
    reactive("E" %in% str_split(req(input$modelSelect), "")[[1]])

  deadCompartmentInModel <-
    reactive("D" %in% str_split(req(input$modelSelect), "")[[1]])

  compartmentsEqualPopulation <- reactive({
    shiny::validate(need(input$modelSelect, "A model must be selected."),
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
                  unique(str_split(input$modelSelect, "")[[1]])]
    boolean <- !(input$population == sum(applicableVariables))
    stopifnot(length(boolean) == 1)
    message <- "Population must be equal to the sum of the initial compartments values!"
    feedbackDanger("population", boolean, message)
    if(boolean) NULL else message
  })

  ## TODO: if a SI or SEI model without an R compartment is ever introduced then
  ## this needs to be modified.
  recoveryRatePositive <- reactive({
    if(exposedCompartmentInModel()) {
      message <- "Sigma must be greater than zero when a recovered compartment exists."
      boolean <- req(input$sigma) == 0
      feedbackDanger("sigma", boolean, message)
      #Temporarily disable the stochastic option for SEI model type
      #TODO: Will re-enable the button once the implementation is done
      disable(selector = "input[name='stochastic'][value='1']")
    } else {
      message <- "Gamma must be greater than zero when a recovered compartment exists."
      boolean <- req(input$gamma) == 0
      feedbackDanger("gamma", boolean, message)
      
      #TODO: Will remove this line when all the stochastic implementation is done for all models
      enable(selector = "input[name='stochastic'][value='1']")
    }

    if(boolean) NULL else message
  })
  
}
