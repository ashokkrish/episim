server <- function(input, output, session) {
  #Temporarily disable the binomial option
  #TODO: Will re-enable the button once binomial implementation is done
  disable(selector = "input[name='distribution'][value='1']")
  
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

  defaults <- reactive({
    filter(defaultInputValues, modelType == req(input$modelSelect)) |>
      select(where(\(x) all(!is.na(x))))
  })

  ## Whenever the model selection changes, the widget values throughout the
  ## application are updated according to the defaults specified for the model
  ## configuration in the defaultInputValues.xlsx spreadsheet in the data/
  ## project subfolder.
  ## MAYBE TODO: this could possibly be an observe... maybe a bindEvent?
  observeEvent(input$modelSelect, {
    req(defaults())
    ## MAYBE TODO: these can be combined into a single UI rendering.
    isolate({
      output$beta <- renderUI({
        rateLabel <- { if (exposedCompartmentInModel()) "exposure" else "infection" }
        defaultValue <- select(defaults(), beta) |> as.numeric()
        stopifnot(is.numeric(defaultValue) && length(defaultValue) == 1)
        numericInputWithMathJax("beta", rateLabel, defaultValue)
      })
      output$gamma <- renderUI({
        rateLabel <- { if (exposedCompartmentInModel()) "infection" else "recovery" }
        defaultValue <- select(defaults(), gamma) |> as.numeric()
        stopifnot(is.numeric(defaultValue) && length(defaultValue) == 1)
        numericInputWithMathJax("gamma", rateLabel, defaultValue)
      })
    })
    updateNumericInputs(defaults(), session)
  })

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
    visibleInputs <- relevantInputs[!(names(relevantInputs) %in% input$hiddenInputs)]
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
    numCompartments <- length(compartments)

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
          doCall(binomialSI, args = visibleInputs())
          plotterType <- "binomial"
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

    modelDataTable <- datatable(round(modelResults, 2), rownames = FALSE)

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
    )
  ))
  output$downloadData <-
    downloadHandler(
      \() paste0(input$modelSelect, "_Model_Summary", Sys.Date(), ".xlsx"),
      \(file) write_xlsx(renderModel()$modelResults, file)
    )
  output$mathematicalModel <- renderUI(renderModel()$modelLatex)


  observeEvent(input$resetAll, {
    updatePickerInput(session, "modelSelect", selected = "")
    updateNumericInputs(defaults(), session)
    for (id in names(input)) {
      if (grepl("Settings_", id)) {
        updateTextInput(session, id, value = "")
        # TODO: this does not work, need to find away to reset
        # colors without relying on the settings reactive
        updateColourInput(session, id, value = "")
      }
    }
  })

  inputsValid <- reactive({
    input # Depend on all inputs.
    if (globalValidator$is_valid()) {
      NULL
    } else {
      "All the required inputs for this model must be valid. Check the warnings and errors on the input widgets."
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
