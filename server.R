server <- function(input, output, session) {
  documentReadyPainter() # Restyle some elements with JavaScript.

  globalValidator <- addRuleListToValidator(
    InputValidator$new(),
    filter(rules, is.na(model))[, 2][[1]][[1]]
  )

  validatorsAndLambdas <-
    filter(rules, !is.na(model)) |>
    rowwise() |>
    mutate(
      vld = list(addRuleListToValidator(InputValidator$new(), ruleList)),
      lambda = list(eval(bquote(\() grepl(.(model), input$modelSelect))))) |>
    select(vld, lambda)

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
    defaultInputValues |>
      filter(
        modelType == req(input$modelSelect),
        ## WARN DONT: compare numerical booleans following C style; using
        ## logicals in any way—coercing numeric to logical on the RHS or using
        ## logical values in the defaultInputValues—seems to bork everything.
        stochastic == input$stochastic,
        vitalDynamics == input$vitalDynamics,
        massAction == input$trueMassAction
      ) |>
      select(beta:replicates) |>
      select(where(\(x) all(!is.na(x))))
  })

  ## Whenever the model selection changes, the widget values throughout the
  ## application are updated according to the defaults specified for the model
  ## configuration in the defaultInputValues.xlsx spreadsheet in the data/
  ## project subfolder.
  observeEvent(input$modelSelect, {
    req(defaults())
    isolate({
      exposedEnabled <- substr(input$modelSelect, 2, 2) == "E"
      output$beta <- renderUI({
        rateLabel <- { if (exposedEnabled) "exposure" else "infection" }
        defaultValue <- select(defaults(), beta) |> as.numeric()
        stopifnot(is.numeric(defaultValue) && length(defaultValue) == 1)
        numericInputWithMathJax("beta", rateLabel, defaultValue)
      })
      output$gamma <- renderUI({
        rateLabel <- { if (exposedEnabled) "infection" else "recovery" }
        defaultValue <- select(defaults(), gamma) |> as.numeric()
        stopifnot(is.numeric(defaultValue) && length(defaultValue) == 1)
        numericInputWithMathJax("gamma", rateLabel, defaultValue)
      })
      updateNumericInputs(defaults(), session)
    })
  })

  ## A reactive value like input, but with hidden and irrelevant inputs removed.
  visibleInputs <- reactive({
    allInputs <- reactiveValuesToList(input)
    nameAmongHiddenInputs <-
      names(allInputs) %in% c(allInputs$hiddenInputs, "hiddenInputs", "go", "resetAll")
    visibleInputs <- allInputs[!nameAmongHiddenInputs]
    stopifnot(is.list(visibleInputs))
    visibleInputs
  })

  renderModel <- reactive({
    shiny::validate(inputsValid(), compartmentsEqualPopulation())

    modellingFunctions <- mget(
      paste0(
        c("plot", "plotSubPlots", "plotPhasePlane"),
        input$modelSelect
      ),
      envir = environment(exposuRe),
      mode = "function"
    )
    modelPlotter <- modellingFunctions[[1]]
    modelSubPlotter <- modellingFunctions[[2]]
    modelPhasePlanePlotter <- modellingFunctions[[3]]

    modelResults <-
      doCall(exposuRe, args = isolate(visibleInputs())) |>
      select(c(time, N, matches(str_split_1(input$modelSelect, ""))))

    output$downloadData <-
      downloadHandler(
        \() paste0(input$modelSelect, "_Model_Summary", Sys.Date(), ".xlsx"),
        \(file) write_xlsx(modelResults, file)
      )

    mainPanel(
      id = "outputPanel",
      tabsetPanel(
        id = "tabSet",
        tabPanel("Plot",
                 ggplotly(modelPlotter(modelResults)),
                 {
                   modelSubPlotter(modelResults) |>
                     imap(\(plot, index) {
                       plotName <- paste0("subplot.", index)
                       output[[plotName]] <- renderPlotly({
                         ggplotly(plot)
                       })
                       column(6, plotlyOutput(plotName))
                     }) |>
                     fluidRow()
                 }),
        tabPanel("Phase Plane", ggplotly(modelPhasePlanePlotter(modelResults))),
        tabPanel("Output Summary",
                 ## FIXME: previously, only the button had to be added to the tabpanel
                 ## but now doing so places the button behind the datatable. I have
                 ## added some custom CSS to fix this for now.
                 div(style = "display: flex; flex-direction: column;",
                     datatable(round(modelResults, 2),
                               options = list(dom = "lprti", pageLength = 50),
                               rownames = FALSE),
                     downloadButton("downloadData",
                                    "Download as Excel",
                                    style = "align-self: flex-start; margin-top: 1vh;"))),
        tabPanel("Mathematical Model",
                 doCall(renderModelLaTeX, args = isolate(visibleInputs())),
                 tagList(img(
                   src = paste0("images/", input$modelSelect, ".svg"),
                   contentType = "image/svg",
                   width = "420px",
                   alt = gsub("\n[\t\ ]+?", " ", r"(The diagram of the model compartments
                     failed to load, or the accessibility text is being read by
                     a screen reader.)")))),
        # tabPanel("Basic Reproduction Number (R0)",
        #          disabled = "",
        #          class = "disabled")
        ))
  })


  output$outputPanel <- renderUI({
    renderModel()
  })

  observeEvent(input$resetAll, {
    updatePickerInput(session, "modelSelect", selected = "")
    updateNumericInputs(defaults(), session)
  })

  inputsValid <- reactive({
    input # Depend on all inputs.
    if (globalValidator$is_valid()) {
      NULL
    } else {
      "All the required inputs for this model must be valid. Check the warnings and errors on the input widgets."
    }
  })

  compartmentsEqualPopulation <- reactive({
    shiny::validate(need(input$modelSelect, "A model must be selected."))
    variables <- c(input$population,
                   input$susceptible,
                   input$exposed,
                   input$infected,
                   input$recovered)
    names(variables) <- c("N", "S", "E", "I", "R")
    applicableVariables <-
      variables[names(variables) %in%
                  unique(str_split(input$modelSelect, "")[[1]])]
    boolean <- input$population == sum(applicableVariables)
    stopifnot(length(boolean) == 1)
    message = "Population must be equal to the sum of the initial compartments values!"
    feedbackDanger("population", !boolean, message)
    if(boolean) NULL else message
  })
}
