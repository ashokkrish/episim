server <- function(input, output, session) {
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
  ## disable(selector = "#stochastic > div:nth-child(2) > label:nth-child(2) > input:nth-child(1)")
  disable(selector = "button.collapse-toggle") # sidebar button
  runjs(r"--($('button.collapse-toggle').hide())--") # sidebar button

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
        ## DEPRECATED DONT: removing this lines may be nice because it is
        ## commented code, but leaving them informs future programmers better.
        ## These conditions are no longer used because the values are no longer
        ## relevant and there is not columns for these values in the dataframe
        ## or spreadsheet any longer.
        ##
        ## stochastic == input$stochastic,
        ## vitalDynamics == input$vitalDynamics,
        ## massAction == input$trueMassAction
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
  greedy_visibleInputs <- reactive({
    shiny::validate(need(input$modelSelect, "A model must be selected."))
    ## TODO: this could be imporoved, but I'm not sure how. This is kinda awful and should be built-in to Shiny.
    relevantInputs <-
      reactiveValuesToList(
        reactiveValues(
          modelSelect    = input$modelSelect,
          trueMassAction = input$trueMassAction,
          stochastic     = input$stochastic,
          vitalDynamics  = input$vitalDynamics,
          replicates     = input$replicates,
          rerun          = input$rerunStochasticSimulation, # MAYBE
          muBirth        = input$muBirth,
          muDeath        = input$muDeath,
          timesteps      = input$timesteps,
          beta           = input$beta,
          gamma          = input$gamma,
          delta          = input$delta,
          sigma          = input$sigma,
          xi             = input$xi,
          population     = input$population,
          susceptible    = input$susceptible,
          exposed        = input$exposed,
          infected       = input$infected,
          recovered      = input$recovered,
          dead           = input$dead))
    visibleInputs <- relevantInputs[!(names(relevantInputs) %in% input$hiddenInputs)]
    stopifnot(is.list(visibleInputs))
    visibleInputs
  })

  ## NOTE: prevent the reactive value from invalidating renderModel too often,
  ## especially when a slider or numeric input is srolling through values and
  ## hasn't truly idled on one value yet. See the following link for more
  ## information: https://shiny.posit.co/r/reference/shiny/1.7.2/debounce.html.
  visibleInputs <- debounce(greedy_visibleInputs, 1250)

  renderModel <- reactive({
    msg <- "The compartment values (except D) must sum to N before simulating."
    shiny::validate(need(compartmentsEqualPopulation(), message = msg),
                    need(recoveryRatePositive(),
                         message = "Recovery rate must be positive."))

    ## BEIGN TODO: obsolete this with a refactoring.
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
    ## END TODO: obsolte this with a refactoring.

    modelResults <-
      doCall(exposuRe, args = visibleInputs()) |>
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
                 br(),
                 ggplotly(modelPlotter(modelResults)),
                 fluidRow(
                   modelSubPlotter(modelResults) |>
                     imap(\(plot, index) {
                       column(6, ggplotly(plot))
                     })
                  )),
        tabPanel("Phase Plane", br(), ggplotly(modelPhasePlanePlotter(modelResults))),
        tabPanel("Output Summary",
                 br(),
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
                 br(),
                 doCall(renderModelLaTeX, args = visibleInputs()),
                 tagList(img(
                   src = paste0("images/", input$modelSelect, ".svg"),
                   contentType = "image/svg",
                   width = "420px",
                   alt = gsub("\n[\t\ ]+?", " ", r"(The diagram of the model compartments
                     failed to load, or the accessibility text is being read by
                     a screen reader.)")))),
        ## tabPanel("Basic Reproduction Number (\(R_0\))")
        ))
  })

  output$outputPanel <- renderUI(renderModel())

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
    boolean <- !(input$population == sum(applicableVariables))
    stopifnot(length(boolean) == 1)
    message <- "Population must be equal to the sum of the initial compartments values!"
    feedbackDanger("population", boolean, message)
    if(boolean) NULL else message
  })

  ## TODO: if a SI or SEI model without an R compartment is ever introduced then
  ## this needs to be modified.
  recoveryRatePositive <- reactive({
    if("E" %in% str_split(input$modelSelect, "")[[1]]) {
      message <- "Sigma must be greater than zero when a recovered compartment exists."
      boolean <- input$sigma == 0
      feedbackDanger("sigma", boolean, message)
    } else {
      message <- "Gamma must be greater than zero when a recovered compartment exists."
      boolean <- input$gamma == 0
      feedbackDanger("gamma", boolean, message)
    }

    if(boolean) NULL else message
  })
}
