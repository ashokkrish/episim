server <- function(input, output, session) {
  observe_helpers(withMathJax = TRUE, help_dir = "www/markdown")

  ## REACT CHART EDITOR
  addKeys("dismissEditor", c("escape", "shift+c"))
  ## When the user clicks the edit plot button the editButtonClicked event will
  ## be created, and then the main panel should be hidden until the user presses
  ## the escape key or presses a button.
  observe({
    shinyjs::hide(selector = "#simulation")
    shinyjs::show(selector = "#chart-editor-container")
    update_plotly_editor(session,
                         "react-chart-editor",
                         ## TODO: edit the reactcharteditoR package so that the
                         ## following input name is customizable.
                         configuration = list(plotId = input$editButtonClicked),
                         value = list())
  })
  observe({
    shinyjs::hide(selector = "#chart-editor-container")
    shinyjs::show(selector = "#simulation")
  }) %>% bindEvent(input$dismissEditor)

  model <- reactive({ str_split_1(req(input$modelSelect), "_") })
  compartmentalModel <- reactive({ model()[[1]] })
  publication <- reactive({
    name <- model()[[2]]
    if (length(name) == 1) NA else name
  })

  ## Disable uniform stochastic in all situations.
  disable(selector = "input[name='distribution'][value='0']")

  exposedCompartmentInModel <- reactive({ str_detect(compartmentalModel(), "E") })
  deadCompartmentInModel <- reactive({ str_detect(compartmentalModel(), "D") })

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
        if (input$trueMassAction == 1) {
          disable(selector = "input[name='distribution'][value='0']")
          updateRadioButtons(inputId = "distribution", selected = 1)
        } else {
          ## Never enable the uniform distribution.
          ## enable(selector = "input[name='distribution'][value='0']")
        }
      }
    }
  })

  observe(do.call(if (exposedCompartmentInModel()) hideTab else showTab,
                  list(inputId = "tabs", target = "phasePlane")))
  observe(do.call(if (input$stochastic == 1) hideTab else showTab,
                  list(inputId = "tabs", target = "outputSummary")))

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

  ## FIXME: All models have at least one set of default values, a "fallback".
  ## Some models have more than one set which is specific to the model and its
  ## configuration. TODO: filter and obtain the defaults differently when the
  ## model selected is part of the Ashok defaults.
  defaults <- reactive({
    modelSpecific <-
      filter(defaultInputValues,
             modelType == req(compartmentalModel()),
             trueMassAction == input$trueMassAction,
             vitalDynamics == input$vitalDynamics)

    if (is.na(publication())) {
      modelSpecific %<>%
        filter(is.na(readablePublicationName))
    } else {
      modelSpecific %<>%
        filter(readablePublicationName == publication())
    }

    if (input$stochastic == 0) {
      modelSpecific %<>%
        filter(is.na(stochastic))
    } else {
      modelSpecific %<>%
        filter(stochastic == case_when(input$distribution == 0 ~ "uniform",
                                       input$distribution == 1 ~ "binomial"))
    }

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
                            modelType == compartmentalModel(),
                            is.na(trueMassAction),
                            is.na(vitalDynamics),
                            is.na(stochastic))
    }

    modelSpecific |>
      select(!c(vitalDynamics,
                trueMassAction,
                stochastic))
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
        if (exposedCompartmentInModel())
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
    msg <- "The compartment values (except D) must sum to N before simulating."
    shiny::validate(need(compartmentsEqualPopulation(), message = msg),
                    need(recoveryRatePositive(),
                         message = "Recovery rate must be positive."))

    selectedModel <- compartmentalModel()
      reactiveValuesToList(
        reactiveValues(
          trueMassAction = input$trueMassAction,
          stochastic = input$stochastic,
          distribution = input$distribution,
          vitalDynamics = input$vitalDynamics,
          muBirth = input$muBirth,
          muDeath = input$muDeath,
          timesteps = input$timesteps,

          rerun = input$rerunStochasticSimulation,

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
          dead = input$dead)) %>%
    subset(not(is_in(names(.), input$hiddenInputs))) %>%
      ## FIXME: why is it necessary to put replicates here?
      append(list(replicates = input$replicates)) %>%
      append(list(modelSelect = compartmentalModel())) %>%
      subset(not(is.na(.)))
  })

  ## NOTE: prevent the reactive value from invalidating renderModel too often,
  ## especially when a slider or numeric input is scrolling through values and
  ## hasn't truly idled on one value yet. See the following link for more
  ## information: https://shiny.posit.co/r/reference/shiny/1.7.2/debounce.html.
  visibleInputs <- debounce(greedy_visibleInputs, 500)

  output$outputPanel <- renderUI({
    msg <- "Reactive value propagating... please wait."
    if (exposedCompartmentInModel()) shiny::validate(need(input$exposed,
                                                          message = msg))
    if (deadCompartmentInModel()) shiny::validate(need(input$dead,
                                                       message = msg))
    modelResults <-
      if (input$stochastic == 1) {
        shiny::validate(need(input$replicates,
                             message = "Replicates are needed."))
        ## MAYBE TODO FIXME: this doesn't seem to have an effect! It is strange
        ## that this check is needed here to prevent an occurrence of #98 with
        ## respect to input$replicates, given that it immediately follows a
        ## shiny::validate & need call. req(input$replicates, cancelOutput =
        ## "progress")
        doCall(if (input$distribution == 0) uniformSI else binomialSI,
               args = req(visibleInputs()))
      } else {
        doCall(epi, args = req(visibleInputs())) %>%
          select(c(time, N, matches(str_split_1(compartmentalModel(), ""))))
      }

    editBtn <- list(
      name = "Edit",
      icon = list(
        path = "M7.127 22.562l-7.127 1.438 1.438-7.128 5.689 5.69zm1.414-1.414l11.228-11.225-5.69-5.692-11.227 11.227 5.689 5.69zm9.768-21.148l-2.816 2.817 5.691 5.691 2.816-2.819-5.691-5.689z",
        transform = 'scale(0.7)'
      ),
      click = htmlwidgets::JS("
        function(gd) {
          Shiny.setInputValue('editButtonClicked', gd.id);
        }
      ")
    )

    model <- list(data = modelResults,
                  selectedModel = compartmentalModel(),
                  plotterType =
                    if (all(input$stochastic == 1, input$distribution == 1))
                      "binomial"
                    else
                      "normal")

    mainPlot <- ggplotly(plotter(model)) %>%
      layout(xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE)) %>%
      config(modeBarButtonsToAdd = list(editBtn))


    phaseplanePlot <- ggplotly(phasePlanePlotterSI(model)) %>%
      layout(xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE)) %>%
      config(modeBarButtonsToAdd = list(editBtn))

    subPlots <- subPlotter(model) |>
      map(\(plot, index) {
        column(6,
               ggplotly(plot) %>%
               layout(xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE)) %>%
               config(modeBarButtonsToAdd = list(editBtn)),     
               br())
      })

    if (input$stochastic != 1) {
      modelDataTable <-
        modelResults[sapply(modelResults, is.numeric)] %>%
        round(2) %>%
        datatable(rownames = FALSE)
    }

    # FIX: vital dynamics error
    modelLatex <- withMathJax(div(
      generate_latex(c(r"(\textbf{MATHEMATICAL MODELS})")),
      if (input$stochastic == 1) {
        doCall(renderStochasticModelLaTex, args = visibleInputs())
        tagList(
          doCall(renderStochasticModelLaTex, args = visibleInputs()),
          generate_latex(c(r"(\textbf{MODEL EXPLANATION})")),
          doCall(renderStochasticDescription, args = visibleInputs())
        )
      } else {
        doCall(renderModelLaTeX, args = visibleInputs())
      },
      generate_latex(c(r"(\textbf{COMPARTMENTAL MODELS})")),
      img(
        src = paste0("images/", req(compartmentalModel()), ".svg"),
        contentType = "image/svg",
        width = "45%",
        alt = gsub("\n[\t\ ]+?", " ",
                   r"(The diagram of the model compartments failed to load, or
                      the accessibility text is being read by a screen
                      reader.)"))
    ))

    output$downloadData <-
      downloadHandler(
        function() paste0(input$modelSelect, "_Model_Summary", Sys.Date(), ".xlsx"),
        function(file) write_xlsx(modelResults, file)
      )

    mainPanel(id = "outputPanel",
              tabsetPanel(
                id = "tabSet",
                tabPanel(
                  "Plot",
                  br(),
                  mainPlot,
                  br(),
                  div(fluidRow(subPlots))
                ),
                if (!exposedCompartmentInModel()) {
                  tabPanel("Phase Plane", br(), phaseplanePlot)
                },
                if (input$stochastic != 1) {
                  tabPanel(
                    "Output Summary",
                    br(),
                    div(style = "display: flex; flex-direction: column;",
                        modelDataTable,
                        downloadButton(
                          "downloadData",
                          "Download as Excel",
                          style = "align-self: flex-start; margin-top: 1vh;"
                        ))
                  )
                },
                tabPanel("Mathematical Model", br(), modelLatex)))
  })

  ## When the user presses the reset button the numeric inputs are reset to the
  ## default values available for the model compartments' parameters and
  ## variables, and the selected model options.
  ##
  ## DONT try to combine these; the UX-logic is as it should be with these two
  ## observers.
  observe({
    isolate(updateNumericInputs(defaults(), session))
  }) |> bindEvent(input$resetNumericInputs)
  ## DONT try to combine these; the UX-logic is as it should be with these two
  ## observers.
  observe({
    if (input$freeze == FALSE) isolate(updateNumericInputs(defaults(), session))
  }) |> bindEvent(input$freeze)

  ## DONT change the return value in the affirmative case (NULL) is ill-advised.
  ## You should know what you're doing before mucking about with this.
  inputsValid <- reactive({
    if (globalValidator$is_valid()) {
      NULL
    } else {
      paste("All the required inputs for this model must be valid.",
            "Check the warnings and errors on the input widgets.")
    }
  }) %>% bindEvent(input) # Depend on all inputs.

  compartmentsEqualPopulation <- reactive({
    shiny::validate(need(compartmentalModel(), "A model must be selected."),
                    need(input$population, "A population is required."),
                    need(input$susceptible, "A number of people must be susceptible."),
                    need(input$infected, "A number of people must be infected."),
                    need(input$recovered, "A number of people must be recovered."))

    applicableVariables <- c(input$susceptible, input$infected, input$recovered)

    if (exposedCompartmentInModel()) {
      shiny::validate(need(input$exposed, "A number of people must be exposed."))
      applicableVariables %<>% append(input$exposed)
    }

    boolean <- !(input$population == sum(applicableVariables))

    stopifnot(length(boolean) == 1)
    message <- "Population must be equal to the sum of the initial compartments values!"
    feedbackDanger("population", boolean, message)
    if(boolean) NULL else message
  })

  ## TODO: if a SI or SEI model without an R compartment is ever introduced then
  ## this needs to be modified.
  recoveryRatePositive <- reactive({
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
