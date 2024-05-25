tabPanel <- function(...) {
  shiny::tabPanel(..., class = "p-3 border border-top-0 rounded-bottom")
}
## NOTE: lintr [object_usage_linter] warnings about a lack of a visible global
## definition for some of the shiny elements here are erroneous and can be
## ignored. There is no reason to evaluate these function definitions in the
## global environment manually, because that is going to happen regardless.

### Model options
timesteps <- numericInput("timesteps", r"[Number of Timesteps (\(m\))]",
  100, 1,
  step = 1,
  width = "300px"
)

actionButtonStyle <- "color: #fff; background-color: #337ab7; border-color: #2e6da4;"
runSimulationOrResetButtons <-
  div(
    id = "actionButtons", style = "display: none;", # Hidden by default
    actionButton("go", "Run Simulation", style = actionButtonStyle),
    actionButton("resetAll", "Reset Values", style = actionButtonStyle)
  )

modelResultsPanel <-
  mainPanel(
    id = "outputPanel", style = "display: none;", # Hidden by default
    tabsetPanel(
      id = "tabSet",
      tabPanel("Plot", plotOutput("modelPlot"),
               conditionalPanel(r"{!([''].includes(input.modelSelect))}")),
      tabPanel("Phase Plane", plotOutput("modelPhasePlane")),
      tabPanel(
        "Output Summary",
        # FIXME: previously, only the button had to be added to the tabpanel
        # but now doing so places the button behind the datatable. I have
        # added some custom CSS to fix this for now.
        div(
          style = "display: flex; flex-direction: column;",
          DT::dataTableOutput("modelSummaryTable"),
          downloadButton(
            "downloadData", "Download as Excel",
            style = "align-self: flex-start; margin-top: 1vh;"
          )
        )
      ),
      tabPanel("Mathematical Model", uiOutput("modelLaTeX"))
    )
  )

## NOTE: https://englishlessonsbrighton.co.uk/names-letters-english-alphabet/
### Parameters
beta <- uiOutput("beta")
gamma <- uiOutput("gamma")

delta <- conditionalPanel(
  r"{['SIRD', 'SEIRD'].includes(input.modelSelect)}",
  numericInput("delta", r"[Fatality rate (\(\delta\))]", 0.5,
    min = 0, max = 1, # TODO: Adjust minimum and maximum
    step = 0.01,
    width = "300px"
  )
)

sigma <- conditionalPanel(
  r"{['SEIR', 'SEIRS', 'SEIRD'].includes(input.modelSelect)}",
  numericInput("sigma", r"[Rate of recovery (\(\sigma\))]", 0.5,
    min = 0, max = 1, # TODO: Adjust minimum and maximum
    step = 0.01,
    width = "300px"
  )
)

xi <- conditionalPanel(
  r"{['SIRS', 'SEIRS'].includes(input.modelSelect)}",
  numericInput("xi", r"[Rate of loss of immunity (\(\xi\))]", 0.5,
    min = 0, max = 1, # TODO: Adjust minimum and maximum
    step = 0.01,
    width = "300px"
  )
)

## 🥌 Rinks dev The emoji is a landmark in this file.
N <- numericInput("population", r"[Population (\(N\))]", 2, min = 2, step = 1, width = "300px")
S <- numericInput("susceptible", r"[Susceptible (\(S\))]", 1, min = 1, step = 1, width = "300px")
I <- numericInput("infected", r"[Infected (\(I\))]", 1, min = 1, step = 1, width = "300px")
R <- numericInput("recovered", r"[Recovered (\(R\))]", 0, min = 0, step = 1, width = "300px")

## Given the modelSelection string is available in the calling environment
## of these functions, display (or don't) the appropriate input.
D <- conditionalPanel(
  r"(['SIRD', 'SEIRD'].includes(input.modelSelect))",
  numericInput("dead", r"[Dead (\(D\))]", 0, min = 0, step = 1, width = "300px")
)

E <- conditionalPanel(
  r"{['SEIR', 'SEIRS', 'SEIRD'].includes(input.modelSelect)}",
  numericInput("exposed", r"[Exposed (\(E\))]", 0, min = 0, step = 1, width = "300px")
)

V <- conditionalPanel(
  ## TODO: write which modoels have vaccination when they're implemented.
  r"{[].includes(input.modelSelect)}",
  numericInput("vaccinated", r"[Vaccinated (\(V\))]", 0, min = 0, step = 1, width = "300px")
)

bold <- function(...) p(..., style = "font-weight: bold")
## TODO: include FontAwesome GitHub logo
GitHub <- function(username, ...) {
  a(..., href = paste0("https://github.com/", username))
}

episimModelAuthorshipTab <-
  nav_panel(
    title = "Authors",
    h3("Development Team", style = "font-weight:bold"), br(),

    ## TODO: Bryce's information
    bold("Bryce Carson, B.Sc."),
    p("Lead Developer"),
    GitHub("bryce-carson"),
    br(),

    # D evelopers' Information
    bold("Khanh Le, Tobias Wondwossen"),
    p("Developer - Undergraduate Student, Mount Royal University, Calgary, AB, CANADA"),
    br(),
    ## GitHub(),

    ## TODO: Reformat Ashok's information so it reads like a physical mailing
    ## address, and the digital links aren't ugly.
    bold("Ashok Krishnamurthy, Ph.D."),
    p("Project Supervisor,"),
    p("Mount Royal University"),
    p("Department of Mathematics & Computing,"),
    p("Calgary, AB, Canada"), br(),
    p("Email:", a("akrishnamurthy@mtroyal.ca",
      href = "mailto:akrishnamurthy@mtroyal.ca"
    )),
    p("Website:", a(
      href = "https://bit.ly/2YKrXjX",
      "https://bit.ly/2YKrXjX",
      target = "_blank"
    )),
    p("Github: ", a(
      href = "https://github.com/ashokkrish/episim",
      "https://github.com/ashokkrish/episim",
      target = "_blank"
    )), br(), br(),
  )

models <- list("SIR", "SIRS", "SIRD", "SEIR", "SEIRS", "SEIRD")
names(models) <- models
models <- append(models, list("Please choose a model" = ""), after = 0)
modelSelect <- pickerInput("modelSelect",
  strong("Epidemic Model"),
  models,
  width = "300px"
)

## FIXME: https://shiny.posit.co/r/articles/build/images/
modelSelectAndDiagram <- div(modelSelect, uiOutput("modelDiagram"))

massAction <- radioButtons("trueMassAction",
  strong("Model Formulation"),
  choices = list("Pseudo-Mass Action" = 0, "True-Mass Action" = 1),
  inline = TRUE
)

deterministic <- radioButtons("stochastic",
  strong("Model Stochasticity"),
  choices = list("Deterministic" = 0, "Stochastic" = 1),
  inline = TRUE
)

## Vaccination
## alpha <- numericInput("alpha", r"(Rate of vaccination ($ \alpha $))", 0.0,
##   min = 0.0, max = 1.0,
##   step = 0.01,
##   width = "300px"
##   ) |>
##   ## TODO: write which models have vaccination when they're implemented.
##   conditionalPanel(r"{[].includes(input.modelSelect)}")
vitalDynamics <-
  div(checkboxInput("vitalDynamics", "Vital Dynamics", FALSE, "300px"),
      conditionalPanel(
        r"[input.vitalDynamics == '1']",
        numericInput("muBirth",
                     r"[Rate of births (\(\mu_B\))]",
                     0, 0, 0.1, 0.0001),
        numericInput("muDeath",
                     r"[Rate of naturally caused death (\(\mu_D\))]",
                     0, 0, 0.1, 0.0001)),
      id = "vitalDynamics-card",
      class = "card bslib-card",
      style = "margin: 10px;")

modelOptions <- div(id = "modelOptions",
                    div(massAction, style = "margin: 10px;"),
                    div(deterministic, style = "margin: 10px;"),
                    vitalDynamics)

replicates <-
  conditionalPanel(
    r"[input.stochastic == '1']",
    numericInput(
      "replicates",
      "Number of Replicates (simulations)",
      50, 0, 100, 1,
      "300px"
    )
  )

modelParameters <-
  div(
    id = "parameters",
    div(id = "commonParameters", beta, gamma),
    div(id = "additionalParameters", delta, sigma, xi),

    ## TODO: include the following objects if these model options are implemented.
    ## vaccinationParameters
    ## immigrationParameters
  )

modelVariables <- div(id = "variables",
  div(id = "commonVariables", N, S, E, I, R, D)
)

### Design
modelConfigurationPanel <-
  sidebarPanel(
    id = "inputPanel",
    modelSelectAndDiagram,
    div(
      id = "modelConfiguration", style = "display: none;",
      modelOptions,
      modelParameters,
      modelVariables,
      timesteps,
      replicates
    ),
    runSimulationOrResetButtons
  )


disclaimer <-
  p(gsub(
    r"[\n]", " ",
    r"(This tool uses a mathematical model to simulate epidemic model
outcomes based on user-defined parameters. The output of the model depends on
model assumptions, parameter choices. It is not a medical predictor, and
should be used for informational and research purposes only. Please carefully
consider the parameters you choose. Interpret and use the simulated results
responsibly. Authors are not liable for any direct or indirect consequences
of this usage.)"
  ))

episimModelTab <-
  nav_panel(
    title = "Model",
    sidebarLayout(withMathJax(modelConfigurationPanel), modelResultsPanel)
  )

nonspatial <- navset_card_pill(
  title = "Non-spatial Compartmental Models of Epidemiology",
  placement = "above",
  episimModelTab,
  episimModelAuthorshipTab,
  nav_spacer(),
  nav_menu(
    title = "Links",
    nav_item(a("Mount Royal University", href = "https://mtroyal.ca")),
  )
)

## TODO: make this selectable
nonspatialModelValueBox <-
  value_box(
    title = "Compartmental",
    value = "Non-spatial",
    showcase = bs_icon("box-arrow-down-right"),
    p("Non-spatial compartmental models... are what we have in ye olde episim currently.")
  )

## TODO: integrate spatial modelling into this application
spatialModelValueBox <-
  value_box(
    title = "Compartmental",
    value = "Spatial",
    showcase = bs_icon("rocket-takeoff-fill"),
    p("Some really cool stuff")
  )

## TODO: integrate agent-based modelling into this application
agentBasedModelValueBox <-
  value_box(
    title = "Agent-based",
    value = "Non-spatial, Agent-based model?",
    showcase = bs_icon("virus2"),
    p("Some really cool stuff")
  )

mainSidebar <- sidebar(
  id = "mainSidebar",
  open = FALSE,
  title = "Epidemic modelling type",
  nonspatialModelValueBox,
  spatialModelValueBox,
  agentBasedModelValueBox
)

## APPLICATION UI ROOT -----------------------------------------------------
page_sidebar(
  tags$script(
         type = "text/javascript",
         id = "modelSelectECMAScript",
         async = NA,
         src = "modelSelect.js"
  ),
  tags$script(
    type = "text/javascript",
    id = "MathJax-script",
    async = NA, # A boolean, an attribute without a value.
    src = "https://cdn.jsdelivr.net/npm/mathjax@3.2.2/es5/tex-mml-chtml.min.js"
  ),
  useShinyjs(),
  theme = bs_theme(version = "5"),
  window_title = "Krishnamurthy Episim",
  title = "The Krishnamurthy Lab Epidemic Modelling app",
  sidebar = mainSidebar,
  nonspatial
)
