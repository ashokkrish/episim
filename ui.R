## NOTE: lintr [object_usage_linter] warnings about a lack of a visible global
## definition for some of the shiny elements here are erroneous and can be
## ignored. There is no reason to evaluate these function definitions in the
## global environment manually, because that is going to happen regardless.

maxPopulation <- 900000000

### Model options

timesteps <- numericInput("timesteps", "Number of Timesteps (m)",
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

## TODO: refactor this, making tabPanel functions for the plot, phase-plane,
## table, and model equations; the arguments of the tabsetPanel function call
## will be calls to these functions to construct the contents of each tab.
modelResultsPanel <-
  mainPanel(
    id = "outputPanel", style = "display: none;", # Hidden by default
    tabsetPanel(
      id = "tabSet",
      tabPanel("Plot", plotOutput("modelPlot"), imageOutput("modelDiagram")),
      tabPanel("Phase Plane", plotOutput("modelPhasePlane")),
      tabPanel("Output Summary", tableOutput("modelSummaryTable")),
      tabPanel("Mathematical Model", uiOutput("modelLaTeX"))
    )
  )

## NOTE: https://englishlessonsbrighton.co.uk/names-letters-english-alphabet/
### Parameters
beta <- numericInput("beta", "ERROR", 0.5,
  min = 0, max = 1, # TODO: Adjust minimum and maximum
  step = 0.01,
  width = "300px"
)

gamma <- numericInput("gamma", "ERROR", 0.5,
  min = 0, max = 3, # TODO: Adjust minimum and maximum
  step = 0.01, # TODO: Adjust the stepping constant.
  width = "300px"
)

delta <- conditionalPanel(
  r"{['SIRD', 'SEIRD'].includes(input.modelSelect)}",
  numericInput("delta", r"(Fatality rate ($ \delta $))", 0.5,
    min = 0, max = 1, # TODO: Adjust minimum and maximum
    step = 0.01,
    width = "300px"
  )
)


sigma <-
  conditionalPanel(
    r"{['SIRD', 'SEIRD'].includes(input.modelSelect)}",
    numericInput("sigma", r"(Rate of recovery ($ \sigma $))", 0.5,
      min = 0, max = 1, # TODO: Adjust minimum and maximum
      step = 0.01,
      width = "300px"
    )
  )

xi <- conditionalPanel(
  r"{['SIRS', 'SEIRS'].includes(input.modelSelect)}",
  numericInput("xi", r"(Rate of loss of immunity ($ \xi $))", 0.5,
    min = 0, max = 1, # TODO: Adjust minimum and maximum
    step = 0.01,
    width = "300px"
  )
)

## Vital Dynamics
## bee <- {}
## mu <- {}

## Vaccination
## alpha <- numericInput("alpha", r"(Rate of vaccination ($ \alpha $))", 0.0,
##   min = 0.0, max = 1.0,
##   step = 0.01,
##   width = "300px"
##   ) |>
##   ## TODO: write which models have vaccination when they're implemented.
##   conditionalPanel(r"{[].includes(input.modelSelect)}")

## Migraiton and emigration
## AY <- {}
## BEE <- {}

## 🥌 Rinks dev The emoji is a landmark in this file.
R <- numericInput("recovered", r"(Recovered)", 0, min = 0, step = 1, width = "300px")
I <- numericInput("infected", r"(Infected)", 1, min = 1, step = 1, width = "300px")
N <- numericInput("population", r"(Population)", 2, min = 2, step = 1, width = "300px")
S <- numericInput("susceptible", r"(Susceptible)", 1, min = 1, step = 1, width = "300px")

## Given the modelSelection string is available in the calling environment
## of these functions, display (or don't) the appropriate input.
D <- conditionalPanel(
  r"{['SIRD', 'SEIRD'].includes(input.modelSelect)}",
  numericInput("dead", r"(Dead)", 0, min = 0, step = 1, width = "300px")
)

E <- conditionalPanel(
  r"{['SEIR', 'SEIRS', 'SEIRD'].includes(input.modelSelect)}",
  numericInput("exposed", r"(Exposed)", 0, min = 0, step = 1, width = "300px")
)

V <- conditionalPanel(
  ## TODO: write which modoels have vaccination when they're implemented.
  r"{[].includes(input.modelSelect)}",
  numericInput("vaccinated", r"(Vaccinated)", 0, min = 0, step = 1, width = "300px")
)

bold <- function(...) p(..., style = "font-weight: bold")
## TODO: include FontAwesome GitHub logo
GitHub <- function(username, ...) {
  a(..., href = paste0("https://github.com/", username))
}

episimModelAuthorshipTab <-
  nav_panel(title = "Authors",
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

modelSelect <- pickerInput("modelSelect", "Epidemic Model",
  list(
    "Please choose a model" = "",
    SIR = "SIR",
    SIRS = "SIRS",
    SIRD = "SIRD",
    SEIR = "SEIR",
    SEIRS = "SEIRS",
    SEIRD = "SEIRD"
  ),
  ## FIXME: is this having any effect, given width is not "fit"?
  inline = TRUE,
  width = "300px"
)

massAction <- radioButtons("massActionSelect", "Model Formulation",
  choices = list("Pseudo-Mass Action" = 0, "True-Mass Action" = 1),
  ## FIXME: again, is this having any effect?
  inline = TRUE,
  "300px"
)

deterministic <- radioButtons("stochasticSelect",
  strong("Model Stochasticity"),
  choices = list("Deterministic" = 0, "Stochastic" = 1),
  ## FIXME: again, is this having any effect?
  inline = TRUE,
  "300px"
)

vitalDynamics <- checkboxInput("muValue", "Vital Dynamics", FALSE, "300px")

modelOptions <- div(id = "modelOptions", massAction, deterministic, vitalDynamics)

stochasticVariables <-
  conditionalPanel(
    r"[input.stochasticSelect == '1']",
    numericInput(
      "stochasticModelVariableNumberOfReplicates",
      "Number of Replicates (simulations)",
      50, 0, 100, 1,
      "300px"
    )
  )

vitalDynamicsParameters <-
  conditionalPanel(
    r"[input.muValue == '1']",
    numericInput(
      "muBirth", r"[Birth Rate ($mu_B$)]",
      0, 0, 0.1, 0.0001,
      "300px"
    ),
    numericInput(
      "muDeath", r"[Death Rate due to Natural Causes ($\mu_D$)]",
      0, 0, 0.1, 0.0001,
      "300px"
    )
  )

modelParameters <-
  div(
    id = "parameters",
    div(id = "commonParameters", beta, gamma),
    div(id = "optionalParameters", vitalDynamicsParameters),
    div(
      id = "additionalParameters",
      delta,
      sigma,
      xi
      ## TODO
      ## bee, mu, # Vital statistics
      ## alpha, # Mortality rate
      ## AY, BEE # Immigration and emigration
    )
  )

### Variables; TODO: based upon the value of modelSelection string in this
### calling environment, the common, optional, and additional variable functions
### need to display or not display. This should be controlled within the
### function bodies so that this function remains simple.
modelVariables <- div(id = "variables",
  div(id = "commonVariables", S, I, R, N),
  div(id = "optionalVariables", stochasticVariables),
  div(id = "additionalVariables", D, E, V)
)

### Design
modelConfigurationPanel <-
  sidebarPanel(
    id = "inputPanel",
    modelSelect,
    div(
      id = "modelConfiguration", style = "display: none;",
      modelOptions,
      modelParameters,
      modelVariables,
      timesteps
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
  nav_panel(title = "Model",
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
  tags$script(src = "whenModelSelectChangesTypesetLaTeX.js"),
  useShinyjs(),
  theme = bs_theme(version = "5"),
  window_title = "Krishnamurthy Episim",
  title = "The Krishnamurthy Lab Epidemic Modelling app",
  sidebar = mainSidebar,
  nonspatial
)
