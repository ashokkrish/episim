## NOTE: lintr [object_usage_linter] warnings about a lack of a visible global
## definition for some of the shiny elements here are erroneous and can be
## ignored. There is no reason to evaluate these function definitions in the
## global environment manually, because that is going to happen regardless.

maxPopulation <- 900000000

modelOptions <- function() {
  div(id = "modelOptions",
    radioButtons("qValue", "Model Formulation",
      choices = list("Pseudo-Mass Action" = 0, "True-Mass Action" = 1),
      ## FIXME: again, is this having any effect?
      inline = TRUE,
      "300px",
    ),
    radioButtons("stochasticSelect",
      strong("Model Stochasticity"),
      choices = list("Deterministic" = 0, "Stochastic" = 1),
      ## FIXME: again, is this having any effect?
      inline = TRUE,
      "300px"),
    checkboxInput("muValue", "Vital Dynamics", FALSE, "300px")
  )
}

stochasticVariables <-
  conditionalPanel(r"[input.stochasticSelect == '1']",
                   numericInput("stochasticModelVariableNumberOfReplicates",
                                "Number of Replicates (simulations)",
                                50, 0, 100, 1,
                                "300px"))

vitalDynamicsParameters <-
  conditionalPanel(r"[input.muValue == '1']",
    numericInput("muBirth", r"[Birth Rate ($mu_B$)]",
                 0, 0, 0.1, 0.0001,
                 "300px"),
    numericInput("muDeath", r"[Death Rate due to Natural Causes ($\mu_D$)]",
                 0, 0, 0.1, 0.0001,
                 "300px"))

modelConfigurationPanel <- function() {
  sidebarPanel(id = "inputPanel",
    pickerInput("modelSelect", "Epidemic Model",
                list("Please choose a model" = "",
                     SIR = "SIR",
                     SIRS = "SIRS",
                     SIRD = "SIRD",
                     SEIR = "SEIR",
                     SEIRD = "SEIRD"),
                ## FIXME: is this having any effect, given width is not "fit"?
                inline = TRUE,
                width = "300px"),
    div(id = "modelConfiguration", style = "display: none;",
        modelOptions(),
        modelParameters(),
        modelVariables(),
        numericInput("timesteps", "Number of Timesteps (m)",
                     100, 1,
                     step = 1,
                     width = "300px")),
    runSimulationOrResetButtons())
}

runSimulationOrResetButtons <- function() {
  actionButtonStyle <- "color: #fff; background-color: #337ab7; border-color: #2e6da4;"
  div(id = "actionButtons", style = "display: none;",
    actionButton("go", "Run Simulation", style = actionButtonStyle),
    actionButton("resetAll", "Reset Values", style = actionButtonStyle)
  )
}

## TODO: refactor this, making tabPanel functions for the plot, phase-plane,
## table, and model equations; the arguments of the tabsetPanel function call
## will be calls to these functions to construct the contents of each tab.
modelResultsPanel <- function() {
  mainPanel(id = "outputPanel", style = "display: none;",
    tabsetPanel(
      id = "tabSet",
      tabPanel("Plot", plotOutput("modelPlot"), imageOutput("modelDiagram")),
      tabPanel("Phase Plane", plotOutput("modelPhasePlane")),
      tabPanel("Output Summary", tableOutput("modelSummaryTable")),
      tabPanel("Mathematical Model", uiOutput("modelLaTeX"))
    )
  )
}

## NOTE: https://englishlessonsbrighton.co.uk/names-letters-english-alphabet/
### Parameters
beta <- function() {
  numericInput("beta", r"(Exposure Rate ($\beta$))", 0.5,
    min = 0, max = 1, # TODO: Adjust minimum and maximum
    step = 0.01,
    width = "300px"
  )
}

gamma <- function() {
  numericInput("gamma", r"(Exposure Rate ($\gamma$))", 0.5,
    min = 0, max = 3, # TODO: Adjust minimum and maximum
    step = 0.01, # TODO: Adjust the stepping constant.
    width = "300px")
}

delta <- function() {
  numericInput("delta", r"(Exposure Rate ($\delta$))", 0.5,
    min = 0, max = 1, # TODO: Adjust minimum and maximum
    step = 0.01,
    width = "300px")
}

omega <- function() {
  numericInput("omega", r"(Exposure Rate ($\omega$))", 0.5,
    min = 0, max = 1, # TODO: Adjust minimum and maximum
    step = 0.01,
    width = "300px")
}

## Vital Dynamics
bee <- function() {}
mu <- function() {}

## Death due to disease
alpha <- function() {}

## Migraiton and emigration
AY <- function() {}
BEE <- function() {}

modelParameters <- function() {
  div(id = "parameters",
    div(id = "commonParameters", beta(), gamma()),
    div(id = "optionalParameters", vitalDynamicsParameters),
    div(id = "additionalParameters",
      delta(),
      omega(),
      bee(), mu(), # Vital statistics
      alpha(), # Mortality rate
      AY(), BEE() # Immigration and emigration
    )
  )
}

### Variables; TODO: based upon the value of modelSelection string in this
### calling environment, the common, optional, and additional variable functions
### need to display or not display. This should be controlled within the
### function bodies so that this function remains simple.
modelVariables <- function() {
  div(id = "variables",
    div(id = "commonVariables", S(), I(), R(), K()),
    div(id = "optionalVariables", stochasticVariables),
    div(id = "additionalVariables", D(), E(), V())
  )
}

## 🥌 Rinks dev The emoji is a landmark in this file.
R <- function() {}
I <- function() {}
N <- function() {}
K <- N # Try to use K because it's what is used in Ma and Li 2009.
S <- function() {}

## TODO: given the modelSelection string is available in the calling environment
## of these functions, display (or don't) the appropriate input.
D <- function() {}
E <- function() {}
V <- function() {}

### Model options
massAction <- function() {}
deterministic <- function() {}
vitalDynamics <- function() {}

### Design
episimModelTab <- function() {
  tabPanel("Model",
    sidebarLayout(withMathJax(modelConfigurationPanel()),
                  modelResultsPanel()))
}

bold <- function(...) p(..., style = "font-weight: bold")
## TODO: include FontAwesome GitHub logo
GitHub <- function(username, ...) {
  a(..., href = paste0("https://github.com/", username))
}

episimModelAuthorshipTab <- function() {
  tabPanel(
    "Authors",
    h3("Development Team", style = "font-weight:bold"), br(),

    ## TODO: Bryce's information
    bold("Bryce Carson, B.Sc."),
    p("Lead Developer"),
    GitHub("bryce-carson"),
    br(),

    ## TODO: Khanh's information.
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
                  href = "mailto:akrishnamurthy@mtroyal.ca")),
    p("Website:", a(href = "https://bit.ly/2YKrXjX",
                    "https://bit.ly/2YKrXjX",
                    target = "_blank")),
    p("Github: ", a(href = "https://github.com/ashokkrish/episim",
                    "https://github.com/ashokkrish/episim",
                    target = "_blank")), br(), br(),


    h3("Disclaimer", style = "font-weight:bold"), br(),

    ## Substitute the line breaks in the source for null-strins to join them. A
    ## poor man's here-doc.
    p(gsub("\n", " ",
      r"(This tool uses a mathematical model to simulate epidemic model
outcomes based on user-defined parameters. The output of the model depends on
model assumptions, parameter choices. It is not a medical predictor, and
should be used for informational and research purposes only. Please carefully
consider the parameters you choose. Interpret and use the simulated results
responsibly. Authors are not liable for any direct or indirect consequences
of this usage.)"))
  )
}

fluidPage(useShinyjs(),
          div(titlePanel("Compartmental Models of Epidemiology")),
          withMathJax(navbarPage(title = "",
                                 episimModelTab(),
                                 episimModelAuthorshipTab())))
