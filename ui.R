tabPanel <- function(...) {
  shiny::tabPanel(..., class = "p-3 border border-top-0 rounded-bottom")
}

wellPanel <- function(...) {
  div(class = "well", style = "margin-bottom: 0.75rem;", ...)
}

developer <- function(lastName, firstName, degree = "", affiliationIndex = 1, href = "") {
  tag("address",
      list(a(style = "font-weight: bold;",
             href = href,
             target = "_blank",
             lastName,
             ",",
             firstName,
             degree,
             tag("sup", affiliationIndex))))
}

### Model options
timesteps <- numericInput("timesteps", r"[Number of Timesteps (\(m\))]",
  100, 1,
  step = 1,
  width = "300px"
)

resetButton <- actionButton("resetAll", "Reset Values")

generatePlotSettingsUI <- function(id) {
  div(
    style = "position: relative;", 
    div(
      style = "position: absolute; top: 5px; left: 10px; z-index: 1000;",
      dropdown(
        tags$h3("Plot Options"),
        textInput(paste0(id, "_title"), "Title:"),
        textInput(paste0(id, "_xAxisLabel"), "X-axis label:"),
        textInput(paste0(id, "_yAxisLabel"), "Y-axis label:"),
        uiOutput(paste0(id, "_colorPickers")),  
        icon = icon("cog"),
        status = "primary",
        circle = FALSE,
        inline = TRUE,
        size = "xs",
        style = "simple",
        animate = animateOptions(
          enter = animations$fading_entrances$fadeInDown,
          exit = animations$fading_exits$fadeOutUp))))
}



modelResultsPanel <- mainPanel(
  tabsetPanel(
    tabPanel("Plot", br(), 
             conditionalPanel(
               condition = "input.modelSelect != ''",
               generatePlotSettingsUI("plotSettings")
             ), 
             uiOutput("plot"),
             br(),
             conditionalPanel(
               condition = "output.plot != null",
               uiOutput("subPlots")
             )
    ),
    tabPanel("Phase Plane", br(),
             conditionalPanel(
               condition = "input.modelSelect != ''",
               generatePlotSettingsUI("phasePlanePlotSettings")
             ), 
             uiOutput("phasePlanePlot")),
    tabPanel("Output Summary", br(), uiOutput("outputSummary")),
    tabPanel("Mathematical Model", br(), uiOutput("mathematicalModel")),
  )
)


## NOTE: https://englishlessonsbrighton.co.uk/names-letters-english-alphabet/
### Parameters
beta <- uiOutput("beta")
gamma <- uiOutput("gamma")

delta <- conditionalPanel(
  r"{['SIRD', 'SEIRD'].includes(input.modelSelect)}",
  numericInput("delta", r"[Fatality rate (\(\delta\))]", 0.5,
    min = 0, max = 1,
    step = 0.01,
    width = "300px"
  )
)

sigma <- conditionalPanel(
  r"{['SEIR', 'SEIRS', 'SEIRD'].includes(input.modelSelect)}",
  numericInput("sigma", r"[Rate of recovery (\(\sigma\))]", 0.5,
    min = 0, max = 1,
    step = 0.01,
    width = "300px"
  )
)

xi <- conditionalPanel(
  r"{['SIRS', 'SEIRS'].includes(input.modelSelect)}",
  numericInput("xi", r"[Rate of loss of immunity (\(\xi\))]", 0.5,
    min = 0, max = 1,
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

bold <- function(...) p(..., style = "font-weight: bold")

## TODO: include FontAwesome GitHub logo
GitHub <- function(username, ...) {
  a(..., href = paste0("https://github.com/", username))
}

episimModelAuthorshipTab <-
  nav_panel(
    title = "Authors",
    h2("Developers", style = "font-weight:bold"),
    developer("Carson", "Bryce", "B.Sc", href = "https://github.com/bryce-carson/"),
    developer("Le", "Khanh", href = "https://github.com/kle6951/"),
    developer("Wondwossen", "Tobias", href = "https://github.com/Toby-exe"),
    br(),

    h3("Affiliations"),
    p(tag("sup", 1), "Mount Royal University", br(),
      "4825 Mount Royal Gate SW", br(),
      "Calgary, Alberta, Canada", br(),
      "T3E 6K6"),

    h2("Supervisor"),
    ## TODO: Reformat Ashok's information so the digital links aren't ugly.
    tag("address",
        list(
          p(a("Ashok Krishnamurthy, Ph.D.", href = "https://bit.ly/2YKrXjX", target = "_blank", style = "font-weight: bold;"), br(),
            "Mount Royal University", br(),
            "Department of Mathematics & Computing,", br(),
            "Calgary, AB, Canada", br(),
            a("akrishnamurthy@mtroyal.ca", href = "mailto:akrishnamurthy@mtroyal.ca"), br(),
            a("Episim GitHub", href = "https://github.com/ashokkrish/episim", target = "_blank")),
          style = r"(a[href^='mailto']::before {content: '📧 ';} a[href^='tel']::before {content: '📞 ';})")))

models <- list("SIR", "SIRS", "SIRD", "SEIR", "SEIRS", "SEIRD")
names(models) <- models
models <- append(models, list("Please choose a model" = ""), after = 0)
modelSelect <- pickerInput("modelSelect",
  strong("Epidemic Model"),
  models,
  width = "98%")

massAction <- radioButtons("trueMassAction",
  strong("Model Formulation"),
  choices = list("Pseudo-Mass Action" = 0, "True-Mass Action" = 1),
  inline = TRUE)

deterministic <- radioButtons("stochastic",
  strong("Model Stochasticity"),
  choices = list("Deterministic" = 0, "Stochastic" = 1),
  inline = TRUE)

probability <- radioButtons("distribution",
  strong("Probability Distribution"),
  choices = list("Uniform" = 0, "Binomial" = 1),
  inline = TRUE)

## FIXME: use this CSS path to fix the margin-bottom of the vital dynamics checkbox within this well panel: div.well:nth-child(4) > div:nth-child(1); set the margin-bottom property to zero pixels.
vitalDynamics <-
  wellPanel(checkboxInput("vitalDynamics", "Vital Dynamics", FALSE, "300px"),
            conditionalPanel(
              r"[input.vitalDynamics == '1']",
              numericInput("muBirth",
                           r"[Rate of births (\(\mu_B\))]",
                           0.012, 0, 1.2, 0.0001),
              numericInput("muDeath",
                           r"[Rate of naturally caused death (\(\mu_D\))]",
                           0, 0, 0.00876, 0.0001)))

modelOptions <- wellPanel(id = "modelOptions",
                          h3("Options"),
                          div(massAction, style = "margin: 10px;"),
                          wellPanel(deterministic,
                                    conditionalPanel(r"[input.stochastic == '0']",
                                                     vitalDynamics),
                                    conditionalPanel(
                                      r"[input.stochastic == '1']",
                                      probability,
                                      numericInput("replicates",
                                                   "Number of Replicates (simulations)",
                                                   50, 0, 100, 1,
                                                   "300px"),
                                      actionButton("rerunStochasticSimulation",
                                                   "Rerun stochastic simulation"))),
                          timesteps)

modelParameters <-
  wellPanel(id = "parameters",
            h3("Parameters"),
            div(id = "commonParameters", beta, gamma),
            div(id = "additionalParameters", delta, sigma, xi))

modelVariables <-
  wellPanel(id = "variables",
            h3("Variables"),
            div(id = "commonVariables", N, S, E, I, R, D))

### Design
modelConfigurationPanel <-
  sidebarPanel(id = "inputPanel", 
    modelSelect,
    conditionalPanel(r"--(input.modelSelect !== '')--",
      div(id = "modelConfiguration",
          modelOptions,
          modelParameters,
          modelVariables),
      resetButton))

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
    sidebarLayout(withMathJax(modelConfigurationPanel), modelResultsPanel))

nonspatial <- navset_card_pill(
  title = "Non-spatial Compartmental Models of Epidemiology",
  placement = "above",
  episimModelTab,
  episimModelAuthorshipTab,
  nav_spacer(),
  nav_menu(title = "Links",
           nav_item(a("Github", href = "https://github.com/ashokkrish/episim", target = "_blank")),
           nav_item(a("Mount Royal University", href = "https://mtroyal.ca", target = "_blank"))))

## TODO: make this selectable
nonspatialModelValueBox <-
  value_box(
    title = "Compartmental",
    value = "Non-spatial",
    showcase = bs_icon("box-arrow-down-right"),
    p("Non-spatial compartmental models... are what we have in ye olde episim currently.")
  )

## TODO: integrate spatial modeling into this application
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
  useShinyFeedback(),
  ## autoWaiter(),
  tags$head(
    tags$style(HTML("
      /* Customize the color of all buttons */
      .btn, .btn-primary, .btn-success, .btn-warning, .btn-danger, .btn-info, .btn-toggle, .action-button {
        background-color: #18536F; /* Your desired color */
        border-color: #18536F; /* Your desired color */
        color: #fff !important; /* White text */
      }

      /* Change the hover color of all buttons */
      .btn:hover, .btn-primary:hover, .btn-success:hover, .btn-warning:hover, .btn-danger:hover, .btn-info:hover, .btn-toggle:hover, .action-button:hover {
        background-color: #0d3246; /* Darker shade for hover */
        border-color: #0d3246; /* Darker shade for hover */
      }
    "))),
  tags$script(
         type = "text/javascript",
         id = "modelSelectCustomizationsScript",
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
  theme = bs_theme(version = "5", primary = "#18536F"),
  window_title = "Krishnamurthy Episim",
  title = div(
    style = "background-color: #18536F; padding: 10px; width: 100%; box-sizing: border-box; display: flex; align-items: center;
    height: 100px;",
    div(
      h1(
        strong("The Krishnamurthy Lab Epidemic Modelling app"),
        style = "margin: 0px 0px; color: white; font-size: 35px"
      )
    )),
  sidebar = mainSidebar,
  nonspatial
)
