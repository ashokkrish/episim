maxPopulation <- 900000000

fluidPage(
  useShinyjs(),
  div(
    titlePanel("Compartmental Models of Epidemiology")
  ),
  navbarPage(
    title = "",
    tabPanel(
      title = "Model",
      sidebarLayout(
        sidebarPanel(
          (""),
          pickerInput(
            inputId = "modelSelect",
            label = ("Epidemic Model"),
            choices = list("Please choose a model", "SIR", "SIRD", "SEIR", "SEIRD", "SIR-Stochastic"),
            options = list("", "SIR", "SIRD", "SEIR", "SEIRD", "SIR-Stochastic"),
            inline = TRUE,
            width = "300px"
          ),
          div(
            radioButtons(
              inputId = "qValue",
              label = ("Model Formulation"),
              choiceValues = list(1, 0),
              choiceNames = list("True-Mass Action", "Pseudo-Mass Action"),
              inline = TRUE,
              width = "300px",
              selected = "0"
            ),
            radioButtons(
              inputId = "stochasticSelect",
              label = strong("Model Stochasticity"),
              choiceValues = list("Deterministic", "Stochastic"),
              choiceNames = list("Deterministic", "Stochastic"),
              selected = "Deterministic", # character(0), #
              inline = TRUE,
              width = "300px"
            ),
            checkboxInput(
              "muValue",
              label = "Vital Dynamics",
              value = FALSE,
              width = "300px"
            ),
            withMathJax(),
            conditionalPanel(
              condition = "input.muValue == '1'",
              numericInput(
                # TODO: Adjust min/ Max
                inputId = "muBirth",
                label = "Birth Rate (\\( \\mu_B\\))",
                min = 0,
                max = 0.1,
                step = 0.0001,
                value = 0.00,
                width = "300px"
              ),
              numericInput(
                # TODO: Adjust min/ Max
                inputId = "muDeath",
                label = "Death Rate due to Natural Causes (\\( \\mu_D\\))",
                min = 0,
                max = 0.1,
                step = 0.0001,
                value = 0.00,
                width = "300px"
              )
            ),
            conditionalPanel(
              condition = "input.modelSelect == 'SIR-Stochastic'",
              withMathJax(),
              numericInput(
                inputId = "stochasticSIR",
                label = "Number of Simulations",
                min = 1,
                max = 100,
                step = 1,
                value = 50,
                width = "300px"
              ),
              numericInput(
                inputId = "betaSIR_Stoc",
                label = "Transmission Rate (\\( \\beta\\))",
                min = 0,
                max = 1,
                step = 0.00001,
                value = 0.00178,
                width = "300px"
              ),
              numericInput(
                inputId = "gammaSIR_Stoc",
                label = "Removal Rate  (\\( \\gamma\\))",
                min = 0,
                max = 5,
                step = 0.00001,
                value = 2.73,
                width = "300px"
              ),
              numericInput(
                inputId = "populationSIR_Stoc",
                label = "Total Population (N)",
                value = 1000,
                min = 1,
                max = maxPopulation,
                step = 1,
                width = "300px"
              ),
              numericInput(
                inputId = "susceptibleSIR_Stoc",
                label = "Susceptible (S)",
                value = 990,
                min = 1,
                max = maxPopulation,
                step = 1,
                width = "300px"
              ),
              numericInput(
                inputId = "infectedSIR_Stoc",
                label = "Infected (I)",
                value = 10,
                min = 1,
                max = maxPopulation,
                step = 1,
                width = "300px"
              ),
              numericInput(
                inputId = "recoveredSIR_Stoc",
                label = "Recovered (R)",
                value = 0,
                min = 0,
                max = maxPopulation,
                step = 1,
                width = "300px"
              )
            ),
            conditionalPanel(
              condition = "input.modelSelect == 'SIR'",
              withMathJax(),
              numericInput(
                inputId = "betaSIR",
                label = "Transmission Rate (\\( \\beta\\))",
                min = 0,
                max = 1,
                step = 0.00001,
                value = 0.001,
                width = "300px"
              ),
              numericInput(
                inputId = "gammaSIR",
                label = "Removal Rate  (\\( \\gamma\\))",
                min = 0,
                max = 5,
                step = 0.00001,
                value = 0.1,
                width = "300px"
              ),
              numericInput(
                inputId = "populationSIR",
                label = "Total Population (N)",
                value = 500,
                min = 1,
                max = maxPopulation,
                step = 1,
                width = "300px"
              ),
              numericInput(
                inputId = "susceptibleSIR",
                label = "Susceptible (S)",
                value = 499,
                min = 1,
                max = maxPopulation,
                step = 1,
                width = "300px"
              ),
              numericInput(
                inputId = "infectedSIR",
                label = "Infected (I)",
                value = 1,
                min = 1,
                max = maxPopulation,
                step = 1,
                width = "300px"
              ),
              numericInput(
                inputId = "recoveredSIR",
                label = "Recovered (R)",
                value = 0,
                min = 0,
                max = maxPopulation,
                step = 1,
                width = "300px"
              ),
            ),
            conditionalPanel(
              condition = "input.modelSelect == 'SIRD'",
              withMathJax(),
              numericInput(
                inputId = "betaSIRD",
                label = "Transmission Rate (\\( \\beta\\))",
                min = 0,
                max = 0.5,
                step = 0.00001,
                value = 0.1,
                width = "300px"
              ),
              numericInput(
                inputId = "gammaSIRD",
                label = "Removal Rate  (\\( \\gamma\\))",
                min = 0,
                max = 0.5,
                step = 0.00001,
                value = 0.1,
                width = "300px"
              ),
              numericInput(
                inputId = "deltaSIRD",
                label = "Death Rate (\\( \\delta\\))",
                min = 0,
                max = 0.5,
                step = 0.00001,
                value = 0.05,
                width = "300px"
              ),
              numericInput(
                inputId = "populationSIRD",
                label = "Total Population (N)",
                value = 500,
                min = 1,
                max = maxPopulation,
                step = 1,
                width = "300px"
              ),
              numericInput(
                inputId = "susceptibleSIRD",
                label = "Susceptible (S)",
                value = 499,
                min = 1,
                max = maxPopulation,
                step = 1,
                width = "300px"
              ),
              numericInput(
                inputId = "infectedSIRD",
                label = "Infected (I)",
                value = 1,
                min = 1,
                max = maxPopulation,
                step = 1,
                width = "300px"
              ),
              numericInput(
                inputId = "recoveredSIRD",
                label = "Recovered (R)",
                value = 0,
                min = 0,
                max = maxPopulation,
                step = 1,
                width = "300px"
              ),
              numericInput(
                inputId = "deadSIRD",
                label = "Dead (D)",
                value = 0,
                min = 0,
                max = maxPopulation,
                step = 1,
                width = "300px"
              ),
            ),
            conditionalPanel(
              condition = "input.modelSelect == 'SEIR'",
              withMathJax(),
              numericInput(
                # TODO: Adjust min/ Max
                inputId = "beta",
                label = "Exposure Rate (\\( \\beta\\))",
                min = 0,
                max = 1,
                step = 0.01,
                value = 0.5,
                width = "300px"
              ),
              numericInput(
                # TODO: Adjust min/ Max
                inputId = "gamma",
                label = "Infectiousness (\\( \\gamma\\))",
                min = 0,
                max = 3,
                step = 0.00001,
                value = 0.5,
                width = "300px"
              ),
              numericInput(
                # TODO: Adjust min/ Max
                inputId = "sigma",
                label = "Removal Rate  (\\( \\sigma\\))",
                min = 0,
                max = 0.5,
                step = 0.00001,
                value = 0.1,
                width = "300px"
              ),
              numericInput(
                inputId = "population",
                label = "Total Population (N)",
                value = 53,
                min = 1,
                max = maxPopulation,
                step = 1,
                width = "300px"
              ),
              numericInput(
                inputId = "susceptible",
                label = "Susceptible (S)",
                value = 50,
                min = 1,
                max = maxPopulation,
                step = 1,
                width = "300px"
              ),
              numericInput(
                inputId = "exposed",
                label = "Exposed (E)",
                value = 3,
                min = 0,
                max = maxPopulation,
                step = 1,
                width = "300px"
              ),
              numericInput(
                inputId = "infected",
                label = "Infected (I)",
                value = 0,
                min = 1,
                max = maxPopulation,
                step = 1,
                width = "300px"
              ),
              numericInput(
                inputId = "recovered",
                label = "Recovered (R)",
                value = 0,
                min = 1,
                max = maxPopulation,
                step = 1,
                width = "300px"
              ),
            ),
            conditionalPanel(
              condition = "input.modelSelect == 'SEIRD'",
              withMathJax(),
              numericInput(
                # TODO: Adjust min/ Max
                inputId = "betaSEIRD",
                label = "Exposure Rate (\\( \\beta\\))",
                min = 0,
                max = 1,
                step = 0.01,
                value = 0.5,
                width = "300px"
              ),
              numericInput(
                # TODO: Adjust min/ Max
                inputId = "gammaSEIRD",
                label = "Infectiousness (\\( \\gamma\\))",
                min = 0,
                max = 3,
                step = 0.00001,
                value = 0.5,
                width = "300px"
              ),
              numericInput(
                # TODO: Adjust min/ Max
                inputId = "sigmaSEIRD",
                label = "Removal Rate  (\\( \\sigma\\))",
                min = 0,
                max = 0.5,
                step = 0.00001,
                value = 0.1,
                width = "300px"
              ),
              numericInput(
                inputId = "deltaSEIRD",
                label = "Death Rate (\\( \\delta\\))",
                min = 0,
                max = 0.5,
                step = 0.00001,
                value = 0.05,
                width = "300px"
              ),
              numericInput(
                inputId = "populationSEIRD",
                label = "Total Population (N)",
                value = 53,
                min = 1,
                max = maxPopulation,
                step = 1,
                width = "300px"
              ),
              numericInput(
                inputId = "susceptibleSEIRD",
                label = "Susceptible (S)",
                value = 50,
                min = 1,
                max = maxPopulation,
                step = 1,
                width = "300px"
              ),
              numericInput(
                inputId = "exposedSEIRD",
                label = "Exposed (E)",
                value = 3,
                min = 0,
                max = maxPopulation,
                step = 1,
                width = "300px"
              ),
              numericInput(
                inputId = "infectedSEIRD",
                label = "Infected (I)",
                value = 0,
                min = 1,
                max = maxPopulation,
                step = 1,
                width = "300px"
              ),
              numericInput(
                inputId = "recoveredSEIRD",
                label = "Recovered (R)",
                value = 0,
                min = 1,
                max = maxPopulation,
                step = 1,
                width = "300px"
              ),
              numericInput(
                inputId = "deadSEIRD",
                label = "Dead (D)",
                value = 0,
                min = 0,
                max = maxPopulation,
                step = 1,
                width = "300px"
              ),
            ),
            numericInput(
              inputId = "timesteps",
              label = "Number of Timesteps (m)",
              value = 100,
              min = 1,
              step = 1,
              width = "300px"
            ),
          ),
          # outside of div
          actionButton("go", "Run Simulation",
            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
          ),
          actionButton("resetAll", "Reset Values",
            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
          ),
        ),

        ##########################
        # End of Side Panel UI...#
        ##########################

        mainPanel(
          tabsetPanel(
            id = "tabSet",
            tabPanel(
              title = "Plot",
              # downloadButton(outputId = "downloadPlot", label = "Save PNG/JPEG"),
              conditionalPanel(
                condition = "input.modelSelect == 'SIR'",
                plotOutput("plotSIR"),
                img(src = "SIR.jpg", height = "100px")
              ),
              conditionalPanel(
                condition = "input.modelSelect == 'SIR-Stochastic'",
                plotOutput("plotSIR_Stoc")
              ),
              conditionalPanel(
                condition = "input.modelSelect == 'SIRD'",
                plotOutput("plotSIRD"),
                img(src = "SIRD.jpg", height = "200px")
              ),
              conditionalPanel(
                condition = "input.modelSelect == 'SEIR'",
                plotOutput("plotSEIR"),
                img(src = "SEIR.jpg", height = "100px")
              ),
              conditionalPanel(
                condition = "input.modelSelect == 'SEIRD'",
                plotOutput("plotSEIRD"),
                img(src = "SEIRD.jpg", height = "200px")
              )
            ),
            tabPanel(
              title = "Phase Plane",
              # downloadButton(outputId = "downloadPlane", label = "Save PNG/JPEG"),
              conditionalPanel(
                condition = "input.modelSelect == 'SIR'",
                plotOutput("SIRPhasePlane")
              ),
              conditionalPanel(
                condition = "input.modelSelect == 'SIRD'",
                plotOutput("SIRDPhasePlane")
              ),
              conditionalPanel(
                condition = "input.modelSelect == 'SEIR'",
                plotOutput("SEIRPhasePlane")
              ),
              conditionalPanel(
                condition = "input.modelSelect == 'SEIRD'",
                plotOutput("SEIRDPhasePlane")
              )
            ),
            tabPanel(
              title = "Output Summary",
              conditionalPanel(
                condition = "input.modelSelect == 'SIR'",
                tableOutput("tableSIR")
              ),
              conditionalPanel(
                condition = "input.modelSelect == 'SIRD'",
                tableOutput("tableSIRD")
              ),
              conditionalPanel(
                condition = "input.modelSelect == 'SEIR'",
                tableOutput("tableSEIR")
              ),
              conditionalPanel(
                condition = "input.modelSelect == 'SEIRD'",
                tableOutput("tableSEIRD")
              )
            ),
            tabPanel(
              title = "Mathematical Model",
              conditionalPanel(
                condition = "input.modelSelect == 'SIR'",
                conditionalPanel(
                  condition = "input.muValue == '0'",
                  withMathJax(
                    helpText("Susceptible $$\\frac{dS}{dt} = - \\beta \\frac{ S I}{N^q}$$"),
                    helpText(
                      "Infectious $$\\frac{dI}{dt} = \\frac{\\beta S I}{N^q} - \\gamma I $$"
                    ),
                    helpText("Recovered $$\\frac{dR}{dt} = \\gamma I $$"),
                    helpText("Reproductive ratio $$R_0 =  \\frac{\\beta}{\\gamma} S(0)^q$$"),
                    helpText("q-Value", br(), "$$ 1, frequency-dependent $$", br(), "$$ 0, density-dependent $$")
                  )
                ),
                conditionalPanel(
                  condition = "input.muValue == '1'",
                  withMathJax(
                    helpText(
                      "Susceptible $$\\frac{dS}{dt} =\\mu_B N - \\mu_D S - \\beta \\frac{ S I}{N^q}$$"
                    ),
                    helpText(
                      "Infectious $$\\frac{dI}{dt} = \\frac{\\beta S I}{N^q} - \\gamma I - \\mu_D I$$"
                    ),
                    helpText("Recovered $$\\frac{dR}{dt} = \\gamma I - \\mu_D R $$"),
                    helpText("Reproductive ratio $$R_0 =  \\frac{\\beta}{\\gamma} S(0)^q$$"),
                    helpText("q-Value", br(), "$$ 1, frequency-dependent $$", br(), "$$ 0, density-dependent $$")
                  )
                )
              ),
              conditionalPanel(
                condition = "input.modelSelect == 'SIRD'",
                conditionalPanel(
                  condition = "input.muValue == '0'",
                  withMathJax(
                    helpText("Susceptible $$\\frac{dS}{dt} = - \\beta \\frac{ S I}{N^q}$$"),
                    helpText(
                      "Infectious $$\\frac{dI}{dt} = \\frac{\\beta S I}{N^q} - \\gamma I - \\delta I $$"
                    ),
                    helpText("Recovered $$\\frac{dR}{dt} = \\gamma I $$"),
                    helpText("Dead $$\\frac{dD}{dt} = \\delta I $$"),
                    helpText("Reproductive ratio $$R_0 =  \\frac{\\beta}{\\gamma + \\delta} S(0)^q$$"),
                    helpText("q-Value", br(), "$$ 1, frequency-dependent $$", br(), "$$ 0, density-dependent $$")
                  )
                ),
                conditionalPanel(
                  condition = "input.muValue == '1'",
                  withMathJax(
                    helpText(
                      "Susceptible $$\\frac{dS}{dt} =\\mu_B N - \\mu_D S - \\beta \\frac{ S I}{N^q}$$"
                    ),
                    helpText(
                      "Infectious $$\\frac{dI}{dt} = \\frac{\\beta S I}{N^q} - \\gamma I - \\delta I - \\mu_D I$$"
                    ),
                    helpText("Recovered $$\\frac{dR}{dt} = \\gamma I - \\mu_D R $$"),
                    helpText("Dead $$\\frac{dD}{dt} = \\delta I $$"),
                    helpText("Reproductive ratio $$R_0 =  \\frac{\\beta}{\\gamma + \\delta} S(0)^q$$"),
                    helpText("q-Value", br(), "$$ 1, frequency-dependent $$", br(), "$$ 0, density-dependent $$")
                  )
                )
              ),
              conditionalPanel(
                condition = "input.modelSelect == 'SEIR'",
                conditionalPanel(
                  condition = "input.muValue == '0'",
                  withMathJax(
                    helpText("Susceptible $$\\frac{dS}{dt} = - \\beta \\frac{ S I}{N^q}$$"),
                    helpText("Exposed $$\\frac{dE}{dt} = \\beta \\frac{ S I}{N^q} - \\gamma E $$"),
                    helpText("Infectious $$\\frac{dI}{dt} = \\gamma E - \\sigma I $$"),
                    helpText("Recovered $$\\frac{dR}{dt} = \\sigma I $$"),
                    helpText("Reproductive ratio $$R_0 =  \\frac{\\beta}{\\gamma}$$"),
                    helpText("q-Value", br(), "$$ 1, frequency-dependent $$", br(), "$$ 0, density-dependent $$")
                  )
                ),
                conditionalPanel(
                  condition = "input.muValue == '1'",
                  withMathJax(
                    helpText(
                      "Susceptible $$\\frac{dS}{dt} =\\mu_B N - \\mu_D S - \\beta \\frac{ S I}{N^q}$$"
                    ),
                    helpText("Exposed $$\\frac{dE}{dt} = \\beta \\frac{ S I}{N^q} - \\gamma E - \\mu_D E$$"),
                    helpText("Infectious $$\\frac{dI}{dt} = \\gamma E - \\sigma I - \\mu_D I $$"),
                    helpText("Recovered $$\\frac{dR}{dt} = \\sigma I - \\mu_D R $$"),
                    helpText("Reproductive ratio $$R_0 =  \\frac{\\beta}{\\gamma}$$"),
                    helpText("q-Value", br(), "$$ 1, frequency-dependent $$", br(), "$$ 0, density-dependent $$")
                  )
                )
              ),
              conditionalPanel(
                condition = "input.modelSelect == 'SEIRD'",
                conditionalPanel(
                  condition = "input.muValue == '0'",
                  withMathJax(
                    helpText("Susceptible $$\\frac{dS}{dt} = - \\beta \\frac{ S I}{N^q}$$"),
                    helpText("Exposed $$\\frac{dE}{dt} = \\beta \\frac{ S I}{N^q} - \\gamma E $$"),
                    helpText("Infectious $$\\frac{dI}{dt} = \\gamma E - \\sigma I - \\delta I $$"),
                    helpText("Recovered $$\\frac{dR}{dt} = \\sigma I $$"),
                    helpText("Dead $$ \\frac{dD}{dt} = \\delta I $$"),
                    helpText("Reproductive ratio $$R_0 =  \\frac{\\beta}{\\gamma}$$"),
                    helpText("q-Value", br(), "$$ 1, frequency-dependent $$", br(), "$$ 0, density-dependent $$")
                  )
                ),
                conditionalPanel(
                  condition = "input.muValue == '1'",
                  withMathJax(
                    helpText(
                      "Susceptible $$\\frac{dS}{dt} =\\mu_B N - \\mu_D S - \\beta \\frac{ S I}{N^q}$$"
                    ),
                    helpText("Exposed $$\\frac{dE}{dt} = \\beta \\frac{ S I}{N^q} - \\gamma E - \\mu_D E$$"),
                    helpText("Infectious $$\\frac{dI}{dt} = \\gamma E - \\sigma I -  \\delta I- \\mu_D I $$"),
                    helpText("Recovered $$\\frac{dR}{dt} = \\sigma I - \\mu_D R $$"),
                    helpText("Dead $$ \\frac{dD}{dt} = \\delta I $$"),
                    helpText("Reproductive ratio $$R_0 =  \\frac{\\beta}{\\gamma}$$"),
                    helpText("q-Value", br(), "$$ 1, frequency-dependent $$", br(), "$$ 0, density-dependent $$")
                  )
                )
              ),
            )
          )
        )
      )
    ),
    tabPanel(
      title = "Authors",
      h3("Development Team", style = "font-weight:bold"),
      br(),
      p(span("Tobias Wondwossen, Khanh Le, Bryce Carson", style = "font-weight:bold")),
      p("Lead Developer - Undergraduate Students"),
      p("Mount Royal University,"),
      p("Department of Mathematics & Computing,"),
      p("Calgary, AB, Canada"),
      br(),
      p(span("Ashok Krishnamurthy, PhD", style = "font-weight:bold")),
      p("Project Supervisor,"),
      p("Mount Royal University"),
      p("Department of Mathematics & Computing,"),
      p("Calgary, AB, Canada"),
      br(),
      p("Email:", a("akrishnamurthy@mtroyal.ca", href = "mailto:akrishnamurthy@mtroyal.ca")),
      p("Website:", a(href = "https://bit.ly/2YKrXjX", "https://bit.ly/2YKrXjX", target = "_blank")),
      p("Github: ", a(href = "https://github.com/ashokkrish/episim", "https://github.com/ashokkrish/episim", target = "_blank")),
      br(),
      br(),
      h3("Disclaimer", style = "font-weight:bold"),
      br(),
      p("This tool uses a mathematical model to simulate epidemic model outcomes based on user-defined parameters. The output of the model depends on model assumptions, parameter choices. It is not a medical predictor, and should be used for informational and research purposes only. Please carefully consider the parameters you choose. Interpret and use the simulated results responsibly. Authors are not liable for any direct or indirect consequences of this usage.")
    )
  )
)
