#---------------------------------------------------------#
# Deterministic SIR epidemic model in a closed population #
#---------------------------------------------------------#

equationsSIR <- function(time, variables, parameters)
{
  S <- variables[1]
  I <- variables[2]
  R <- variables[3]
  N <- variables[4]
  q <- variables[5]

  #  dS <- (input$muBirth * N) - (input$muDeath * S) - (input$betaSIR * ((S * I) / (N^q)))
  dS <- (parameters["muB"] * N) - (parameters["muD"] * S) - (parameters["beta"] * ((S * I) / (N^q)))
  #  dI <- (input$betaSIR * ((S * I) / (N^q))) - (input$gammaSIR * I) - (input$muDeath * I)
  dI <- (parameters["beta"] * ((S * I) / (N^q))) - (parameters["gamma"] * I) - (parameters["muD"] * I)
  # dR <- (input$gammaSIR * I) - (input$muDeath * R)
  dR <- (parameters["gamma"] * I) - (parameters["muD"] * R)
  
  dN <- dS + dI + dR
  list(c(dS, dI, dR, dN, q))
}

solveSIR <- function(beta = 0.001, gamma = 0.1, muB = 0, muD = 0, population = 500,
                      susceptible = 499, infected = 1, recovered = 0,timesteps = 50,
                      q = 0)
{
  variables <- c(susceptible, infected, recovered, population,q)
  names(variables) <- c("S", "I", "R", "N","q")
  parameters <- c(beta, gamma, muB, muD)
  names(parameters) <- c("beta", "gamma", "muB", "muD")
  
  deSolve::lsoda(variables, seq(0, timesteps, by = 1),
                 equationsSIR, parameters) |> as.data.frame()
}

plotTheme <- ggplot2::theme(
  axis.line = ggplot2::element_line(color = "black"),
  axis.text = ggplot2::element_text(size = 14),
  axis.title.x = ggplot2::element_text(size = 16, face = "bold"),
  axis.title.y = ggplot2::element_text(size = 16, face = "bold"),
  plot.title = ggplot2::element_text(size = 22, face = "bold"),
  legend.position = "bottom"
)

# Plot
plotSIR <- function()
{
  ggplot2::ggplot(solveSIR(), ggplot2::aes(x = time)) +
    plotTheme +

    ggplot2::labs(title = "SIRS Epidemic Model", y = "Number of People",
                  x = "Time") +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::geom_line(ggplot2::aes(y = S, color = "Blue"), linewidth = 1.5) +
    ggplot2::geom_line(ggplot2::aes(y = I, color = "Red"), linewidth = 1.5) +
    ggplot2::geom_line(ggplot2::aes(y = R, color = "Green"), linewidth = 1.5) +

    ggplot2::scale_color_identity(
      name = "SIR", breaks = c("Blue", "Red", "Green"),
      labels = c("Susceptible", "Infected", "Recovered"), guide = "legend"
    )
}

# plot phase plane
plotPhasePlaneSIR <- function()
{
  ggplot2::ggplot(solveSIR(), ggplot2::aes(x = S))+

    ggplot2::geom_line(ggplot2::aes(y = I, color = "Blue"), linewidth = 1.5) +
    plotTheme +
    ggplot2::ggtitle("SI Phase Plane") + 
    ggplot2::theme(plot.title = ggplot2::element_text(size = 22, face = "bold")) +
    ggplot2::ylab("Infected (I)") +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::xlab("Susceptible (S)") +
    ggplot2::scale_color_identity(
      name = "SIR", breaks = c("Blue", "Red", "Green"),
      labels = c("Susceptible", "Infected", "Recovered"), guide = "legend"
    )
}

solveAndRenderSIR <- function()
{
  expr <- quote({
    output$modelPlot <- renderPlot(plotSIR())
    output$modelPhasePlane <- renderPlot(plotPhasePlaneSIR())
    output$modelSummaryTable <- renderTable((solveSIR())[-c(6)])
  })
  
  eval.parent(expr)
}
