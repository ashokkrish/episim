#----------------------------------------------------------#
# Deterministic SIRS epidemic model in a closed population #
#----------------------------------------------------------#

equationsSIRS <- function(time, variables, parameters)
{
  S <- variables[1]
  I <- variables[2]
  R <- variables[3]
  N <- variables[4]
  
  dS <- (parameters["muB"] * N) - (parameters["beta"] * ((S * I) / (N))) + (parameters["xi"] * R) - (parameters["muD"] * S)
  dI <- (parameters["beta"] * ((S * I) / (N))) - (parameters["gamma"] * I) - (parameters["muD"] * I)
  dR <- (parameters["gamma"] * I) - (parameters["xi"] * R) - (parameters["muD"] * R)
  
  dN <- dS + dI + dR
  
  list(c(dS, dI, dR, dN))
}

solveSIRS() <- function(beta = 0.001, gamma = 0.1, xi = 0.1, muB = 0, muD = 0, population = 500,
                        susceptible = 499, infected = 1, recovered = 0,timesteps = 50)
{
  variables <- c(susceptible, infected, recovered, population,q)
  names(variables) <- c("S", "I", "R", "N")
  parameters <- c(beta, gamma, xi, muB, muD)
  names(parameters) <- c("beta", "gamma", "xi", "muB", "muD")
  
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
plotSIRS <- function()
{
  ggplot2::ggplot(solveSISR(), ggplot2::aes(x = time)) +
    plotTheme +
    ggplot2::labs(title = "SIRS Epidemic Model", y = "Number of People",
                  x = "Time") +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::geom_line(aes(y = S, color = "Blue"), linewidth = 1.5) +
    ggplot2::geom_line(aes(y = I, color = "Red"), linewidth = 1.5) +
    ggplot2::geom_line(aes(y = R, color = "Green"), linewidth = 1.5) +
    ggplot2::scale_color_identity(
      name = "SIRS", breaks = c("Blue", "Red", "Green"),
      labels = c("Susceptible", "Infected", "Recovered"), guide = "legend"
    )
}
# plot phase plane
plotPhasePlaneSIRS <- function()
{
  ggplot2::ggplot(solveSIRS(), ggplot2::aes(x = S))+
    ggplot2::geom_line(aes(y = I, color = "Blue"), linewidth = 1.5) +
    plotTheme +
    ggplot2::ggtitle("SI Phase Plane") + 
    ggplot2::theme(plot.title = element_text(size = 22, face = "bold")) +
    ggplot2::ylab("Infected (I)") +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::xlab("Susceptible (S)") +
    ggplot2::scale_color_identity(
      name = "SIRS", breaks = c("Blue", "Red", "Green"),
      labels = c("Susceptible", "Infected", "Recovered"), guide = "legend"
    )
}

solveAndRenderSIRS <- function()
{
  expr <- quote({
    output$modelPlot <- renderPlot(plotSIRS())
    output$modelPhasePlane <- renderPlot(plotPhasePlaneSIRS())
    output$modelSummaryTable <- renderTable((solveSIRS())[-c(6)])
  })
  
  eval.parent(expr)
}