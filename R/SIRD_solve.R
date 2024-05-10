equationsSIRD <- function(time, variables, parameters) {
  S <- variables[1]
  I <- variables[2]
  R <- variables[3]
  D <- variables[4]
  N <- variables[5]
  q <- variables[6]
  dS <- (parameters["muB"] * N) - (parameters["muD"] * S) - (parameters["beta"] * ((S * I) / (N^q)))
  dI <- (parameters["beta"] * ((S * I) / (N^q))) - (parameters["gamma"] * I) - (parameters["delta"] * I) - (parameters["muD"] * I)
  dR <- (parameters["gamma"] * I) - (parameters["muD"] * R)
  dD <- (parameters["delta"] * I)
  dN <- dS + dI + dR
  list(c(dS, dI, dR, dD, dN, q))
}

solveSIRD <- function(
    beta = 0.001,
    gamma = 0.1,
    delta = 0.05,
    muB = 0,
    muD = 0,
    population = 500,
    susceptible = 499,
    infected = 1,
    recovered = 0,
    dead = 0,
    timesteps = 50,
    q = 0) {
  deSolve::lsoda(
    c(
      S = susceptible,
      I = infected,
      R = recovered,
      D = dead,
      N = population,
      q = q
    ),
    seq(0, timesteps, by = 1),
    equationsSIRD,
    c(
      beta = beta,
      gamma = gamma,
      delta = delta,
      muB = muB,
      muD = muD
    )
  ) |>
    as.data.frame()
}

plotSIRD <- function() {
  ggplot2::ggplot(solveSIRD(), ggplot2::aes(x = time)) +
    ggplot2::theme(
      axis.line = ggplot2::element_line(color = "black"),
      axis.text = ggplot2::element_text(size = 14),
      axis.title.x = ggplot2::element_text(size = 16, face = "bold"),
      axis.title.y = ggplot2::element_text(size = 16, face = "bold")
    ) +
    ggplot2::ggtitle("SIRD Epidemic Model") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 22, face = "bold")) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::ylab("Number of People") +
    ggplot2::xlab("Time") +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::geom_line(aes(y = S, color = "Blue"), linewidth = 1.5) +
    ggplot2::geom_line(aes(y = I, color = "Red"), linewidth = 1.5) +
    ggplot2::geom_line(aes(y = R, color = "Green"), linewidth = 1.5) +
    ggplot2::geom_line(aes(y = D, color = "Orange"), linewidth = 1.5) +
    ggplot2::scale_color_identity(
      name = "SIRD", breaks = c("Blue", "Red", "Green", "Orange"),
      labels = c("Susceptible", "Infected", "Recovered", "Dead"), guide = "legend"
    )
}

plotPhasePlaneSIRD <- function() {
  ggplot2::ggplot(solveSIRD(), ggplot2::aes(x = S)) +
    ggplot2::geom_line(aes(y = I, color = "Blue"), linewidth = 1.5) +
    ggplot2::theme(
      axis.line = ggplot2::element_line(color = "black"),
      axis.text = ggplot2::element_text(size = 14),
      axis.title.x = ggplot2::element_text(size = 16, face = "bold"),
      axis.title.y = ggplot2::element_text(size = 16, face = "bold")
    ) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::ggtitle("SI Phase Plane") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 22, face = "bold")) +
    ggplot2::ylab("Infected (I)") +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::xlab("Susceptible (S)") +
    ggplot2::scale_color_identity(
      name = "SIRD", breaks = c("Blue", "Red", "Green", "Orange"),
      labels = c("Susceptible", "Infected", "Recovered", "Dead"), guide = "legend"
    )
}

solveAndRenderSIRD <- function() {
  expr <- quote({
    output$modelPlot <- renderPlot(plotSIRD())
    output$modelPhasePlane <- renderPlot(plotPhasePlaneSIRD())
    output$modelSummaryTable <- renderTable((solveSIRD())[-c(6)])
  })

  eval.parent(expr)
}