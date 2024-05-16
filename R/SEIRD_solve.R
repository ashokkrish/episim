equationsSEIRD <- function(time, variables, parameters) {
  S <- variables[1]
  E <- variables[2]
  I <- variables[3]
  R <- variables[4]
  D <- variables[5]
  N <- variables[6]
  q <- variables[7]
  dS <- (parameters["muB"] * N) - (parameters["muD"] * S) - (parameters["beta"] * ((S * I) / (N^q)))
  dE <- (parameters["beta"] * ((S * I) / (N^q))) - (parameters["gamma"] * E) - (parameters["muD"] * E)
  dI <- (parameters["gamma"] * E) - (parameters["sigma"] * I) - (parameters["delta"] * I) - (parameters["muD"] * I)
  dR <- (parameters["sigma"] * I) - (parameters["muD"] * R)
  dD <- (parameters["delta"] * I)
  dN <- dS + dE + dI + dR
  list(c(dS, dE, dI, dR, dD, dN, q))
}

solveSEIRD <- function(
    beta = 0.35,
    gamma = 0.1429,
    sigma = 0.0476,
    delta = 0.05,
    muB = 0,
    muD = 0,
    population = 500,
    susceptible = 499,
    exposed = 0,
    infected = 1,
    recovered = 0,
    dead = 0,
    timesteps = 20,
    q = 0) {
  deSolve::lsoda(
    c(
      S = susceptible,
      E = exposed,
      I = infected,
      R = recovered,
      D = dead,
      N = population,
      q = q
    ),
    seq(0, timesteps, by = 1),
    equationsSEIRD,
    c(
      beta = beta,
      gamma = gamma,
      sigma = sigma,
      delta = delta,
      muB = muB,
      muD = muD
    )
  ) |>
    as.data.frame()
}

plotSEIRD <- function() {
  ggplot2::ggplot(solveSEIRD(), ggplot2::aes(x = time)) +
    ggplot2::theme(
      axis.line = ggplot2::element_line(color = "black"),
      axis.text = ggplot2::element_text(size = 14),
      axis.title.x = ggplot2::element_text(size = 16, face = "bold"),
      axis.title.y = ggplot2::element_text(size = 16, face = "bold")
    ) +
    ggplot2::ggtitle("SEIRD Epidemic Model") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 22, face = "bold")) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::ylab("Number of People") +
    ggplot2::xlab("Time") +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::geom_line(ggplot2::aes(x = time, y = S, color = "Blue"), linewidth = 1.5) +
    ggplot2::geom_line(ggplot2::aes(x = time, y = E, color = "Brown"), linewidth = 1.5) +
    ggplot2::geom_line(ggplot2::aes(x = time, y = I, color = "Red"), linewidth = 1.5) +
    ggplot2::geom_line(ggplot2::aes(x = time, y = R, color = "Green"), linewidth = 1.5) +
    ggplot2::geom_line(ggplot2::aes(x = time, y = D, color = "Orange"), linewidth = 1.5) +
    ggplot2::scale_color_identity(
      name = "SEIRD",
      breaks = c("Blue", "Brown", "Red", "Green", "Orange"),
      labels = c("Susceptible", "Exposed", "Infected", "Recovered", "Dead"),
      guide = "legend"
    )
}

plotPhasePlaneSEIRD <- function() {
  ggplot2::ggplot(solveSEIRD(), ggplot2::aes(x = S)) +
    ggplot2::geom_line(ggplot2::aes(y = I, color = "Blue"), linewidth = 1.5) +
    ggplot2::theme(
      axis.line = ggplot2::element_line(color = "black"),
      axis.text = ggplot2::element_text(size = 14),
      axis.title.x = ggplot2::element_text(size = 16, face = "bold"),
      axis.title.y = ggplot2::element_text(size = 16, face = "bold")
    ) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::ggtitle("SEIRD Phase Plane") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 22, face = "bold")) +
    ggplot2::ylab("Infected (I)") +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::xlab("Susceptible (S)") +
    ggplot2::scale_color_identity(
      name = "SEIRD",
      breaks = c("Blue", "Brown", "Red", "Green", "Orange"),
      labels = c("Susceptible", "Exposed", "Infected", "Recovered", "Dead"),
      guide = "legend"
    )
}

solveAndRenderSEIRD <- function(beta, gamma, sigma, delta, muB, muD, population, susceptible, exposed, infected, recovered, dead, timesteps, q) {
  expr <- quote({
    model <- solveSEIRD(beta, gamma, sigma, delta, muB, muD, population, susceptible, exposed, infected, recovered, dead, timesteps, q)
    output$modelPlot <- renderPlot(plotSEIRD(model))
    output$modelPhasePlane <- renderPlot(plotPhasePlaneSEIRD(model))
    output$modelSummaryTable <- renderTable(model[, 1:6])
  })

  eval.parent(expr)
}
