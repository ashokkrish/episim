equationsSEIR <- function(time, variables, parameters, q = 0, ...) {
  with(
    as.list(parameters),
    with(as.list(variables), {
      ## TODO: review R's operator precedence rules, and the associated
      ## algebraic equalities: a(bc/d) ~= abc/d ~= (abc)/d. My algebra is poor.
      betaSINq <- beta * ((S * I) / N^q)

      dS <- sum(muB * N, -(muD * S), xi * R, -betaSINq)
      dE <- sum(betaSINq, -(gamma * E), -(muD * E))
      dI <- sum(gamma * E, -(sigma * I), -(muD * I))
      dR <- sum(sigma * I, -(xi * R), -(muD * R))
      dN <- dS + dE + dI + dR

      list(c(dS, dE, dI, dR, dN), q)
    })
  )
}

solveSEIR <-
  function(beta = 0.001, gamma = 0.1, muB = 0, muD = 0, xi = 0.05,
           population = 500, susceptible = 498, infected = 1, sigma = 0.01,
           recovered = 0, timesteps = 50, q = 0, exposed = 1, ...) {

    ## Assign the variables and parameters to vectors then name them, makign the
    ## function readable.
    variables <- c(susceptible, exposed, infected, recovered, population)
    names(variables) <- c("S", "E", "I", "R", "N")
    parameters <- c(beta, gamma, muB, muD, xi, sigma)
    names(parameters) <- c("beta", "gamma", "muB", "muD", "xi", "sigma")

    deSolve::lsoda(variables,
                   seq(0, timesteps, by = 1),
                   equationsSEIR,
                   q = q,
                   parameters, ...) |>
      as.data.frame()
  }

plotTheme <- ggplot2::theme(
  axis.line = ggplot2::element_line(color = "black"),
  axis.text = ggplot2::element_text(size = 14),
  axis.title.x = ggplot2::element_text(size = 16, face = "bold"),
  axis.title.y = ggplot2::element_text(size = 16, face = "bold"),
  plot.title = ggplot2::element_text(size = 22, face = "bold"),
  legend.position = "bottom"
)

plotSEIR <- function(model) {
  ggplot2::ggplot(model, ggplot2::aes(x = time, linewidth = 1.5)) +
    plotTheme +
    ggplot2::labs(title = "SIRD Epidemic Model",
                  y = "Number of People",
                  x = "Time") +
    ggplot2::geom_line(ggplot2::aes(y = S, color = "Blue")) +
    ggplot2::geom_line(ggplot2::aes(y = E, color = "Blue")) +
    ggplot2::geom_line(ggplot2::aes(y = I, color = "Red")) +
    ggplot2::geom_line(ggplot2::aes(y = R, color = "Green")) +
    ggplot2::geom_line(ggplot2::aes(y = D, color = "Orange")) +
    ggplot2::scale_x_continuous(ggplot2::expansion()) +
    ggplot2::scale_y_continuous(ggplot2::expansion()) +
    ggplot2::scale_color_identity(
      name = "SIRD",
      breaks = c("Blue", "Yellow", "Red", "Green", "Orange"),
      labels = c("Susceptible", "Infected", "Recovered", "Dead"),
      guide = "legend"
    )
}

plotPhasePlaneSEIR <- function(model) {
  ggplot2::ggplot(model, ggplot2::aes(x = S)) +
    ggplot2::geom_line(ggplot2::aes(y = I, color = "Blue"), linewidth = 1.0) +
    plotTheme +
    ggplot2::ggtitle("SEI Phase Plane") +
    ggplot2::theme(plot.title = ggplot2::element_text(
      size = 22,
      face = "bold"
    )) +
    ggplot2::ylab("Infected (I)") +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::xlab("Susceptible (S)") +
    ggplot2::scale_color_identity("SEIR",
      breaks = c(
        "Blue",
        "Yellow",
        "Red",
        "Green",
        "Orange"
      ),
      labels = c(
        "Susceptible",
        "Exposed",
        "Infected",
        "Recovered",
        "Dead"
      ),
      guide = "legend"
    )
}

solveAndRenderSEIR <- function() {
  expr <- quote({
    model <- solveSEIR()
    output$modelPlot <- renderPlot(plotSEIR(model))
    output$modelPhasePlane <- renderPlot(plotPhasePlaneSEIR(model))
    output$modelSummaryTable <- renderTable(model[, 1:6])
  })

  eval.parent(expr)
}
