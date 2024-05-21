plotSEIRD <- function(model) {
  ggplot2::ggplot(model, ggplot2::aes(x = time)) +
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

plotPhasePlaneSEIRD <- function(model) {
  ggplot2::ggplot(model, ggplot2::aes(x = S)) +
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
