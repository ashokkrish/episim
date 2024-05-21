plotSIRD <- function(model) {
  ggplot2::ggplot(model, ggplot2::aes(x = time)) +
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
    ggplot2::geom_line(ggplot2::aes(y = S, color = "Blue"), linewidth = 1.5) +
    ggplot2::geom_line(ggplot2::aes(y = I, color = "Red"), linewidth = 1.5) +
    ggplot2::geom_line(ggplot2::aes(y = R, color = "Green"), linewidth = 1.5) +
    ggplot2::geom_line(ggplot2::aes(y = D, color = "Orange"), linewidth = 1.5) +
    ggplot2::scale_color_identity(
      name = "SIRD", breaks = c("Blue", "Red", "Green", "Orange"),
      labels = c("Susceptible", "Infected", "Recovered", "Dead"), guide = "legend"
    )
}

plotPhasePlaneSIRD <- function(model) {
  ggplot2::ggplot(model, ggplot2::aes(x = S)) +
    ggplot2::geom_line(ggplot2::aes(y = I, color = "Blue"), linewidth = 1.5) +
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
