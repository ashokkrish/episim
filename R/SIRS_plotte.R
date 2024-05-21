plotTheme <- ggplot2::theme(
  axis.line = ggplot2::element_line(color = "black"),
  axis.text = ggplot2::element_text(size = 14),
  axis.title.x = ggplot2::element_text(size = 16, face = "bold"),
  axis.title.y = ggplot2::element_text(size = 16, face = "bold"),
  plot.title = ggplot2::element_text(size = 22, face = "bold"),
  legend.position = "bottom"
)

# Plot
plotSIRS <- function(model)
{
  ggplot2::ggplot(model, ggplot2::aes(x = time)) +
    plotTheme +
    ggplot2::labs(title = "SIRS Epidemic Model", y = "Number of People",
                  x = "Time") +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::geom_line(ggplot2::aes(y = S, color = "Blue"), linewidth = 1.5) +
    ggplot2::geom_line(ggplot2::aes(y = I, color = "Red"), linewidth = 1.5) +
    ggplot2::geom_line(ggplot2::aes(y = R, color = "Green"), linewidth = 1.5) +
    ggplot2::scale_color_identity(
      name = "SIRS", breaks = c("Blue", "Red", "Green"),
      labels = c("Susceptible", "Infected", "Recovered"), guide = "legend"
    )
}
# plot phase plane
plotPhasePlaneSIRS <- function(model)
{
  ggplot2::ggplot(model, ggplot2::aes(x = S))+
    ggplot2::geom_line(aes(y = I, color = "Blue"), linewidth = 1.5) +
    plotTheme +
    ggplot2::ggtitle("SI Phase Plane") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 22, face = "bold")) +
    ggplot2::ylab("Infected (I)") +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::xlab("Susceptible (S)") +
    ggplot2::scale_color_identity(
      name = "SIRS", breaks = c("Blue", "Red", "Green"),
      labels = c("Susceptible", "Infected", "Recovered"), guide = "legend"
    )
}
