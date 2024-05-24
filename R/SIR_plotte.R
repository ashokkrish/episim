plotTheme <- ggplot2::theme(
  axis.line = ggplot2::element_line(color = "black"),
  axis.text = ggplot2::element_text(size = 14),
  axis.title.x = ggplot2::element_text(size = 16, face = "bold"),
  axis.title.y = ggplot2::element_text(size = 16, face = "bold"),
  plot.title = ggplot2::element_text(size = 22, face = "bold"),
  legend.position = "bottom"
)

# Plot
plotSIR <- function(model)
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
      name = "SIR", breaks = c("Blue", "Red", "Green"),
      labels = c("Susceptible", "Infected", "Recovered"), guide = "legend"
    )
}

# plot phase plane
plotPhasePlaneSIR <- function(model)
{
  ggplot2::ggplot(model, ggplot2::aes(x = S))+

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

plotSubPlotsSIR <- function(model) {
  data_long <- tidyr::pivot_longer(model, cols = c(S, I, R), names_to = "Compartment", values_to = "Value")

  x_limits <- range(data_long$time)
  y_limits <- range(data_long$Value)

  plotMain <- ggplot2::ggplot(data_long, ggplot2::aes(x = time, y = Value, color = Compartment)) +
    ggplot2::geom_line() +
    ggplot2::scale_color_brewer(palette = "Dark2")

  compartment_colors <- ggplot2::ggplot_build(plotMain)$data[[1]]$colour %>%
    unique()

  plotList <- lapply(unique(data_long$Compartment), function(compartment) {
    p <- ggplot2::ggplot(dplyr::filter(data_long, Compartment == compartment), ggplot2::aes(x = time, y = Value, color = Compartment)) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::labs(
        title = paste("Compartment:", compartment),
        x = "Time",
        y = "Number of People"
      ) +
      ggplot2::theme(
        axis.line = ggplot2::element_line(color = "black"),
        axis.text = ggplot2::element_text(size = 10),
        axis.title.x = ggplot2::element_text(size = 10, face = "bold"),
        axis.title.y = ggplot2::element_text(size = 10, face = "bold"),
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        legend.position = "none"
      ) +
      ggplot2::scale_x_continuous(limits = x_limits, expand = c(0, 0)) +
      ggplot2::scale_y_continuous(limits = y_limits, expand = c(0, 0)) +
      ggplot2::scale_color_manual(values = setNames(compartment_colors, unique(data_long$Compartment)))
    
    plotly::ggplotly(p)
  })

  subplotPlotly <- plotly::subplot(
    plotList,
    nrows = 2,
    shareX = FALSE,
    shareY = FALSE,
    titleX = TRUE,
    titleY = TRUE,
    margin = 0.05
  )
}
