# Define a common theme for all plots
plotTheme <- ggplot2::theme(
  axis.line = ggplot2::element_line(color = "black"),
  axis.text = ggplot2::element_text(size = 14),
  axis.title.x = ggplot2::element_text(size = 16, face = "bold"),
  axis.title.y = ggplot2::element_text(size = 16, face = "bold"),
  plot.title = ggplot2::element_text(size = 22, face = "bold"),
  legend.position = "bottom"
)

plotSettings <- list(
  ggplot2::scale_color_brewer(palette = "Dark2"),
  ggplot2::scale_x_continuous(expand = c(0, 0)),
  ggplot2::scale_y_continuous(expand = c(0, 0))
)

# Plot SEIR model
plotSEIR <- function(model) {
  plot <- ggplot2::ggplot(model, ggplot2::aes(x = time)) +
    plotTheme +
    ggplot2::labs(
      title = "SEIR Epidemic Model", 
      y = "Number of People",
      x = "Time"
    ) +
    ggplot2::geom_line(ggplot2::aes(y = S, color = "Susceptible"), linewidth = 0.7) +
    ggplot2::geom_line(ggplot2::aes(y = E, color = "Exposed"), linewidth = 0.7) +
    ggplot2::geom_line(ggplot2::aes(y = I, color = "Infected"), linewidth = 0.7) +
    ggplot2::geom_line(ggplot2::aes(y = R, color = "Recovered"), linewidth = 0.7) +
    ggplot2::guides(color = ggplot2::guide_legend(title = "SEIR")) 

  Reduce(`+`, c(list(plot), plotSettings))
}

# Plot phase plane for SEIR model
plotPhasePlaneSEIR <- function(model) {
  plot <- ggplot2::ggplot(model, ggplot2::aes(x = S)) +
    ggplot2::geom_line(ggplot2::aes(y = I, color = "Infected"), linewidth = 1.5) + 
    plotTheme +
    ggplot2::ggtitle("SI Phase Plane") +
    ggplot2::ylab("Infected (I)") +
    ggplot2::xlab("Susceptible (S)") +
    ggplot2::guides(color = ggplot2::guide_legend(title = "SEIR")) 

  Reduce(`+`, c(list(plot), plotSettings))
}

# Plot subplots for SEIR model compartments
plotSubPlotsSEIR <- function(model) {
  data_long <- tidyr::pivot_longer(model, cols = c(S, E, I, R), names_to = "Compartment", values_to = "Value")

  x_limits <- range(data_long$time)
  y_limits <- range(data_long$Value)

  plotMain <- ggplot2::ggplot(data_long, ggplot2::aes(x = time, y = Value, color = Compartment)) +
    ggplot2::geom_line() +
    ggplot2::scale_color_brewer(palette = "Dark2") 

  compartment_colors <- ggplot2::ggplot_build(plotMain)$data[[1]]$colour %>% 
    unique()

  lapply(unique(data_long$Compartment), function(compartment) {
    ggplot2::ggplot(dplyr::filter(data_long, Compartment == compartment), ggplot2::aes(x = time, y = Value, color = Compartment)) +
      ggplot2::geom_line(linewidth = 0.7) +
      ggplot2::labs(
        title = paste("Compartment:", compartment),
        x = "Time",
        y = "Number of People"
      ) +
      plotTheme +
      ggplot2::scale_x_continuous(limits = x_limits, expand = c(0, 0)) +
      ggplot2::scale_y_continuous(limits = y_limits, expand = c(0, 0)) +
      ggplot2::scale_color_manual(values = setNames(compartment_colors, unique(data_long$Compartment))) +
      ggplot2::theme(legend.position = "none") 
  })
}