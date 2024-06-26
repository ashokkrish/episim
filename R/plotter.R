defaultColors <- c(
    "#9467bd", "#2ca02c", "#ff7f0e", "#1f77b4", "#d62728",
    "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
)

# Plot theme settings
plotTheme <- ggplot2::theme(
    axis.line = ggplot2::element_line(color = "black"),
    axis.text = ggplot2::element_text(size = 14),
    axis.title.x = ggplot2::element_text(size = 14, face = "bold"),
    axis.title.y = ggplot2::element_text(size = 14, face = "bold"),
    plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "bottom"
)

compartment_names <- c(
    S = "Susceptible",
    E = "Exposed",
    I = "Infected",
    R = "Recovered",
    D = "Deceased"
)


plotter <- function(model) {

  compartments <- strsplit(model$selectedModel, "")[[1]]
  
  plot <- ggplot2::ggplot(model$data, ggplot2::aes(x = time)) +
    ggplot2::labs(
      title = paste(model$selectedModel, "Epidemic Model"),
      x = "Time",
      y = "Number of People"
    )
  
  if (model$plotterType == "binomial") {
    for (compartment in compartments) {
      plot <- plot + ggplot2::geom_line(
        ggplot2::aes_string(y = compartment, color = shQuote(compartment_names[compartment]), group = "iteration"),
        alpha = 0.3, linewidth = 0.7
      )
    }
  } else {
    for (compartment in compartments) {
      plot <- plot + ggplot2::geom_line(
        ggplot2::aes_string(y = compartment, color = shQuote(compartment_names[compartment])),
        linewidth = 0.7
      )
    }
  }
  
  plot <- plot +
    theme_classic() +
    plotTheme +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::guides(color = ggplot2::guide_legend(title = paste(model$selectedModel, "Model")))
  
}

# Phase plane plotter for SI models
phasePlanePlotterSI <- function(model) {

    if (model$plotterType == "binomial") {
        plot <- ggplot2::ggplot(model$data, ggplot2::aes(x = S, y = I, group = iteration, color = iteration)) +
            ggplot2::geom_path(alpha = 0.3, linewidth = 0.7) 
    } else {
        plot <- ggplot2::ggplot(model$data, ggplot2::aes(x = S, y = I)) +
            ggplot2::geom_path(color = "#1f77b4", linewidth = 0.7)
    }

    plot <- plot +
      theme_classic() +
      plotTheme +
      ggplot2::labs(
        title = paste(model$selectedModel, "Phase Plane"),
        x = "Susceptible (S)", 
        y = "Infected (I)"
      ) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = c(0, 0))

}

subPlotter <- function(model) {
    compartments <- unique(unlist(strsplit(model$selectedModel, "")))
    compartments <- compartments[!duplicated(compartments)]

    data_long <- tidyr::pivot_longer(model$data, cols = compartments, names_to = "Compartment", values_to = "Value")

    x_limits <- range(data_long$time)
    y_limits <- range(data_long$Value)

    colors <- defaultColors[seq_along(compartments)]
    names(colors) <- compartments

    plotCommon <- function(compartment, data_subset) {
        ggplot2::ggplot(data_subset, ggplot2::aes(x = time, y = Value, color = Compartment)) +
            ggplot2::labs(
                title = paste("Compartment:", compartment_names[compartment]),
                x = "Time",
                y = compartment_names[compartment]
            ) +
            ggplot2::scale_x_continuous(limits = x_limits, expand = c(0, 0)) +
            ggplot2::scale_y_continuous(limits = y_limits, expand = c(0, 0)) +
            ggplot2::scale_color_manual(values = colors) +
            theme_classic() +
            plotTheme +
            ggplot2::theme(legend.position = "none")
    }

    lapply(compartments, function(compartment) {
        data_subset <- dplyr::filter(data_long, Compartment == compartment)
        
        if (model$plotterType == "binomial") {
            plotCommon(compartment, data_subset) +
                ggplot2::geom_line(linewidth = 0.7, alpha = 0.3, ggplot2::aes(group = iteration))
        } else {
            plotCommon(compartment, data_subset) +
                ggplot2::geom_line(linewidth = 0.7)
        }
    })
}


