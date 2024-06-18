# Default colors for the compartments
defaultColors <- c("#9467bd", "#2ca02c", "#ff7f0e", "#1f77b4", "#d62728", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")

# Plot theme settings
plotTheme <- ggplot2::theme(
  axis.line = ggplot2::element_line(color = "black"),
  axis.text = ggplot2::element_text(size = 14),
  axis.title.x = ggplot2::element_text(size = 16, face = "bold"),
  axis.title.y = ggplot2::element_text(size = 16, face = "bold"),
  plot.title = ggplot2::element_text(size = 22, face = "bold"),
  legend.position = "bottom"
)

plotSettings <- list(
  ggplot2::scale_x_continuous(expand = c(0, 0)),
  ggplot2::scale_y_continuous(expand = c(0, 0))
)

compartment_names <- c(
  S = "Susceptible",
  E = "Exposed",
  I = "Infected",
  R = "Recovered",
  D = "Deceased"
)

# Main plot function
plotter <- function(model, settings) {

  title <- if (!is.null(settings$title) && settings$title != "") {
    settings$title
  } else {
    paste(model$selectedModel, "Epidemic Model")
  }

  xAxisLabel <- if (!is.null(settings$xAxisLabel) && settings$xAxisLabel != "") {
    settings$xAxisLabel
  } else {
    "Time"
  }

  yAxisLabel <- if (!is.null(settings$yAxisLabel) && settings$yAxisLabel != "") {
    settings$yAxisLabel
  } else {
    "Number of People"
  }

  compartments <- strsplit(model$selectedModel, "")[[1]]


  if (!is.null(settings$colors) && length(settings$colors) > 0) {
      settings$colors <- Filter(Negate(is.null), settings$colors)

      if (length(settings$colors) > 0) {
          colors <- settings$colors
          names(colors) <- compartment_names[compartments]
          color_scale <- ggplot2::scale_color_manual(values = colors)
      } else {
          colors <- defaultColors[seq_along(compartments)]
          names(colors) <- compartment_names[compartments]
      }
  } else {
      colors <- defaultColors[seq_along(compartments)]
      names(colors) <- compartment_names[compartments]
  }

  plot <- ggplot2::ggplot(model$data, ggplot2::aes(x = time)) +
    plotTheme +
    ggplot2::labs(
      title = title,
      x = xAxisLabel,
      y = yAxisLabel
    )

  if (model$plotterType == "Binomial") {
    for (compartment in compartments) {
      if (compartment %in% names(model$data)) {
        plot <- plot + ggplot2::geom_line(
          ggplot2::aes_string(y = compartment, group = iteration, alpha = 0.3, color = shQuote(compartment_names[compartment])),
          linewidth = 0.7
        )
      }
    }
  } else {
    for (compartment in compartments) {
      if (compartment %in% names(model$data)) {
        plot <- plot + ggplot2::geom_line(
          ggplot2::aes_string(y = compartment, color = shQuote(compartment_names[compartment])),
          linewidth = 0.7
        )
      }
    }
  }

  plot <- plot + ggplot2::scale_color_manual(values = colors) +
    theme_classic() +
    ggplot2::guides(color = ggplot2::guide_legend(title = model$selectedModel)) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))  # Center the plot title

  plot <- Reduce(`+`, c(list(plot), plotSettings))

  return(plot)
}

phasePlanePlotterSI <- function(model, settings) {

  title <- if (!is.null(settings$title) && settings$title != "") {
    settings$title
  } else {
    paste(model$selectedModel, "Phase Plane")
  }

  xAxisLabel <- if (!is.null(settings$xAxisLabel) && settings$xAxisLabel != "") {
    settings$xAxisLabel
  } else {
    "Susceptible (S)"
  }

  yAxisLabel <- if (!is.null(settings$yAxisLabel) && settings$yAxisLabel != "") {
    settings$yAxisLabel
  } else {
    "Infected (I)"
  }

  color <- if (!is.null(settings$color)) {
    settings$color
  } else {
    defaultColors[1]  
  }

  plot <- ggplot2::ggplot(model$data, ggplot2::aes(x = S, y = I)) +
    ggplot2::geom_line(color = color, linewidth = 0.7) +
    plotTheme +
    theme_classic() +
    ggplot2::labs(
      title = title,
      x = xAxisLabel,
      y = yAxisLabel
    ) +
    ggplot2::theme(legend.position = "none")

  plot <- Reduce(`+`, c(list(plot), plotSettings))

  return(plot)
}

subPlotter <- function(model) {

  compartments <- unique(unlist(strsplit(model$selectedModel, "")))
  compartments <- compartments[!duplicated(compartments)]
  
  data_long <- tidyr::pivot_longer(model$data, cols = compartments, names_to = "Compartment", values_to = "Value")
  
  x_limits <- range(data_long$time)
  y_limits <- range(data_long$Value)
  
  colors <- defaultColors[seq_along(compartments)]
  names(colors) <- compartments
  
  lapply(compartments, function(compartment) {
    ggplot2::ggplot(dplyr::filter(data_long, Compartment == compartment), ggplot2::aes(x = time, y = Value, color = Compartment)) +
      ggplot2::geom_line(linewidth = 0.7) +
      ggplot2::labs(
        title = paste("Compartment:", compartment_names[compartment]),
        x = "Time",
        y = compartment_names[compartment]
      ) +
      plotTheme +
      ggplot2::scale_x_continuous(limits = x_limits, expand = c(0, 0)) +
      ggplot2::scale_y_continuous(limits = y_limits, expand = c(0, 0)) +
      ggplot2::scale_color_manual(values = colors) +
      theme_classic() +
      ggplot2::theme(legend.position = "none")
  })
}
