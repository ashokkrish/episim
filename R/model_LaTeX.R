# Define the CSS styles once
css_styles <- HTML(paste0(
  '<style>',
  '  .equation-container {',
  '    display: flex;',
  '    justify-content: flex-start;',
  '  }',
  '  .equation-block {',
  '    margin: 0;',
  '    padding: 0;',
  '  }',
  '</style>'
))

# Helper function to generate the LaTeX content
generate_latex <- function(equations) {
  HTML(paste0(
    css_styles,
    '<div class="equation-container">',
    '<blockquote class="equation-block">',
    r"(\begin{align*})",
    paste(equations, collapse = r"(\\)"),
    r"(\end{align*})",
    '</blockquote>',
    '</div>'
  ))
}

renderModelLaTeX <- function(reactiveValuesList) {
  equations <- do.call(
    reactiveValuesList$modelSelect,
    list(
      reactiveValuesList$vitalDynamics,
      forceOfInfection(reactiveValuesList$trueMassAction)
    )
  )

  generate_latex(equations) |> helpText() |> withMathJax()
}
