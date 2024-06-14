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
  '    font-size: 20px;', 
  '  }',
  '</style>'
))

# Helper function to generate the LaTeX content
generate_latex <- function(equations) {
  HTML(paste0(
    css_styles,
    '<div class="equation-container">',
    '<div class="equation-block">',
    r"(\begin{align*})",
    paste(equations, collapse = r"(\\)"),
    r"(\end{align*})",
    '</div>',
    '</div>'
  ))
}

renderModelLaTeX <- function(modelSelect, vitalDynamics, trueMassAction) {
  do.call(modelSelect,
          list(vitalDynamics, forceOfInfection(trueMassAction))) |>
    generate_latex() |>
    helpText() |>
    withMathJax()
}

renderStochasticModelLaTex <- function(modelSelect, trueMassAction){
  do.call(paste0(modelSelect,"_nonVD"),
          list(forceOfInfection(trueMassAction))) |>
    generate_latex() |>
    helpText() |>
    withMathJax()
}
