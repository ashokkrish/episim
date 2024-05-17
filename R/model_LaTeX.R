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

## FIXME: is this working or not?
renderModelLaTeX <- function(model, vitalStatistics, massAction) {
  eval(call(model, vitalStatistics, getForceOfInfection(massAction))) |>
    generate_latex() |>
    helpText()}
