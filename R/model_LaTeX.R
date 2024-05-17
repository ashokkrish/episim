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
    '\\begin{align*}',
    paste(equations, collapse = '\\\\'),
    '\\end{align*}',
    '</blockquote>',
    '</div>'
  ))
}

SIR_LaTeX <- function(mu,qValue) {
  forceOfInfection <- getForceOfInfection(qValue)
  helpText(generate_latex(SIR(mu,forceOfInfection)))
}

SIRS_LaTeX <- function(mu,qValue) {
  forceOfInfection <- getForceOfInfection(qValue)
  helpText(generate_latex(SIRS(mu,forceOfInfection)))
}

SIRD_LaTeX <- function(mu,qValue) {
  forceOfInfection <- getForceOfInfection(qValue)
  helpText(generate_latex(SIRD(mu,forceOfInfection)))
}

SEIR_LaTeX <- function(mu,qValue) {
  forceOfInfection <- getForceOfInfection(qValue)
  helpText(generate_latex(SEIR(mu,forceOfInfection)))
}

SEIRS_LaTeX <- function(mu,qValue) {
  forceOfInfection <- getForceOfInfection(qValue)
  helpText(generate_latex(SEIRS(mu,forceOfInfection)))
}

SEIRD_LaTeX <- function(mu,qValue) {
  forceOfInfection <- getForceOfInfection(qValue)
  helpText(generate_latex(SEIRD(mu,forceOfInfection)))
}

## FIXME: is this working or not?
renderModelLaTeX <- function(model, vitalStatistics,massAction) {
  tagList(withMathJax(eval(call(paste0(model, "_LaTeX"), vitalStatistics, massAction))))
}
