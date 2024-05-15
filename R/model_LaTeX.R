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

SIR_LaTeX <- function(mu) {
  switch(mu + 1, 
         helpText(generate_latex(c(SIR_nonVD_equation))),
         helpText(generate_latex(c(SIR_VD_equation)))
  )
}

SIRS_LaTeX <- function(mu) {
  switch(mu + 1, 
         helpText(generate_latex(c(SIRS_nonVD_equation))),
         helpText(generate_latex(c(SIRS_VD_equation )))
  )
}

SIRD_LaTeX <- function(mu) {
  switch(mu + 1,
         helpText(generate_latex(c(SIRD_nonVD_equation))),
        helpText(generate_latex(c(SIRD_VD_equation)))
  )
}

SEIR_LaTeX <- function(mu) {
  switch(mu + 1,
         helpText(generate_latex(c(SEIR_nonVD_equation))),
         helpText(generate_latex(c(SEIR_VD_equation)))
  )
}

SEIRD_LaTeX <- function(mu) {
  switch(mu + 1,
         helpText(generate_latex(c(SEIRD_nonVD_equation))),
         helpText(generate_latex(c(SEIRD_VD_equation)))
  )
}

## FIXME: is this working or not?
renderModelLaTeX <- function(model, vitalStatistics) {
  tagList(withMathJax(eval(call(paste0(model, "_LaTeX"), vitalStatistics))))
}
