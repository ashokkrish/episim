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
  if(!mu)
  {
    if (qValue==0){helpText(generate_latex(c(SIR_PMA_nonVD,SIR_nonVD_equation)))}
    else {helpText(generate_latex(c(SIR_TMA_nonVD,SIR_nonVD_equation)))}
  } else 
  {
    if (qValue==0){helpText(generate_latex(c(SIR_PMA_VD,SIR_VD_equation)))}
    else {helpText(generate_latex(c(SIR_TMA_VD,SIR_VD_equation)))}
  }
}

SIRS_LaTeX <- function(mu,qValue) {
  if(!mu)
  {
    if (qValue==0){helpText(generate_latex(c(SIRS_PMA_nonVD,SIRS_nonVD_equation)))}
    else {helpText(generate_latex(c(SIRS_TMA_nonVD,SIRS_nonVD_equation)))}
  } else {
    if (qValue==0){helpText(generate_latex(c(SIRS_PMA_VD,SIRS_VD_equation)))}
    else {helpText(generate_latex(c(SIRS_TMA_VD,SIRS_VD_equation)))}
  }
}

SIRD_LaTeX <- function(mu,qValue) {
  if(!mu)
  {
    if (qValue==0){helpText(generate_latex(c(SIRD_PMA_nonVD,SIRD_nonVD_equation)))}
    else {helpText(generate_latex(c(SIRD_TMA_nonVD,SIRD_nonVD_equation)))}
  } else {
    if (qValue==0){helpText(generate_latex(c(SIRD_PMA_VD,SIRD_VD_equation)))}
    else {helpText(generate_latex(c(SIRD_TMA_VD,SIRD_VD_equation)))}
  }
}

SEIR_LaTeX <- function(mu,qValue) {
  if(!mu)
  {
    if (qValue==0){helpText(generate_latex(c(SEIR_PMA_nonVD,SEIR_nonVD_equation)))}
    else {helpText(generate_latex(c(SEIR_TMA_nonVD,SEIR_nonVD_equation)))}
  } else {
    if (qValue==0){helpText(generate_latex(c(SEIR_PMA_VD,SEIR_VD_equation)))}
    else {helpText(generate_latex(c(SEIR_TMA_VD,SEIR_VD_equation)))}
  }
}

SEIRD_LaTeX <- function(mu,qValue) {
  if(!mu)
  {
    if (qValue==0){helpText(generate_latex(c(SEIRD_PMA_nonVD,SEIRD_nonVD_equation)))}
    else {helpText(generate_latex(c(SEIRD_TMA_nonVD,SEIRD_nonVD_equation)))}
  } else {
    if (qValue==0){helpText(generate_latex(c(SEIRD_PMA_VD,SEIRD_VD_equation)))}
    else {helpText(generate_latex(c(SEIRD_TMA_VD,SEIRD_VD_equation)))}
  }
}

## FIXME: is this working or not?
renderModelLaTeX <- function(model, vitalStatistics,massAction) {
  tagList(withMathJax(eval(call(paste0(model, "_LaTeX"), vitalStatistics, massAction))))
}
