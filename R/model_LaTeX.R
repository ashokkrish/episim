SIR_LaTeX <- function(mu) {
  switch(mu + 1, 
    helpText(
      HTML(paste0(
        '<style>',
        '  .equation-container {',
        '    display: flex;',
        '    justify-content: flex-start;',
        '  }',
        '  .equation-block {',
        '    margin: 0;',
        '    padding: 0;',
        '  }',
        '</style>',
        '<div class="equation-container">',
        '<blockquote class="equation-block">',
        '\\begin{align*}',
        '    &', r"(\frac{dS}{dt} = - \beta \frac{ S I}{N^q}\\)", 
        '    &', r"(\frac{dI}{dt} = \frac{\beta S I}{N^q} - \gamma {I})", ' \\\\',
        '    &', r"(\frac{dR}{dt} = \gamma {I})", ' \\\\',
        '    &', r"(R_0 =  \frac{\beta}{\gamma} S(0)^q)", ' \\\\',
        '    &', r"(\begin{cases} 1 & \text{, frequency-dependent} \\\\ 0 & \text{, density-dependent} \end{cases})", ' \\\\',
        '\\end{align*}',
        '</blockquote>',
        '</div>'
      ))
    ),
    helpText(
      HTML(paste0(
        '<style>',
        '  .equation-container {',
        '    display: flex;',
        '    justify-content: flex-start;',
        '  }',
        '  .equation-block {',
        '    margin: 0;',
        '    padding: 0;',
        '  }',
        '</style>',
        '<div class="equation-container">',
        '<blockquote class="equation-block">',
        '\\begin{align*}',
        '    &', r"(\frac{dS}{dt} = \mu_B N - \mu_D S - \beta \frac{ S I}{N^q})", ' \\\\',
        '    &', r"(\frac{dI}{dt} = \frac{\beta S I}{N^q} - \gamma I - \mu_D {I})", ' \\\\',
        '    &', r"(\frac{dR}{dt} = \gamma I - \mu_D {R})", ' \\\\',
        '    &', r"(R_0 =  \frac{\beta}{\gamma} S(0)^q)", ' \\\\',
        '    &', r"(\begin{cases} 1 & \text{, frequency-dependent} \\\\ 0 & \text{, density-dependent} \end{cases})", ' \\\\',
        '\\end{align*}',
        '</blockquote>',
        '</div>'
      ))
    )
  )
}

SIRS_LaTeX <- function(mu) {
  switch(mu + 1, 
         helpText(
           HTML(paste0(
             '<style>',
             '  .equation-container {',
             '    display: flex;',
             '    justify-content: flex-start;',
             '  }',
             '  .equation-block {',
             '    margin: 0;',
             '    padding: 0;',
             '  }',
             '</style>',
             '<div class="equation-container">',
             '<blockquote class="equation-block">',
             '\\begin{align*}',
             '    &', r"(\frac{dS}{dt} = - \beta \frac{ S I}{N} + \xi{R})", ' \\\\',
             '    &', r"(\frac{dI}{dt} = \frac{\beta S I}{N} - \gamma {I})", ' \\\\',
             '    &', r"(\frac{dR}{dt} = \gamma {I} - \xi{R})", ' \\\\',
             '    &', r"(R_0 =  \frac{\beta}{\gamma} S(0)^q)", ' \\\\',
             '    &', r"(\begin{cases} 1 & \text{, frequency-dependent} \\\\ 0 & \text{, density-dependent} \end{cases})", ' \\\\',
             '\\end{align*}',
             '</blockquote>',
             '</div>'
           ))
         ),
         helpText(
           HTML(paste0(
             '<style>',
             '  .equation-container {',
             '    display: flex;',
             '    justify-content: flex-start;',
             '  }',
             '  .equation-block {',
             '    margin: 0;',
             '    padding: 0;',
             '  }',
             '</style>',
             '<div class="equation-container">',
             '<blockquote class="equation-block">',
             '\\begin{align*}',
             '    &', r"(\frac{dS}{dt} = \mu{N}- \beta \frac{ S I}{N} + \xi{R} - \nu{S})", ' \\\\',
             '    &', r"(\frac{dI}{dt} = \frac{\beta S I}{N} - \gamma {I} - \nu{I})", ' \\\\',
             '    &', r"(\frac{dR}{dt} = \gamma {I} - \xi{R} - \nu{R})", ' \\\\',
             '    &', r"(R_0 =  \frac{\beta}{\gamma} S(0)^q)", ' \\\\',
             '    &', r"(\begin{cases} 1 & \text{, frequency-dependent} \\\\ 0 & \text{, density-dependent} \end{cases})", ' \\\\',
             '\\end{align*}',
             '</blockquote>',
             '</div>'
           ))
         )
  )
}

SIRD_LaTeX <- function(mu) {
  switch(mu + 1,
    helpText(
      HTML(paste0(
        '<style>',
        '  .equation-container {',
        '    display: flex;',
        '    justify-content: flex-start;',
        '  }',
        '  .equation-block {',
        '    margin: 0;',
        '    padding: 0;',
        '  }',
        '</style>',
        '<div class="equation-container">',
        '<blockquote class="equation-block">',
        '\\begin{align*}',
        '    &', r"(\frac{dS}{dt} = - \beta \frac{ S I}{N^q})", ' \\\\',
        '    &', r"(\frac{dI}{dt} = \frac{\beta S I}{N^q} - \gamma {I} - \delta {I} )", ' \\\\',
        '    &', r"(\frac{dR}{dt} = \gamma {I} )", ' \\\\',
        '    &', r"(\frac{dD}{dt} = \delta {I})", ' \\\\',
        '    &', r"(R_0 =  \frac{\beta}{\gamma} S(0)^q)", ' \\\\',
        '    &', r"(\begin{cases} 1 & \text{, frequency-dependent} \\\\ 0 & \text{, density-dependent} \end{cases})", ' \\\\',
        '\\end{align*}',
        '</blockquote>',
        '</div>'
      ))
    ),
    helpText(
      HTML(paste0(
        '<style>',
        '  .equation-container {',
        '    display: flex;',
        '    justify-content: flex-start;',
        '  }',
        '  .equation-block {',
        '    margin: 0;',
        '    padding: 0;',
        '  }',
        '</style>',
        '<div class="equation-container">',
        '<blockquote class="equation-block">',
        '\\begin{align*}',
        '    &', r"(\frac{dS}{dt} =\mu_B {N} - \mu_D {S} - \beta \frac{ S I}{N^q})", ' \\\\',
        '    &', r"(\frac{dI}{dt} = \frac{\beta S I}{N^q} - \gamma {I} - \delta {I} - \mu_D {I})", ' \\\\',
        '    &', r"(\frac{dR}{dt} = \gamma {I} - \mu_D {R} )", ' \\\\',
        '    &', r"(\frac{dD}{dt} = \delta {I})", ' \\\\',
        '    &', r"(R_0 =  \frac{\beta}{\gamma} S(0)^q)", ' \\\\',
        '    &', r"(\begin{cases} 1 & \text{, frequency-dependent} \\\\ 0 & \text{, density-dependent} \end{cases})", ' \\\\',
        '\\end{align*}',
        '</blockquote>',
        '</div>'
      ))
    )
  )
}

SEIR_LaTeX <- function(mu) {
  switch(mu + 1,
    helpText(
      HTML(paste0(
        '<style>',
        '  .equation-container {',
        '    display: flex;',
        '    justify-content: flex-start;',
        '  }',
        '  .equation-block {',
        '    margin: 0;',
        '    padding: 0;',
        '  }',
        '</style>',
        '<div class="equation-container">',
        '<blockquote class="equation-block">',
        '\\begin{align*}',
        '    &', r"(\frac{dS}{dt} = - \beta \frac{ S I}{N^q})", ' \\\\',
        '    &', r"(\frac{dE}{dt} = \beta \frac{ S I}{N^q} - \gamma {E})", ' \\\\',
        '    &', r"(\frac{dI}{dt} = \gamma {E} - \sigma {I})", ' \\\\',
        '    &', r"(\frac{dR}{dt} = \sigma {I})", ' \\\\',
        '    &', r"(R_0 =  \frac{\beta}{\gamma} S(0)^q)", ' \\\\',
        '    &', r"(\begin{cases} 1 & \text{, frequency-dependent} \\\\ 0 & \text{, density-dependent} \end{cases})", ' \\\\',
        '\\end{align*}',
        '</blockquote>',
        '</div>'
      ))
    ),
    helpText(
      HTML(paste0(
        '<style>',
        '  .equation-container {',
        '    display: flex;',
        '    justify-content: flex-start;',
        '  }',
        '  .equation-block {',
        '    margin: 0;',
        '    padding: 0;',
        '  }',
        '</style>',
        '<div class="equation-container">',
        '<blockquote class="equation-block">',
        '\\begin{align*}',
        '    &', r"(\frac{dS}{dt} = \mu_B {N} - \mu_D {S} - \beta \frac{ S I}{N^q})", ' \\\\',
        '    &', r"(\frac{dE}{dt} = \beta \frac{ S I}{N^q} - \gamma {E} - - \mu_D {E})", ' \\\\',
        '    &', r"(\frac{dI}{dt} = \gamma {E} - \sigma {I} - \mu_D {I})", ' \\\\',
        '    &', r"(\frac{dR}{dt} = \sigma {I}) - \mu_D {R} )", ' \\\\',
        '    &', r"(R_0 =  \frac{\beta}{\gamma} S(0)^q)", ' \\\\',
        '    &', r"(\begin{cases} 1 & \text{, frequency-dependent} \\\\ 0 & \text{, density-dependent} \end{cases})", ' \\\\',
        '\\end{align*}',
        '</blockquote>',
        '</div>'
      ))
    )
  )
}

SEIRD_LaTeX <- function(mu) {
  switch(mu + 1,
    helpText(
      HTML(paste0(
        '<style>',
        '  .equation-container {',
        '    display: flex;',
        '    justify-content: flex-start;',
        '  }',
        '  .equation-block {',
        '    margin: 0;',
        '    padding: 0;',
        '  }',
        '</style>',
        '<div class="equation-container">',
        '<blockquote class="equation-block">',
        '\\begin{align*}',
        '    &', r"(\frac{dS}{dt} = - \beta \frac{ S I}{N^q})", ' \\\\',
        '    &', r"(\frac{dE}{dt} = \beta \frac{ S I}{N^q} - \gamma {E})", ' \\\\',
        '    &', r"(\frac{dI}{dt} = \gamma {E} - \sigma {I} - \delta {I} )", ' \\\\',
        '    &', r"(\frac{dR}{dt} = \sigma {I})", ' \\\\',
        '    &', r"(\frac{dD}{dt} = \delta {I})", ' \\\\',
        '    &', r"(R_0 =  \frac{\beta}{\gamma} S(0)^q)", ' \\\\',
        '    &', r"(\begin{cases} 1 & \text{, frequency-dependent} \\\\ 0 & \text{, density-dependent} \end{cases})", ' \\\\',
        '\\end{align*}',
        '</blockquote>',
        '</div>'
      ))
    ),
    helpText(
      HTML(paste0(
        '<style>',
        '  .equation-container {',
        '    display: flex;',
        '    justify-content: flex-start;',
        '  }',
        '  .equation-block {',
        '    margin: 0;',
        '    padding: 0;',
        '  }',
        '</style>',
        '<div class="equation-container">',
        '<blockquote class="equation-block">',
        '\\begin{align*}',
        '    &', r"(\frac{dS}{dt} = \mu_B {N} - \mu_D {S} - \beta \frac{ S I}{N^q})", ' \\\\',
        '    &', r"(\frac{dE}{dt} = \beta \frac{ S I}{N^q} - \gamma {E} - - \mu_D {E})", ' \\\\',
        '    &', r"(\frac{dI}{dt} = \gamma {E} - \sigma {I} -  \delta {I} - \mu_D {I})", ' \\\\',
        '    &', r"(\frac{dR}{dt} = \sigma {I}) - \mu_D {R} )", ' \\\\',
        '    &', r"(\frac{dD}{dt} = \delta {I})", ' \\\\',
        '    &', r"(R_0 =  \frac{\beta}{\gamma} S(0)^q)", ' \\\\',
        '    &', r"(\begin{cases} 1 & \text{, frequency-dependent} \\\\ 0 & \text{, density-dependent} \end{cases})", ' \\\\',
        '\\end{align*}',
        '</blockquote>',
        '</div>'
      ))
    )
  )
}

## FIXME: is this working or not?
renderModelLaTeX <- function(model, vitalStatistics) {
  tagList(withMathJax(call(paste0(model, "_LaTeX"), vitalStatistics)))
}
