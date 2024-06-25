# episim: Compartmental Models of Epidemiology
episim is a Shiny applicatin for interactive, mathematical modelling of infectious diseases in six deterministic compartmental models of disease transmission, and a growing number of stochastic compartmental models.

The (deterministic) compartmental models supported are
- SIR
- SIRS
- SIRD
- SEIR
- SEIRS
- SEIRD

## Overview
Non-spatial epidemic modeling refers to the study and simulation of disease spread without explicitly considering the spatial location or geographic distribution of individuals or populations. In contrast to spatial epidemic modeling, which takes into account the physical locations of individuals or groups, non-spatial models focus on the dynamics of disease transmission within a population without considering the spatial relationships.

These models typically involve mathematical equations and simulations to represent the interactions between individuals, the transmission of the infectious agent, and the progression of the disease over time. Non-spatial models often use compartments to categorize individuals based on their health status, such as susceptible, infected, and recovered (SIR models) or susceptible, exposed, infected, and recovered (SEIR models).

Non-spatial epidemic models are valuable for understanding the basic dynamics of disease spread, estimating key parameters like transmission rates and recovery rates, and assessing the potential impact of interventions such as vaccination or social distancing measures. While spatial models are crucial for capturing the geographical patterns of disease transmission, non-spatial models provide a simpler and more abstract representation that can be computationally efficient and easier to analyze mathematically.

## Getting started
In the Posit RStudio IDE, open any of the top-level R files and click the <kbd>Run App</kbd> button.

## Features
In the R Shiny app's current state, it can run SIR, SIRD, SIRS, SEIR and SEIRD models. Each of these models can be run in either the true-mass action (frequency-dependent) or pseudo-mass action (density-dependent) formulation for a given population. Additionally, the option to run these models with vital dynamics with independent death (due to natural causes) and birth rates is also available. Plans to incorporate a stochastic version of each model are in the works and will be included in the next major version of the app.

Once appropriate parameters are selected by the user, the app will generate a time-series plot for all the epidemic compartments, a phase plane plot (for SI-type models) and an output summary for the selected model. Under the deterministic formulation the graphs are reactive to user input and will dynamically change as the user changes the parameter values. The mathematical model tab will also react to user input to show the correct mathematical model equations used to produce the output. A flow chart depicting how an individual may move through the different epidemic compartments is displayed, this is also reactive to user input.

## Credits
This [R Shiny](https://shiny.rstudio.com/) app would not be possible without the efforts of a team of research assistants.

### Latest team members
- [Tobias Wondwossen](https://github.com/Toby-exe)
- [Khanh Le](https://github.com/kle6951)
- [Bryce Carson](https://github.com/bryce-carson)

### Past members
- Michael
<!-- TODO: others -->

## Feedback
Development of the application is supervised by Dr. Ashok Krishnamurthy. We welcome questions, insights, and feedback.

Contact: [Ashok Krishnamurthy, Ph.D.](mailto:akrishnamurthy@mtroyal.ca)
Website: <https://bit.ly/2YKrXjX>
