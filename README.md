# episim: Compartmental Models of Epidemiology

## Overview
Non-spatial epidemic modeling refers to the study and simulation of disease spread without explicitly considering the spatial location or geographic distribution of individuals or populations. In contrast to spatial epidemic modeling, which takes into account the physical locations of individuals or groups, non-spatial models focus on the dynamics of disease transmission within a population without considering the spatial relationships.

These models typically involve mathematical equations and simulations to represent the interactions between individuals, the transmission of the infectious agent, and the progression of the disease over time. Non-spatial models often use compartments to categorize individuals based on their health status, such as susceptible, infected, and recovered (SIR models) or susceptible, exposed, infected, and recovered (SEIR models).

Non-spatial epidemic models are valuable for understanding the basic dynamics of disease spread, estimating key parameters like transmission rates and recovery rates, and assessing the potential impact of interventions such as vaccination or social distancing measures. While spatial models are crucial for capturing the geographical patterns of disease transmission, non-spatial models provide a simpler and more abstract representation that can be computationally efficient and easier to analyze mathematically.

Run the app by loading `global.R` and clicking `Run App`.

## Features
In the R Shiny app's current state, it can run SIR, SIRD, SIRS, SEIR and SEIRD models. Each of these models can be run in either the true-mass action (frequency-dependent) or pseudo-mass action (density-dependent) formulation for a given population. Additionally, the option to run these models with vital dynamics with independent death (due to natural causes) and birth rates is also available. Plans to incorporate a stochastic version of each model are in the works and will be included in the next major version of the app.

Once appropriate parameters are selected by the user, the app will generate a time-series plot for all the epidemic compartments, a phase plane plot (for SI-type models) and an output summary for the selected model. Under the deterministic formulation the graphs are reactive to user input and will dynamically change as the user changes the parameter values. The mathematical model tab will also react to user input to show the correct mathematical model equations used to produce the output. A flow chart depicting how an individual may move through the different epidemic compartments is displayed, this is also reactive to user input.

## Contents of the repository

The contents of the repository are currently the following:

- `global.R`: main file to run in RStudio console
- `ui.R`: user interface
- `server.R`: server side code
- `www`: a folder that contains the flowcharts for the mathematical models

## Credits
This interactive [R Shiny](https://shiny.rstudio.com/) app would not be possible without the help from our team of research assistants [Tobias Wondwossen](https://github.com/Toby-exe), [Khanh Le](https://github.com/kle6951), [Bryce Carson](https://github.com/bryce-carson)

## References

## Feedback
The app is maintained by Dr. Ashok Krishnamurthy.

Contact: [Ashok Krishnamurthy, Ph.D.](mailto:akrishnamurthy@mtroyal.ca)  
Website: <https://bit.ly/2YKrXjX>  

This interactive R Shiny app is maintained by Ashok Krishnamurthy. We welcome questions, insights, and feedback.
