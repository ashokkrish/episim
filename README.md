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
After following the instructions in [Running Episim locally ***or*** contributing to its development?](#running-episim-locally-or-contributing-to-its-development), if you're using the Posit [RStudio IDE](https://posit.co/downloads/), open any of the top-level R files and click the <kbd>Run App</kbd> button.

If you're not using RStudio, we trust that you're familiar with R programming and can determine how to run a Shiny application yourself (hint: `shiny::runApp()` is what you want if you're running R from the command line, or VS Code; Emacs with ESS will open the application automatically when you source global.R).
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
- Michael Myer
<!-- FIXME TODO: list all other past members. -->

## Feedback
Development of the application is supervised by Dr. Ashok Krishnamurthy. We welcome questions, insights, and feedback.

Contact: [Ashok Krishnamurthy, Ph.D.](mailto:akrishnamurthy@mtroyal.ca)
Website: <https://bit.ly/2YKrXjX>

# Running Episim locally ***or*** contributing to its development?
## Install runtime dependencies
The dependencies of the application are documented here. Hopefully they are not out of date!

### Bootstrapping project dependencies
***renv* is used to manage application dependencies in a reproducible manner.**

1. First, ensure you have the necessary runtime dependencies installed by running `install.packages("renv")`.
2. Second, run `renv::restore()` to activate renv *in your copy of the project*. You'll need to restart the R session manually if you're not using RStudio; you might need to run `renv::restore()` a second time.
3. When `renv::restore()` prompts you, you should respond with `1` to indicate you wish to continue to activate the project, then install all the packages managed with the lockfile by responding `Y` to the next question *renv* asks.

### Headings are now prosaic, so read on!
### Virtually all runtime dependencies are loaded with `library`,
Explanations for why a dependency is used is given in [the Writ of Dependencies](#writ-of-dependencies).

```R
library(tidyverse)
library(here)
library(deSolve)
library(shiny)
library(bslib)
library(bsicons)
library(shinyhelper)
library(shinyjs)
library(shinyvalidate)
library(shinyWidgets)
library(shinycssloaders)
library(colourpicker)
library(rlang)
library(R.utils)
library(readxl)
library(writexl)
library(plotly)
library(DT)
library(reactlog)
library(shinyFeedback)
library(reshape2)
library(magrittr)
library(reactR)
```

### but some dependencies loaded with `box::use`:
***box* is used to load an R package installed as a Git submodule in R/, rather than loading it through conventional means after installing from GitHub with `devtools::install_github(c("ashokkrish/ehpi", "ashokkrish/react-chart-editoR"))`.**

- *reactcharteditor* is a wrapper package for *plotly/react-chart-editor*, using *reactR* to assist in wrapping the package into a Shiny UI component.

<!-- Ashok TODO: this decision was made to accommodate the workflow familiar to you wherein all application dependencies *not on CRAN* are available *in source form* within the R/ folder. This *does not* guarantee that the remote development HEAD repository is still available; this means that Toby-exe/react-chart-editoR could potentially be deleted in the future. You *must* fork this repository before we can change the remote repository that the Git submodule points to `ashokkrish/react-chart-editoR`. -->
<!-- Ashok TODO: ehpi::epi is great! With box we'd be *forced* to write ehpi$epi, so say it twice just like the epi epi mantra: let's use another Git submodule and not have R/epi.R, for consistency! (You need to fork this repository as well.) -->

## Want to contribute?
### Communicate with active team members
Ping somebody (e.g. `@ashokkrish`) in an open issue to ask about an issue. Active team members will respond on GitHub to notifications.

### Ask to be assigned to an issue if it is tagged with `help-wanted`
If an issue is labelled `help-wanted` then the current team is struggling with this issue and outside help is either *warranted* by the complexity of the issue, or merely *appreciated* because we don't have time to prioritize resolving it.

This item is intimately involved with the first, *communication is really important!*

### Feel free!
We won't reject spontaneous pull requests from unknown contributors without considering them first. Episim is an open-source simulation framework in-development. We love contributors! :heart: However, our development workflow is closer to the Cathedral over at SQLite, but we are currently a distributed, bazaar team. Thankfully Git is distributed, even though our workflow isn't!

# Writ of Dependencies
- tidyverse :: the eponymous tidy data wrangling and analysis metapackage; "why *wouldn't* we use it?" is a better question.
- here :: we use this to [write reproducible file system paths](https://here.r-lib.org/), and so should you!
- deSolve :: a core dependency of R/epi.R, this package is used for d
- shiny :: Episim wouldn't be a Shiny application without it!
- bslib :: our user interface uses the Bootstrap 5 "theme" and several of its UI components; it makes the application look better!
- bsicons :: this provides some fancy SVG/font icons which allow us to have emojis, essentially; it makes the applciation look better!
- shinyhelper :: this allows us to easily include modal dialogs which provide users with `helpful` information about a UI component.
- shinyjs :: it makes working with JavaScript in Shiny much easier, and adds many features!
- shinyvalidate :: lets us ensure users enter form data correctly before we use it.
- shinyWidgets :: provides us with shinier widgets!
- rlang :: this is used for some metaprogramming that permits R/epi.R to implement six... or eight? compartmental epidemic models in under one-hundred lines of R.
- R.utils :: an indispensible package which provides utility functions common in R application development.
- readxl :: for converting XLS and XLSX (Microsoft Excel) spreadsheet file formats to dataframes.
- writexl :: for writing dataframes to the aforementioned formats.
- plotly :: for rendering interactive plots converted from ggplot objects (generated by functions from the ggplot2 package which the tidyverse metapackage loaded for us).
- DT :: for rendering really nice, interactive tables of values.
- shinyFeedback :: for beautiful instant input form validation feedback.
- reshape2 :: 
- magrittr :: to provide the famous `%>%` pipe operator, potentially enable R 3.x users to run Episim (allowing use to avoid `|>` only available in R 4.x), and also the handy `%<>%` pipe operator, which simplifies some longer code.
- reactR :: enables the use of the package *reactcharteditor*, included as a Git submodule under R/, which is a react-based Plotly chart editor created and maintained by Plotly; quite a useful feature for epidemiologists creating plots for publication with Episim!
- shinycssloaders :: no explanation given <!-- hey... we dont' use this! FIXME TODO -->
- colourpicker :: no explanation given <!-- hey... we dont' use this! FIXME TODO -->
- reactlog :: for development purposes, actually! <!-- FIXME TODO: remove this before the next stable release. Only *experienced Shiny developers need it!* -->