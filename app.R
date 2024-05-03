# NOTE: Dependency management should be handled outside of the application
# (generally with the packrat package); only using packages should be handled
# inside an application.
if (!(c("deSolve",
        "shiny",
        "shinyhelper",
        "shinyjs",
        "shinyvalidate",
        "shinyWidgets",
        "tidyverse") |>
      lapply(FUN = require, character.only = TRUE) |>
      as.logical() |>
      all()))
  exit()

# TODO: Assess whether it actually simplifies things to use "here", or if it
# introduces a point of breakage.
library(here)
shinyAppDir(here::here()) # and hurrah!
