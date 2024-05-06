# NOTE: don't install packages within an application, and don't comment code
# related to package installation; such lines are only relevant to interactive,
# exploratory data analysis scripts shared directly between users. Packages and
# Shiny applications should never contain such code. Package dependencies should
# be satisfied using package metadata.
packagesloaded <-
  c("tidyverse",
    "here",
    "deSolve",
    "shiny",
    "shinyhelper",
    "shinyjs",
    "shinyvalidate",
    "shinyWidgets") |>
  lapply(FUN = require, character.only = TRUE) |>
  as.logical() |>
  all()

if (!packagesloaded) exit()

## NEXT: switch to using the rpojroot package when preparing this Shiny
## application for deployment, because the here package is a lightweight wrapper
## around that. There may be shortcomings to the switch, or there may be cons to
## not switching. Close examination of the final production environment being
## prepared by T.W. will determine what we finally use. For now, during
## interactive development of the application, here is a perfect solution.
here::i_am("global.R")
shinyAppDir(here::here()) # and hurrah!
