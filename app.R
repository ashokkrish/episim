# NOTE: don't install packages within an application, and don't comment code
# related to package installation; such lines are only relevant to interactive,
# exploratory data analysis scripts shared directly between users. Packages and
# Shiny applications should never contain such code. Package dependencies should
# be satisfied using package metadata.
#
# FIXME: this needs to be evaluated a second time, otherwise shinyjs::useShinyjs
# is not found. TODO: consult Shiny documentation concerning the initialization
# of applications and the treatment of app.R within RStudio specifically (this
# problem is not apparent in ESS).
packagesloaded <-
  c("deSolve",
    "shiny",
    "shinyhelper",
    "shinyjs",
    "shinyvalidate",
    "shinyWidgets",
    "tidyverse") |>
  lapply(FUN = require, character.only = TRUE) |>
  as.logical() |>
  all()

if (!packagesloaded) exit()

## rprojroot may not be appropriate if it has any dependency on RStudio; we
## might not use anything RStudio/posit related in production.
##
## NOTE DONT: This package is intended for interactive use only. Use
## ‘rprojroot::has_file()’ or the other functions in the ‘rprojroot’ package for
## more control, or for package development.
shinyAppDir(here::here()) # and hurrah!
