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
library(xlsx)
library(R.utils)

## NEXT: switch to using the rpojroot package when preparing this Shiny
## application for deployment, because the here package is a lightweight wrapper
## around that. There may be shortcomings to the switch, or there may be cons to
## not switching. Close examination of the final production environment being
## prepared by T.W. will determine what we finally use. For now, during
## interactive development of the application, here is a perfect solution.
here::i_am("global.R")
defaultInputValues <- read.xlsx(here("data/defaultInputValues.xlsx"),
                                sheetIndex = 1) |>
  mutate(stochastic = as.logical(stochastic),
         vitalStatistics = as.logical(vitalStatistics),
         massAction = as.logical(vitalStatistics))
shinyAppDir(here::here()) # and hurrah!
