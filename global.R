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
library(xlsx) # FIXME: this is slow and gross.
library(R.utils)

## NEXT: switch to using the rpojroot package when preparing this Shiny
## application for deployment, because the here package is a lightweight wrapper
## around that. There may be shortcomings to the switch, or there may be cons to
## not switching. Close examination of the final production environment being
## prepared by T.W. will determine what we finally use. For now, during
## interactive development of the application, here is a perfect solution.
here::i_am("global.R")

## FIXME: switch to a readr or super fast library, not this old, slow stuff.
defaultInputValues <- read.xlsx(here("data/defaultInputValues.xlsx"),
  sheetIndex = 1
)
## FIXME: switch to a readr or super fast library, not this old, slow stuff.
## TODO: implement the necessary UI and server logic to have the following be
## meaningful to the application.
## publishedInputValues <- read.xlsx(here("data/defaultInputValues.xlsx"),
##                                 sheetIndex = 2)

## Load the minified JavaScript to extend the modelSelect widget functionality.
modelSelectJavaScript <- read_file(here("www/modelSelect.min.js"))
shinyAppDir(here::here()) # and hurrah!
