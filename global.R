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
library(rlang)
library(R.utils)
library(readxl)

here::i_am("global.R")

## DONE: do not modify the column types given below; they are current as of
## 2024-05-23. They should only change if the ordering of columns changes.
columnTypes <- c("text", # Model name
                 ## FIXME: See the related fixme in the filter() call in the
                 ## defaults() reactive expresison. Using logicals in any way
                 ## seems to bork filtering for no obvious reason.
                 ## Column types :: numeric -> logical, or logical -> numeric ::
                 ## stochastic, vitalDynamics, trueMassAction
                 ## "logical", "logical", "logical",
                 character(19))
columnTypes[2:20] <- "numeric"
defaultInputValues <- read_xlsx(here("data/defaultInputValues.xlsx"),
                                col_types = columnTypes)

## TODO: implement the necessary UI and server logic to have the following be
## meaningful to the application.
## publishedInputValues <- read_xlsx(here("data/defaultInputValues.xlsx"), 2)

shinyAppDir(here::here()) # and hurrah!
