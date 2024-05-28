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
library(rlang)
library(R.utils)
library(readxl)
library(writexl)
library(plotly)

here::i_am("global.R")

## DONE: do not modify the column types given below; they are current as of
## 2024-05-23. They should only change if the ordering of columns changes.
columnTypes <- c(
  "text", # Model name
  ## WARN DONT: using logicals in any way, it inexplicably borks everything. A
  ## character vector is used to hold the string "numeric," as modified below.
  character(19)
)
columnTypes[2:20] <- "numeric"
defaultInputValues <- read_xlsx(here("data/defaultInputValues.xlsx"),
                                col_types = columnTypes)

## TODO: implement the necessary UI and server logic to have the following be
## meaningful to the application.
## publishedInputValues <- read_xlsx(here("data/defaultInputValues.xlsx"), 2)

shinyAppDir(here::here()) # and hurrah!
