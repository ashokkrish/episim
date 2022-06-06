library(shiny)
library(shinyjs)
library(shinyhelper)
library(shinyWidgets)
library(deSolve)
library(ggplot2)
library(tidyverse)

shinyApp(
     ui = source('ui.r',local=TRUE),
     server =source('server.r',session)
)