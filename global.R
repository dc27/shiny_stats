library(dplyr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjqui)
library(purrr)

source("R/generate_pop_data.R")
source("R/ci.R")
# 'reset' seed
set.seed(NULL)

# markdown > details arrow icon
icon_list <- function(x){
  lapply(
    x,
    function(x) {
      tags$div(
        icon("arrows-alt-h"),
        tags$strong(x)
      )
    }
  )
}