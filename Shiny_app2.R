library(shiny)
library(shinydashboard)
library(randomfields)
library(ggplot2)
reactlog::reactlog_enable()

source('Distributions.R')
source('Randomly_generated_fields.R')
source('Moving_average.R')
source('UI.R')
source('SERVER.R')


shinyApp(ui, server)

