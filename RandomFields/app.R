#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(png)
#install_github("C-Juliette/randomfields")


source('UI.R')
source('SERVER.R')


shinyApp(ui, server)

