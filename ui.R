
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ini)
library(plotly)
param <- read.ini('setup.ini')
param$default_num <- lapply(param$default_num, as.numeric)
param$default_str$dates <- strsplit(param$default_str$dates, ':')[[1]]
param$default_str$dates_minmax <- strsplit(param$default_str$dates_minmax, ':')[[1]]

shinyUI(fluidPage(
  titlePanel("Banner network map"),
  fluidRow(wellPanel(
    fluidRow(
      # get
      column(2, textInput("text", label = h5("Exact subbrands list (e.g. kia rio)"), value = param$default_str$text)), 
      column(2, dateRangeInput("dates", label = h5("Date range"), min = param$default_str$dates_minmax[1], max = param$default_str$dates_minmax[2], 
                               start = param$default_str$dates[1], end = param$default_str$dates[2])),
             
      # filter
      column(2, textInput("category", label = h5("Site category (e.g. auto, news)"), value = param$default_str$category)),
      column(1, numericInput("top_net", label = h5("Top networks"), value = param$default_num$top_net)),
      column(1, numericInput("top_creative", label = h5("Top creatives"), value = param$default_num$top_creative)),
      column(1, numericInput("clean", label = h5("Min days per row"), value = param$default_num$clean)),
      # plot
      column(1, radioButtons("type", label = h5("Type to plot"), choices = list("All" = 1, "Network" = 2))),
      column(1, checkboxInput("network_first", label = "Network first", value = param$default_num$network_first == 1), 
             actionButton("go", "Draw the map (Enter)", icon = icon('picture-o')))
      
    ))),
  
  fluidRow(
    
    column(width = 4, div(style = paste0("height:", param$format$height_1row, "px"), verbatimTextOutput("click"))),
    column(width = 8, div(style = paste0("height:", param$format$height_1row, "px"), imageOutput("myImage")),
           tags$head(tags$script(src="enter_button.js")))
    
  ),
  fluidRow(
    column(width = 12, plotlyOutput("map", height = paste0(param$format$height_2row, "px")))
  )
))
