library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(readtext)
library(plotly)
library(shinythemes)
library(DT)
library(data.table)
library(tidyverse)


fluidPage(theme = shinytheme("flatly"), 
          
          # Application title
          #titlePanel("Ebbylitics"
            #div(column(width = 6, h2("My Header")), 
            #    column(width = 6, tags$img(src = "ebbylity_color_logo.png"))),
           # windowTitle="MyPage"
          #),
          
          # Sidebar with a slider input for number of bins 
          fluidPage(
           # tags$img(src = "ebbylity_color_logo.png", height="10%", width="10%", align="left"),
            sidebarPanel(width = 3,
                         tags$img(src = "ebbylity_logo.png", height="80%", width="80%", align="left"),
                         
              # Input: Select a file ----
              #fileInput("file1", "Choose Data File",
              #          multiple = TRUE,
              #          accept = c("text/csv",
              #                     "text/comma-separated-values,text/plain",
              #                     ".csv")),
              # Input: Select file ----
              radioButtons("file", "Choose Analysis",
                           choices = c("Home Improvement Spend" = 1, 
                                       "College Town Spend" = 2,
                                       "Auto Spend" = 3),
                           selected = 1, width = "200px"),
              # Input: Select a file ----
              #fileInput("file2", "Choose Search File",
              #          multiple = TRUE,
              #          accept = c("text/csv",
              #                     "text/comma-separated-values,text/plain",
              #                     ".csv")),
              
              # Horizontal line ----
              #tags$hr(),
              
              # Input: Checkbox if file has header ----
              #checkboxInput("header", "Header", TRUE),
              
              # Input: Select separator ----
              #radioButtons("sep", "Separator",
              #             choices = c(Tilda = "~", 
              #                         Comma = ",",
              #                         Semicolon = ";",
              #                         Tab = "\t"),
              #             selected = "~")
            ),
            
            # Main panel for displaying outputs ----
            mainPanel(
              tabsetPanel(type = "tabs",
                          tabPanel('Plots', 
                                   plotlyOutput("stacked"),
                                   plotlyOutput("storesplot")
                          ), 
                          tabPanel("Spend by Account", 
                                   dataTableOutput("Outliers")
                          ),tabPanel("Spend by Category", 
                                     dataTableOutput("Stores")
                          )
              )
              
              
            )
            
            
          )
)