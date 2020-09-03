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
          titlePanel("Live Data Analysis"),
          
          # Sidebar with a slider input for number of bins 
          fluidPage(
            
            sidebarPanel(
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