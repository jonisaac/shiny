<<<<<<< HEAD
library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(readtext)
library(plotly)
library(shinythemes)
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"), 
                
                
                
                # Application title
                titlePanel("Live Data Analysis"),
                
                # Sidebar with a slider input for number of bins 
                flowLayout(
                  
                  #sidebarPanel(
                  # Input: Select a file ----
                  fileInput("file1", "Choose Data File",
                            multiple = TRUE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                  # Input: Select a file ----
                  fileInput("file2", "Choose Search File",
                            multiple = TRUE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                  # Horizontal line ----
                  tags$hr(),
                  
                  # Input: Checkbox if file has header ----
                  checkboxInput("header", "Header", TRUE),
                  
                  # Input: Select separator ----
                  radioButtons("sep", "Separator",
                               choices = c(Tilda = "~", 
                                           Comma = ",",
                                           Semicolon = ";",
                                           Tab = "\t"),
                               selected = "~")
                  #),
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    tabPanel('Plots', 
                             
                             
                             
                             # Output: Data file ----
                             plotlyOutput("stacked"),
                             dataTableOutput("Outliers")
                    )
                    
                  )
                  
                  
                )
=======
library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(readtext)
library(plotly)

fluidPage(
  
  
  
  # Application title
  titlePanel("Live Data Analysis"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(
      # Input: Select a file ----
      fileInput("file1", "Choose Data File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      # Input: Select a file ----
      fileInput("file2", "Choose Search File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Tilda = "~", 
                               Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = "~")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabPanel('Plots', 
               
               
               
               # Output: Data file ----
               plotlyOutput("stacked"),
               tableOutput("Outliers")
      )
      
    )
    
    
  )
>>>>>>> 3890ad2363f5ea55878f0f1a50bdf59562d6c50d
)