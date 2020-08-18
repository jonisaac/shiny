library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(readtext)
library(plotly)
library(shinythemes)
# Define UI for application that draws a histogram
fluidPage(theme = shinytheme("superhero"), 
                
                
                
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
                    radioButtons("file", "Select Search File",
                                 choices = c(Home = 1, 
                                             College = 2,
                                             Auto = 3,
                                             new = 4),
                                 selected = 1),
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
                             dataTableOutput("Outliers")
                    )
                    
                  )
                  
                  
                )
)
