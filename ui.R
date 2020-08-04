
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
               plotOutput("Occurances"),
               plotOutput("Amount"),
               tableOutput("Outliers")
      )
      
    )
    
    
  )
)