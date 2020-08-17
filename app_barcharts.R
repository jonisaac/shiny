#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(readtext)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  

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
)



# Define server logic to read selected file ----
server <- function(input, output) {
    
    account_mat <- reactive({
      req(input$file1)
      req(input$file2)
      library(dplyr)
      library(ggplot2)
      library(stringr)
      library(stringi)
      library(readtext)
      library(data.table)
      
      df <- read.table(input$file1$datapath,
                       header = input$header,
                       sep = input$sep, fill = TRUE,
                       row.names = NULL, stringsAsFactors = F,
                       comment.char = "",
                       colClasses = c(rep("character", 32)))
      
      
      symitar <- df[df$account.number != "", ]
      
      class(symitar$X530_03) <- "integer"
      symitar$X530_03 <- .01*symitar$X530_03
      
      set.seed(12345)
      shuffle <- stri_rand_shuffle(unique(symitar$account.number))
      symitar$account.number <- sample(shuffle, size = length(symitar$account.number), replace = TRUE)
      
      payments <- symitar[, c(1, 4, 31)]
      colnames(payments) <- c("acc_num", "payment", 'description')
      

      
      accounts <- unique(symitar$account.number)
      
      search <- read.csv(input$file2$datapath,
                         colClasses = "character",
                         header = FALSE)
      search <- as.data.frame(search)
      #    
      string_match <- function(x){
        return(str_count(string = account_df$description, pattern = regex(as.character(x), ignore_case = T)))
      }
      #    
      string_payment <- function(x){
        y = str_detect(string = account_df$description, pattern = regex(as.character(x), ignore_case = T))
        return(as.matrix(y))
      }
      
      string_sum <- function(x){
        y = str_detect(string = account_df$description, pattern = regex(as.character(x), ignore_case = T))
        return(colSums(as.matrix(account_df[y ,2])))
      }
      
      
      
      account_mat <- matrix(0, nrow = length(accounts), ncol = nrow(search))
      account_mat_amount <- matrix(0, nrow = length(accounts), ncol = nrow(search))
      
      for(j in 1:length(accounts)){
        account_df <- payments[payments$acc_num == accounts[j], ]
        account_mat[j, ] <- colSums(as.matrix(sapply(search[ ,1],FUN = string_match)))
        account_mat_amount[j, ] <- sapply(search[, 1], FUN = string_sum)
        print(j)
      }
      
      
      rownames(account_mat) <- accounts
      rownames(account_mat_amount) <- accounts
      colnames(account_mat) <- search[ ,1]
      colnames(account_mat_amount) <- search[ ,1]
      
      sig_account_hi <- account_mat[rowSums(account_mat)>0, colSums(account_mat)>0]
      # head(sig_account_hi)
      sig_account_amount_hi <- account_mat_amount[rowSums(account_mat_amount)>0, colSums(account_mat_amount)>0]
      # head(sig_account_hi)
      return(t(sig_account_amount_hi))
      
    })
    
    
    output$stacked <- renderPlotly({
      
      req(input$file1)
      req(input$file2)
      
      mat <- account_mat()
      df <- data.frame(terms = row.names(mat), mat)
      df <- as.data.table(df)
      
      data.table::melt(df, id.vars='terms') %>%
        plot_ly(x = ~terms, y = ~value, type = 'bar', 
                name = ~variable, color = ~variable) %>%
        layout(yaxis = list(title = 'Amount', hoverformat = "$,f"), barmode = 'stack', showlegend = FALSE)
      
    })
    
    
    output$Outliers <- renderTable(
      rownames = TRUE, 
      {
        
        
        req(input$file1)
        req(input$file2)

        df <- account_mat()
        
        return(head(t(df[order(rowSums(df), decreasing = T), order(colSums(df), decreasing = T)]), 25))
        
      })
    
    
 
    
}
# Run the app ----
shinyApp(ui, server)