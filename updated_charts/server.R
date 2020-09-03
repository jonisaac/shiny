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
library(shinyBS)
library(shinyLP)


function(input, output) {
  
  account_mat <- reactive({
    
    shinyLP::runExample()
    
    
    index  <- as.numeric(input$file)
    
    df <- read.table("../search_files/aa5_2.txt",
                     header = TRUE,
                     sep = "~", fill = TRUE,
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
    return((sig_account_amount_hi))
    
  })
  
  
  output$stacked <- renderPlotly({
    
    files <- c(
      "../text_files/home_improv.txt",
      "../text_files/college.txt", 
      "../text_files/auto.txt"
    )
    
    titles <- c(
      "Total Amount Spent on Home Improvement", 
      "Total Amount Spent in College Towns", 
      "Total Amount Spent on Auto Repair"
    )
    
    if(as.numeric(input$file) == 4){
      req(input$file2)
      mat <- account_mat()
    } else {
      mat <- read.table(file = as.character(files[as.numeric(input$file)]), header = TRUE, stringsAsFactors = F, encoding = 'UTF-8')
      mat <- mat[ , order(colSums(mat), decreasing = T)]
      mat <- t(mat)
    }
    
    mat <- mat[1:25 ,]
    df <- data.frame(terms = row.names(mat), mat)
    df$terms <- factor(df$terms, levels = as.character(df$terms))
    #df <- df[c(1, order(rowSums(df[ ,-1]), decreasing = T)), ]
    df <- (as.data.table(df))
    
    
    data.table::melt(df, id.vars='terms') %>%
      plot_ly(x = ~terms, y = ~value, type = 'bar', 
              name = ~variable, color = ~variable) %>%
      layout(yaxis = list(title = 'Amount', hoverformat = "$,f"), 
             barmode = 'stack', showlegend = FALSE,title = titles[as.numeric(input$file)], 
             xaxis = list(title = "Account Number"), margin = .25)
    
  })
  
  output$storesplot <- renderPlotly({
    
    files <- c(
      "../text_files/home_improv.txt",
      "../text_files/college.txt", 
      "../text_files/auto.txt"
    )
    
    titles <- c(
      "Total Amount Spent on Home Improvement", 
      "Total Amount Spent in College Towns", 
      "Total Amount Spent on Auto Repair"
    )
    
    
    
    if(as.numeric(input$file) == 4){
      req(input$file2)
      mat <- account_mat()
    } else {
      mat <- read.table(file = as.character(files[as.numeric(input$file)]), header = TRUE, encoding = 'UTF-8')
      mat <- mat[order(rowSums(mat), decreasing = T) , ]
    }
    
    df <- data.frame(terms = row.names(mat), mat)
    df$terms <- factor(df$terms, levels = as.character(df$terms))
    df <- as.data.table(df)
    
    data.table::melt(df, id.vars='terms') %>%
      
      plot_ly(x = ~terms, y = ~value, type = 'bar', 
              name = ~variable, color = ~variable) %>%
      layout(yaxis = list(title = 'Amount', hoverformat = "$,f"),
             barmode = 'stack', showlegend = FALSE,title = titles[as.numeric(input$file)], 
             xaxis = list(title = "Category"), margin = .25)
    
  })
  
  output$Outliers <- renderDataTable(
   # options = list(rowCallback = I(
    #  'function(row, data) {
    #    $("td", row).css("text-align", "center");
    #  }'
    #)),
    {
      #req(input$file1)
      
      
      files <- c(
        "../text_files/home_improv.txt",
        "../text_files/college.txt", 
        "../text_files/auto.txt"
      )
      
      if(as.numeric(input$file) == 4){
        req(input$file2)
        df <- account_mat()
      } else {
        df <- read.table(file = files[as.numeric(input$file)], header = TRUE, encoding = 'UTF-8')
        df <- df[ , order(colSums(df), decreasing = T)]
      }
      
      df <- as.data.table(t(df), keep.rownames = "Account Number")
      summ <- data.table(
        account_numbers = df$'Account Number', 
        total = rowSums(df[,-1 ]), 
        average = round(rowMeans(df[,-1 ]), digits = 2), 
        multiples_the_average = round(rowSums(df[,-1 ])/mean(rowSums(df[,-1 ]))*2)/2
      )
      
      return(datatable(summ))
    })
  
  
  output$Stores <- renderDataTable(
   # options = list(rowCallback = I(
  #    'function(row, data) {
  #      $("td", row).css("text-align", "center");
  #    }'
  #  )),
    {
      #req(input$file1)
      
      
      files <- c(
        "../text_files/home_improv.txt",
        "../text_files/college.txt", 
        "../text_files/auto.txt"
      )
      
      if(as.numeric(input$file) == 4){
        req(input$file2)
        df <- account_mat()
      } else {
        df <- read.table(file = files[as.numeric(input$file)], header = TRUE, encoding = 'UTF-8')
        df <- df[ , order(colSums(df), decreasing = T)]
      }
      
      df <- as.data.table((df), keep.rownames = "Terms")
      summ <- data.table(
        account_numbers = df$'Terms', 
        total = rowSums(df[,-1 ]), 
        average = round(rowMeans(df[,-1 ]), digits = 2), 
        multiples_the_average = round(rowSums(df[,-1 ])/mean(rowSums(df[,-1 ]))*2)/2
      )
      
      return(datatable(summ))
    })
  
  
  
  
  # output$Outliers <- renderTable(
  #  rownames = TRUE, 
  # bordered = TRUE,
  #  hover = TRUE, 
  # {
  
  
  #  req(input$file1)
  #  req(input$file2)
  
  #  df <- account_mat()
  
  #  return(head(t(df[order(rowSums(df), decreasing = T), order(colSums(df), decreasing = T)]), 25))
  
  #})
  
  
  
  
}