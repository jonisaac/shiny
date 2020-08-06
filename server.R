function(input, output) {
  
  
  data_tables <- reactive({
    req(input$file1)
    req(input$file2)
    library(dplyr)
    library(ggplot2)
    library(stringr)
    library(readtext)
    
    df <- read.table(input$file1$datapath,
                     header = input$header,
                     sep = input$sep, fill = TRUE,
                     row.names = NULL, stringsAsFactors = F,
                     comment.char = "",
                     colClasses = c(rep("character", 32)))
    
    
    symitar <- df[df$account.number != "", ]
    
    class(symitar$X530_03) <- "integer"
    symitar$X530_03 <- .01*symitar$X530_03
    
    
    
    payments <- symitar[, c(1, 4, 31)]
    colnames(payments) <- c("acc_num", "payment", 'description')
    
    
    accounts <- unique(symitar$account.number)
    
    search <- read.csv(input$file2$datapath,
                       colClasses = "character",
                       header = FALSE)
    
    string_match <- function(x){
      return(str_count(string = account_df$description, pattern = regex(as.character(x), ignore_case = T)))
    }
    
    string_payment <- function(x){
      y = str_detect(string = account_df$description, pattern = regex(as.character(x), ignore_case = T))
      return(as.matrix(y))
    }
    
    string_sum <- function(x){
      y = str_detect(string = account_df$description, pattern = regex(as.character(x), ignore_case = T))
      return(colSums(as.matrix(account_df[y ,2])))
    }
    
    account_mat <- matrix(nrow = length(accounts), ncol = nrow(search))
    account_mat_amount <- matrix(nrow = length(accounts), ncol = nrow(search))
    
    #for(j in 1:length(accounts)){
    #    account_df <- payments[payments$acc_num == accounts[j], ]
    #    account_mat[j, ] <- colSums(as.matrix(sapply(search[ ,1],FUN = string_match)))
    #    account_mat_amount[j, ] <- sapply(search[, 1], FUN = string_sum)
    #}
    
    rownames(account_mat) <- accounts
    rownames(account_mat_amount) <- accounts
    colnames(account_mat) <- search[ ,1]
    colnames(account_mat_amount) <- search[ ,1]
    
    search_results <- c()
    search_amounts <- c()
    number_of_ind <- c()
    for (i in 1:nrow(search)) {
      search_results <- c(search_results,sum(str_detect(payments$description, (regex(as.character(search[i,1]), ignore_case = T))), na.rm = T))
      search_amounts <- c(search_amounts, sum(payments$payment[str_detect(payments$description, (regex(as.character(search[i,1]), ignore_case = T)))], na.rm = T))
      number_of_ind <- c(number_of_ind, length(unique(payments$acc_num[str_detect(payments$description, (regex(as.character(search[i,1]), ignore_case = T)))])))
    }
    
    #for (i in 1:nrow(search)) {
    #search_results[i] <- #search_results[i] + #sum(str_detect(payments$X510_04, #(regex(as.character(search[i,1]), ignore_case = #T))))
    #}
    
    #search_results
    search_df <- data.frame(
      term = search[ ,1],
      found = search_results, 
      amount = search_amounts,
      indiv = number_of_ind
    )
    
    account_mat <- matrix(nrow = length(accounts), ncol = nrow(search))
    account_mat_amount <- matrix(nrow = length(accounts), ncol = nrow(search))
    
    for(j in 1:length(accounts)){
      account_df <- payments[payments$acc_num == accounts[j], ]
      account_mat[j, ] <- colSums(as.matrix(sapply(search[ ,1],FUN = string_match)))
      account_mat_amount[j, ] <- sapply(search[, 1], FUN = string_sum)
    }
    
    rownames(account_mat) <- accounts
    rownames(account_mat_amount) <- accounts
    colnames(account_mat) <- search[ ,1]
    colnames(account_mat_amount) <- search[ ,1]
    
    sig_account_hi <- account_mat[rowSums(account_mat)>0, colSums(account_mat)>0]
    #head(sig_account_hi)
    sig_account_amount_hi <- account_mat_amount[rowSums(account_mat)>0, colSums(account_mat)>0]
    #head(sig_account_hi)
    #  account_mat_cols <- cbind(accounts, account_mat)
    #  account_mat_cols_rows <- rbind(c(0 ,search), account_mat_cols)
    #  account_mat_amount_cols <- cbind(accounts, account_mat_amount)
    #  account_mat_amount_cols_rows <- rbind(c(0 ,search), account_mat_amount_cols)
    
    #data_tables <- list(search_df = search_df, 
    #                    account_df = sig_account_hi)
    
    return(search_df)
    
  })
  
  account_mat <- reactive({
    req(input$file1)
    req(input$file2)
    library(dplyr)
    library(ggplot2)
    library(stringr)
    library(readtext)
    
    df <- read.table(input$file1$datapath,
                     header = input$header,
                     sep = input$sep, fill = TRUE,
                     row.names = NULL, stringsAsFactors = F,
                     comment.char = "",
                     colClasses = c(rep("character", 32)))
    
    
    symitar <- df[df$account.number != "", ]
    
    class(symitar$X530_03) <- "integer"
    symitar$X530_03 <- .01*symitar$X530_03
    
    
    
    payments <- symitar[, c(1, 4, 31)]
    colnames(payments) <- c("acc_num", "payment", 'description')
    
    
    accounts <- unique(symitar$account.number)
    
    search <- read.csv(input$file2$datapath,
                       colClasses = "character",
                       header = FALSE)
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
    
    #    account_mat <- matrix(nrow = length(accounts), ncol = nrow(search))
    #    account_mat_amount <- matrix(nrow = length(accounts), ncol = nrow(search))
    #    
    #   # for(j in 1:length(accounts)){
    #        account_df <- payments[payments$acc_num == accounts[j], ]
    #        account_mat[j, ] <- colSums(as.matrix(sapply(search[ ,1],FUN = string_match)))
    #        account_mat_amount[j, ] <- sapply(search[, 1], FUN = string_sum)
    #    }
    
    #rownames(account_mat) <- accounts
    #rownames(account_mat_amount) <- accounts
    #colnames(account_mat) <- search[ ,1]
    #colnames(account_mat_amount) <- search[ ,1]
    
    #search_results <- c()
    #search_amounts <- c()
    #number_of_ind <- c()
    #for (i in 1:nrow(search)) {
    #    search_results <- c(search_results,sum(str_detect(payments$description, (regex(as.character(search[i,1]), ignore_case = T))), na.rm = T))
    #    search_amounts <- c(search_amounts, sum(payments$payment[str_detect(payments$description, (regex(as.character(search[i,1]), ignore_case = T)))], na.rm = T))
    #    number_of_ind <- c(number_of_ind, length(unique(payments$acc_num[str_detect(payments$description, (regex(as.character(search[i,1]), ignore_case = T)))])))
    #}
    
    #for (i in 1:nrow(search)) {
    #search_results[i] <- #search_results[i] + #sum(str_detect(payments$X510_04, #(regex(as.character(search[i,1]), ignore_case = #T))))
    #}
    
    #search_results
    #search_df <- data.frame(
    #    term = search[ ,1],
    #    found = search_results, 
    #    amount = search_amounts,
    #    indiv = number_of_ind
    #)
    
    
    
    account_mat <- matrix(nrow = length(accounts), ncol = nrow(search))
    account_mat_amount <- matrix(nrow = length(accounts), ncol = nrow(search))
    
    for(j in 1:length(accounts)){
      account_df <- payments[payments$acc_num == accounts[j], ]
      account_mat[j, ] <- colSums(as.matrix(sapply(search[ ,1],FUN = string_match)))
      account_mat_amount[j, ] <- sapply(search[, 1], FUN = string_sum)
    }
    
    rownames(account_mat) <- accounts
    rownames(account_mat_amount) <- accounts
    colnames(account_mat) <- search[ ,1]
    colnames(account_mat_amount) <- search[ ,1]
    
    sig_account_hi <- account_mat[rowSums(account_mat)>0, colSums(account_mat)>0]
    head(sig_account_hi)
    sig_account_amount_hi <- account_mat_amount[rowSums(account_mat)>0, colSums(account_mat)>0]
    head(sig_account_hi)
    
    #account_mat_cols <- cbind(accounts, col_account_mat)
    #account_mat_cols_rows <- rbind(c(0 ,search), account_mat_cols)
    #account_mat_amount_cols <- cbind(accounts, col_account_mat_amount)
    #account_mat_amount_cols_rows <- rbind(c(0 ,search), account_mat_amount_cols)
    
    return(sig_account_hi)
    
  })
  
  
  output$Occurances <- renderPlot({
    
    req(input$file1)
    req(input$file2)
    
    df <- data_tables()
    
    return(ggplot(df[df$amount > 0, ]) +
             geom_col(aes(x = term, y = found)) +
             theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
    
  })
  
  
  output$Amount <- renderPlot({
    
    req(input$file1)
    req(input$file2)
    
    df <- data_tables() 
    
    return(ggplot(df[df$amount > 0, ]) +
             geom_col(aes(x = term, y = amount)) +
             theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
  })
  
  
  output$Outliers <- renderDataTable({
    
    req(input$file1)
    req(input$file2)
    
    df <- account_mat()
    
    return(df)
    
  })
  
}