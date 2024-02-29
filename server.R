library(shiny)
library(dplyr)
library(DT)
library(plotly)
library(summarytools)

function(input, output, session) {
  tweets <- read.csv("tweets-engagement-metrics.csv")
  kolumny_do_usuniecia <- c("UserID", "LocationID","StateCode", "TweetID")
  tweets <- tweets[, -which(names(tweets) %in% kolumny_do_usuniecia)]
  
  updateSelectInput(session, "selected_country", choices = unique(tweets$Country))
  updateSelectInput(session, "selected_gender", choices = unique(tweets$Gender))
  
  # Filter data by selected country and gender
  filtered_data <- reactive({
    subset(tweets, Country == input$selected_country & Gender == input$selected_gender) %>%
      arrange(Hour)
  })
  
  # Create bar chart for frequency visualization
  output$barChart <- renderPlotly({
    freq_data <- table(filtered_data()$Hour)  
    
    plot_ly(x = names(freq_data), y = freq_data, type = 'bar') %>%
      layout(title = 'Amount of tweets depending on the time of the day',
             xaxis = list(title = 'Values', categoryorder = 'array', categoryarray = 1),
             yaxis = list(title = 'Frequency'))
  })
  
  # Generate sample of 10 tweets
  output$tweetsTable <- renderDT({
    datatable(
      head(filtered_data(), 10), 
      options = list(dom = 't'),
      rownames = FALSE
    )
  })
  
  output$statsSentimentTable <- renderDT({
    # Get selected column from the data
    selected_column <- filtered_data()$Sentiment
    
    # Calculate selected statistics using dplyr
    stats <- data.frame(
      Statistic = c(
        "Mean", "Median", "Standard deviation", "Minimum", "Maximum", "n"
      ),
      Value = c(
        mean(selected_column),
        median(selected_column),
        round(sd(selected_column), 2),  # Round SD to 2 decimal places
        min(selected_column),
        max(selected_column),
        length(selected_column)
      )
    )
    
    # Display the statistics as a DataTable
    datatable(
      stats, 
      options = list(dom = 't'),
      rownames = FALSE
    )
  })
  
  output$statsRetweetsTable <- renderDT({
    # Get selected column from the data
    selected_column <- filtered_data()$RetweetCount
    
    # Calculate selected statistics using dplyr
    stats <- data.frame(
      Statistic = c(
        "Mean", "Median", "Standard deviation", "Minimum", "Maximum", "n"
      ),
      Value = c(
        mean(selected_column),
        median(selected_column),
        round(sd(selected_column), 2),  # Round SD to 2 decimal places
        min(selected_column),
        max(selected_column),
        length(selected_column)
      )
    )
    
    # Display the statistics as a DataTable
    datatable(
      stats, 
      options = list(dom = 't'),
      rownames = FALSE
    )
  })
  
  output$anovaResults <- renderPrint({
    selected_variable <- input$selected_variable
    
    if (!is.null(selected_variable)) {
      anova_results <- aov(
        as.formula( paste0(selected_variable, " ~ Gender") ), 
        data = tweets
      )
      summary(anova_results)
    } else {
      NULL
    }
  })
  
  # Create histogram for Sentiment
  output$histSentiment <- renderPlot({
    # Get selected column from the data
    selected_column <- filtered_data()$Sentiment
    
    # Create histogram
    hist(selected_column, main = paste("Histogram of Sentiment for", input$selected_country, "and", input$selected_gender),
         xlab = "Sentiment", col = "lightblue", border = "black")
  })
  
  # Create histogram for RetweetCount
  output$histRetweets <- renderPlot({
    # Get selected column from the data
    selected_column <- filtered_data()$RetweetCount
    
    # Create histogram
    hist(selected_column, main = paste("Histogram of Retweets for", input$selected_country, "and", input$selected_gender),
         xlab = "Retweets", col = "lightgreen", border = "black")
  })
}

