library(shiny)
library(dplyr)
library(DT)
library(plotly)
library(summarytools)

fluidPage(
  titlePanel("Shiny App with Dropdown and Table"),
  sidebarLayout(
    sidebarPanel(
      
      selectInput("selected_country", "Select Country", choices = NULL),
      selectInput("selected_gender", "Select Gender", choices = NULL),
      
      h4("Sentiment statistics"),
      DTOutput("statsSentimentTable"),
      
      h4("Retweet statistics"),
      DTOutput("statsRetweetsTable"),
      
      h4("ANOVA analysis - Gender vs X variable"),
      selectInput("selected_variable", "Select Variable", 
                  choices = c("RetweetCount", "Sentiment")),
      verbatimTextOutput("anovaResults")
      
      
    ),
    mainPanel(
      
      plotlyOutput("barChart"),
      plotOutput("histSentiment"),
      plotOutput("histRetweets"),
      
      h3("Sample of the tweets:"),
      DTOutput("tweetsTable")
    )
  )
)

