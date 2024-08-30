

#Matthew's code

library(shiny)
library(tidyverse)
library(caret)
library(kernlab)
library(ggplot2)
library(randomForest)


use_model_to_predict <- function(df,df_solution){
  load(file="prediction.rda")
  
  svmPred <- as.factor(predict(svmModel,df))
  
  df_solution$expensive <- as.factor(as.integer(as.logical(df_solution$expensive)))
  
  confusionMatrix(svmPred, df_solution$expensive)
}


ui <- fluidPage(
    
    titlePanel("Group 1 Final Project"),
    

    fileInput("upload", label = "Input File", accept = c(".csv")),
    
    
    fileInput("Upload_Solution", label = "Solution", accept = c(".csv")),
    
    numericInput("n", "Number of Rows to View", value = 5, min = 1, step = 1),
    
    tableOutput("headForDF"),
    
    plotOutput(outputId = "chart1"),
    
    checkboxGroupInput(inputId = "smokers",
                       label = "Include In Chart Below:",
                       choices = list("Smokers" = "yes",
                                      "Non-Smokers" = "no"),
                       selected = c("yes","no")),
    
    plotOutput(outputId = "chart2"),
    
    
    verbatimTextOutput("txt_results", placeholder = TRUE),

    tableOutput("confmat")
    
)


server <- function(input, output, session) {
  
  getTestData <- reactive({
    req(input$upload)
    data <- read_csv(input$upload$name)
    data <- data %>% 
      rename(
        hyper = hypertension,
        exr = exercise,
        chld = children
      )
    data <- data %>% select(
      age, smoker, hyper, exr, chld, bmi
    )
    data
  })
  
  
  
  
  
  getSolutionData <- reactive({
    req(input$Upload_Solution)
    solution <- read_csv(input$Upload_Solution$name)
    solution
  })
  

  output$headForDF <- renderTable(
    head(getTestData(), input$n)
  )
  

  output$confmat <- renderTable({
    confmat <- use_model_to_predict(getTestData(), getSolutionData())
    confmat$table
    
  }
  
  )  

  
  output$txt_results <- renderText({
    confmat <- use_model_to_predict(getTestData(), getSolutionData())
    paste0("Sensitivity: ",confmat$byClass["Sensitivity"],"
Accuracy: ",confmat$overall["Accuracy"],"
We expected the sensitivity to be around .88, which is what we got when building the model with the training and test data set.")
    
  }

  )
  
  
  output$chart1 <- renderPlot({
    datafile <- read_csv("https://intro-datascience.s3.us-east-2.amazonaws.com/HMO_data.csv")
    datafile <- datafile %>% drop_na()
    
    qval <- quantile(datafile$cost, probs = c(0.75))
    
    smoker_summary <- datafile %>% group_by(smoker) %>%
      summarise(
        count = n(),
        high_cost = sum(ifelse(cost > qval, 1, 0)),
        high_cost_per = high_cost/count
      )
    
    smoker_summary %>% ggplot(aes(x=smoker, y=high_cost_per, fill = smoker)) + geom_bar(stat = "identity") + 
      ylim(0,1) +
      ggtitle("Percentage of Smokers and Non-Smokers Who Are High Cost") +
      ylab("Percentage of High Cost Customers") +
      xlab("Smoker") + theme_minimal()
  })
  
  
  output$chart2 <- renderPlot({
    datafile <- read_csv("https://intro-datascience.s3.us-east-2.amazonaws.com/HMO_data.csv")
    datafile <- datafile %>% drop_na()
    
    datafile <- datafile %>% filter(smoker %in% input$smokers)
    
    datafile %>% ggplot(aes(x = cost)) + 
      geom_density(fill = "orange") +
      theme_minimal() + 
      xlim(0,max(datafile$cost) + 5) +
      ggtitle("Distribution of Cost in Dataset") + 
      ylab("Density") +
      xlab("Cost")
  })
}


shinyApp(ui = ui, server = server)
