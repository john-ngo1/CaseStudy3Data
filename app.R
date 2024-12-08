library(ggplot2)
library(tidyverse)
library(dplyr)
df <- read.csv("gym_members_exercise_tracking_synthetic_data.csv")
df <- mutate(
  df,
  across(
    where(is.character),
    ~ if_else(.x == "", NA_character_, .x)
  )
)

df <- na.omit(df)

ui <- fluidPage(
  
  titlePanel("Exercise Data Regression Predictions"),
  
    sidebarPanel(
      varSelectInput(inputId = "ExpVar", "Explanatory Variable",
                     data = df, selected = "BMI"),
      
      varSelectInput(inputId = "PreVar", "Predictor Variable",
                     data = df, selected = "Age"),
    
      numericInput("Pred", "Predict Chosen Value", value = 10),
      
      p("The dataset used in the application is one called \"Gym Members
        Exercise Tracking Synthetic Data\". It includes information on
        gym members, such as age, weight, height, BPM, BMI, type of workout,
        and other useful information. I wanted to create an app using this data
        where the user can select a predictor variable and an explanatory
        variable and see how different variables affect each other."),
        
      p("The application uses linear regression, which is a statistcal test 
        used to analyze the relationship between two variables and predict
        numerical variables based on the relationship. Linear regression works
        by plotting both variables and finding a line of best fit through the data
        using minimizing residuals. The linear regression model outputs coefficients 
        for the predictor variable and an intercept, which are used in form 
        y=mx+b, or slope intercept form, to predict unknown values."),
      
      p("In the application, the user can choose their desired explanatory
        and predictor variables, as well as choose their desired value of the
        explanatory variable to predict the predictor variable. The application
        outputs the prediction of the explanatory variable based on the user\'s 
        inputs, as well as a scatterplot of the two variables, including the line 
        of best fit in red, and the point of interest that the user predicted in 
        green. Also, the summary of the linear regression model call is also
        outputted at the bottom of the application.")),
  
  sidebarPanel(p("Prediction of Explanatory Variable"),
  
  verbatimTextOutput("prediction")),
  
  mainPanel(
    plotOutput("regressPlot"),
    verbatimTextOutput("summary")
  )
)

server <- function(input, output, session) {
  
  PredictorVar <- reactive(input$PreVar)
  
  lm_fit <- reactive({
    lm(as.formula(paste(input$PreVar, '~', input$ExpVar)), data=df)
  })
  
  summary_stats <- reactive({
    summary(lm_fit())
  })
  
  predicted <- reactive({
    model <- lm(as.formula(paste(input$PreVar, '~', input$ExpVar)), data=df)
    return(input$Pred * model$coefficients[2] + model$coefficients[1])
  })
  output$prediction <- renderText(predicted())
  
  output$summary <- renderPrint({
    summary_stats()
  })
  
  output$regressPlot <- renderPlot({
    ggplot(df, aes(!!input$ExpVar,!!input$PreVar)) + 
      geom_point(color = "blue") +
      geom_smooth(mehod = "lm", se = FALSE, color = "red") + 
      geom_point(aes(x=!!input$Pred, y = predicted()), size = 3, color = "green") +
      theme_minimal()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)