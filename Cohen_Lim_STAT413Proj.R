library(tidyverse)
library(shiny)

library(MASS)

ui <- fluidPage(
  titlePanel("Boston data"),
  textOutput("text"),
  verbatimTextOutput("code"),
  checkboxGroupInput("variable", "Which variable summaries?", 
                     choices = c("Per Capita Crime" = "crim",
                                 "Proportion of Residential > 25,000 sq. ft." = "zn",
                                 "Proportion of Non-Retail Business Acres" = "indus",
                                 "Bounds Charles River (Dummy)" = "chas",
                                 "Nitrogen Oxides Concentration (pp 10 million)" = "nox",
                                 "Avg rooms per dwelling" = "rm",
                                 "Proportion of owner-occupied units built prior to 1940" = "age",
                                 "Weighted mean of distances to five Boston Employment Centres" = "dis",
                                 "Index of Accessibility to Radial Highways" = "rad",
                                 "Full Value Property Tax Rate per $10,000" = "tax",
                                 "Pupil-Teacher Ratio" = "ptratio",
                                 "1000(Bk - 0.63)^2 where Bk is the Proportion of Blacks by Town" = "black",
                                 "Lower Status of the Population (percent)" = "lstat",
                                 "Median Value of Owner-Occupied Homes in $1000s" = "medv"))
)

server <- function(input, output, session) {
  output$text <- renderText({
    "Boston Summaries"
  })
  
  output$code <- renderPrint({
    summary(Boston[input$variable])
  })
}

shinyApp(ui = ui, server = server)

#########
library(tidyverse)
library(ggplot2)
library(MASS)
attach(Boston)

ui <- fluidPage(
  h2("Final Group Project: Housing Values in Suburbs of Boston"),
  hr(),
  h4("by William Cohen and Ana Hyo Young Lim"),
  h6("STAT-413-004", style = "color:blue"),
  h6("Dr. James Dickens"),
  titlePanel("Data Summaries"),
  textOutput("text"),
  verbatimTextOutput("code"),
  checkboxGroupInput("variable", "Which variable summaries?", 
                     choices = c("Per Capita Crime" = "crim",
                                 "Proportion of Residential > 25,000 sq. ft." = "zn",
                                 "Proportion of Non-Retail Business Acres" = "indus",
                                 "Bounds Charles River (Dummy)" = "chas",
                                 "Nitrogen Oxides Concentration (pp 10 million)" = "nox",
                                 "Avg rooms per dwelling" = "rm",
                                 "Proportion of owner-occupied units built prior to 1940" = "age",
                                 "Weighted mean of distances to five Boston Employment Centres" = "dis",
                                 "Index of Accessibility to Radial Highways" = "rad",
                                 "Full Value Property Tax Rate per $10,000" = "tax",
                                 "Pupil-Teacher Ratio" = "ptratio",
                                 "1000(Bk - 0.63)^2 where Bk is the Proportion of Blacks by Town" = "black",
                                 "Lower Status of the Population (percent)" = "lstat",
                                 "Median Value of Owner-Occupied Homes in $1000s" = "medv")),
  titlePanel("Scatterplot"),
  selectInput("var1", "Variable 1", choices = c("Per Capita Crime" = "crim",
                                                "Proportion of Residential > 25,000 sq. ft." = "zn",
                                                "Proportion of Non-Retail Business Acres" = "indus",
                                                "Bounds Charles River (Dummy)" = "chas",
                                                "Nitrogen Oxides Concentration (pp 10 million)" = "nox",
                                                "Avg rooms per dwelling" = "rm",
                                                "Proportion of owner-occupied units built prior to 1940" = "age",
                                                "Weighted mean of distances to five Boston Employment Centres" = "dis",
                                                "Index of Accessibility to Radial Highways" = "rad",
                                                "Full Value Property Tax Rate per $10,000" = "tax",
                                                "Pupil-Teacher Ratio" = "ptratio",
                                                "1000(Bk - 0.63)^2 where Bk is the Proportion of Blacks by Town" = "black",
                                                "Lower Status of the Population (percent)" = "lstat",
                                                "Median Value of Owner-Occupied Homes in $1000s" = "medv")),
  selectInput("var2", "Variable 2", choices = c("Per Capita Crime" = "crim",
                                                "Proportion of Residential > 25,000 sq. ft." = "zn",
                                                "Proportion of Non-Retail Business Acres" = "indus",
                                                "Bounds Charles River (Dummy)" = "chas",
                                                "Nitrogen Oxides Concentration (pp 10 million)" = "nox",
                                                "Avg rooms per dwelling" = "rm",
                                                "Proportion of owner-occupied units built prior to 1940" = "age",
                                                "Weighted mean of distances to five Boston Employment Centres" = "dis",
                                                "Index of Accessibility to Radial Highways" = "rad",
                                                "Full Value Property Tax Rate per $10,000" = "tax",
                                                "Pupil-Teacher Ratio" = "ptratio",
                                                "1000(Bk - 0.63)^2 where Bk is the Proportion of Blacks by Town" = "black",
                                                "Lower Status of the Population (percent)" = "lstat",
                                                "Median Value of Owner-Occupied Homes in $1000s" = "medv")),
  plotOutput("plot"),
  titlePanel("Boston Histograms"),
  selectInput("var3", "Boston variables", choices = c("Per Capita Crime" = "crim",
                                                      "Proportion of Residential > 25,000 sq. ft." = "zn",
                                                      "Proportion of Non-Retail Business Acres" = "indus",
                                                      "Bounds Charles River (Dummy)" = "chas",
                                                      "Nitrogen Oxides Concentration (pp 10 million)" = "nox",
                                                      "Avg rooms per dwelling" = "rm",
                                                      "Proportion of owner-occupied units built prior to 1940" = "age",
                                                      "Weighted mean of distances to five Boston Employment Centres" = "dis",
                                                      "Index of Accessibility to Radial Highways" = "rad",
                                                      "Full Value Property Tax Rate per $10,000" = "tax",
                                                      "Pupil-Teacher Ratio" = "ptratio",
                                                      "1000(Bk - 0.63)^2 where Bk is the Proportion of Blacks by Town" = "black",
                                                      "Lower Status of the Population (percent)" = "lstat",
                                                      "Median Value of Owner-Occupied Homes in $1000s" = "medv")),
  plotOutput("plot1"),
)


server <- function(input, output, session) {
  output$text <- renderText({
    "Boston summaries"
  })
  
  output$code <- renderPrint({
    summary(Boston[input$variable])
  })
  
  output$plot <- renderPlot({
    ggplot(Boston, aes(x = .data[[input$var1]], y = .data[[input$var2]])) +
      geom_point(color = "blue")
  })
  
  output$plot1 <- renderPlot({
    ggplot(Boston, aes(x = .data[[input$var3]])) +
      geom_histogram(fill = "red")
  })
}

shinyApp(ui = ui, server = server)

