library(shiny)
library(shinyjs)
library(bslib)
library(glmnet)
library(latex2exp)

source('helper.R')


ui <- page_sidebar(
  title = "Penalized estimators",
  sidebar = sidebar(
    # numericInput(
    #   "sd_x1",
    #   "Standard deviation of X1:",
    #   value = 1, 
    #   min = 0.01,
    #   step = 0.1
    # ),
    # numericInput(
    #   "sd_x2",
    #   "Standard deviation of X2:",
    #   value = 1, 
    #   min = 0.01,
    #   step = 0.1
    # ),
    radioButtons(
      "Estimators",
      "Select one of the estimators",
      choices = list(
        "Ridge",
        "LASSO",
        # "ElasticNet",
        "adaptiveLASSO",
        "marginalizedLASSO"
      ),
      selected = "Ridge"
    ),
    sliderInput(
      "lambda",
      "Lambda Index:",
      min = 0,
      max = 10,
      value = 1,
      step = 1,
      animate = TRUE # Add animation to the slider
    )
  ),
  navset_card_tab(
    id = "tabs",
    title = uiOutput("card_header"),
    
    nav_panel(
      title = "Explanations",
      card_body(
        uiOutput("explanation")
      )
    ),
    
    # nav_panel(
    #   title = "Histograms",
    #   card_body_fill(
    #     fluidRow(
    #       column(6, plotOutput("hist_x1")),
    #       column(6, plotOutput("hist_x2"))
    #     )
    #   )
    # ),
    
    nav_panel(
      title = "Plot",
      card_body_fill(
        plotOutput("plot")
      )
    )
  )
)

server <- function(input, output, session) {
  # data_reactive <- reactiveVal(NULL)
  # 
  # observe( 
  # {
  #   sd_x1 <- input$sd_x1
  #   sd_x2 <- input$sd_x2
  #   
  #   new_data <- generate_data(sd_x1, sd_x2)
  #   
  #   data_reactive(new_data)
  #   
  #   cat("Updated data with sd_x1:", sd_x1, " sd_x2:", sd_x2, "\n")
  # }
  # )
  
  # observe({
  #   if (is.null(data_reactive())) {
  #     data_reactive(generate_data(input$sd_x1, input$sd_x2))
  #   }
  # })
  
  output$card_header <- renderUI(
    {
    estimator <- input$Estimators
    # Create header text based on selected estimator
    h4(paste("Selected Estimator:", estimator))
    }
  )
  
  output$explanation <- renderUI({
    explanation <- getExplanation(input$Estimators)
    tagList(
      div(
        h4(explanation$author, style = "text-align: center;"),
        if (exists("photos", where = explanation)) {
          # Display multiple photos for Elastic Net
          fluidRow(
            lapply(explanation$photos, function(photo) {
              column(6, img(src = photo, style = "display: block; margin-left: auto; margin-right: auto; max-height: 100px; width: auto;"))
            })
          )
        } else {
          # Display a single photo for other estimators
          img(src = explanation$photo, style = "display: block; margin-left: auto; margin-right: auto; max-height: 100px; width: auto;")
        },
        style = "text-align: center;"
      ),
      div(
        withMathJax(paste0(explanation$formula)),
        style = "text-align: center; margin-top: 10px;"
      ),
      p("Reference:", explanation$reference)
    )
  })
  
  # output$hist_x1 <- renderPlot({
  #   req(data_reactive())
  #   data <- data_reactive()
  #   hist(data$x1, main = "Histogram of x1", xlab = "x1", col = "lightblue", border = "black")
  # })
  # 
  # output$hist_x2 <- renderPlot({
  #   req(data_reactive())
  #   data <- data_reactive()
  #   hist(data$x2, main = "Histogram of x2", xlab = "x2", col = "lightgreen", border = "black")
  # })
  
  output$plot <- renderPlot({
    plotFunction(value = input$lambda, type = input$Estimators)
  })
}
  
shinyApp(ui = ui, server = server)
