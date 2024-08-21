library(shiny)
library(bslib)
library(glmnet)
library(latex2exp)

source('helper.R')


ui <- page_sidebar(
  title = "Penalized estimators",
  sidebar = sidebar(
    radioButtons(
      "Estimators",
      "Select one of the estimators",
      choices = list(
        "Ridge",
        "LASSO",
        "adaptiveLASSO",
        "marginalizedLASSO"
      ),
      selected = "Ridge"
    ),
    sliderInput(
      "lambda",
      "Lambda Index:",
      min = 0,
      max = 20,
      value = 2,
      step = 2,
      animate = TRUE # Add animation to the slider
    )
  ),
  card(
    card_header(uiOutput("card_header")),
    plotOutput("plot")
  )
)

server <- function(input, output) {
  output$card_header <- renderUI(
    {
    estimator <- input$Estimators
    # Create header text based on selected estimator
    h4(paste("Selected Estimator:", estimator))
    }
  )
  
  
  output$plot <- renderPlot({
    plotFunction(value = input$lambda, type = input$Estimators)
  })
}
  


  

 
  # output$ridgePlot <- renderPlot({
  #   
  #   k1 <- seq(1, 5, by = 0.01)
  #   
  #   k1 <- k1[k1 > min(rss_lstsq)]
  #   
  #   r1 <- find_closest(apply(ridge$beta, 2, function(x) sum((z0 - x[1] * z1 - x[2] * z2) ^ 2)), k1[lambdaIndex])
  #   
  #   plot(1.02 * range(s), 1.02 * range(s), type = "n", axes = FALSE, xlab = " ", ylab = " ", main = "(a) Ridge Regression")
  #   axis(side = 1, pos = 0, col = "black", at = seq(-1, 1, length = 5))
  #   axis(side = 2, pos = 0, col = "black", at = seq(-1, 1, length = 5))
  #   text(0.05, 1, TeX("$\\beta_2$"))
  #   text(0.98, 0.08, TeX("$\\beta_1$"))
  #   
  #   contour(s, s, matrix(rss_lstsq, nrow = n_lstsq), levels = k1, add = TRUE, col = "gray", drawlabels = FALSE)
  #   contour(s, s, matrix(rss_lstsq, nrow = n_lstsq), levels = k1[1], add = TRUE, col = "black", drawlabels = FALSE)
  #   draw_circle <- function(r, lwd = 1, col = "red") {
  #     radians <- seq(0, 2 * pi, length = 100)
  #     lines(r * sin(radians), r * cos(radians), col = col, lwd = lwd)
  #   }
  #   draw_circle(sqrt(lstsq_beta[1]^2 + lstsq_beta[2]^2))
  #   points(lstsq_beta[1], lstsq_beta[2], pch = 19, col = "red")
  #   
  #   draw_circle(sqrt(ridge$beta[1, r1]^2 + ridge$beta[2, r1]^2), col = "blue")
  #   arrows(lstsq_beta[1], lstsq_beta[2], ridge$beta[1, r1], ridge$beta[2, r1], len = 0.05, col = "blue")
  # })
  # 
  # output$lassoPlot <- renderPlot({
  #   lambdaIndex <- input$lambdaIndex
  #   k1 <- seq(1, 5, by = 0.01)
  #   k1 <- c(0.1 * k1, k1, 10 * k1, 100 * k1, 1000 * k1)
  #   k1 <- k1[k1 > min(rss_lstsq)]
  #   
  #   r1 <- find_closest(apply(lasso$beta, 2, function(x) sum((z0 - x[1] * z1 - x[2] * z2) ^ 2)), k1[lambdaIndex])
  #   
  #   plot(1.02 * range(s), 1.02 * range(s), type = "n", axes = FALSE, xlab = " ", ylab = " ", main = "(b) LASSO Regression")
  #   axis(side = 1, pos = 0, col = "black", at = seq(-1, 1, length = 5))
  #   axis(side = 2, pos = 0, col = "black", at = seq(-1, 1, length = 5))
  #   text(0.05, 1, TeX("$\\beta_2$"))
  #   text(0.98, 0.08, TeX("$\\beta_1$"))
  #   
  #   contour(s, s, matrix(rss_lstsq, nrow = n_lstsq), levels = k1, add = TRUE, col = "gray", drawlabels = FALSE)
  #   contour(s, s, matrix(rss_lstsq, nrow = n_lstsq), levels = k1[1], add = TRUE, col = "black", drawlabels = FALSE)
  #   
  #   d0 <- abs(lstsq_beta[1]) + abs(lstsq_beta[2])
  #   segments(d0, 0, 0, d0, col = "red")
  #   segments(0, d0, -d0, 0, col = "red")
  #   segments(-d0, 0, 0, -d0, col = "red")
  #   segments(0, -d0, d0, 0, col = "red")
  #   points(lstsq_beta[1], lstsq_beta[2], pch = 19, col = "red")
  #   
  #   d1 <- abs(lasso$beta[1, r1]) + abs(lasso$beta[2, r1])
  #   segments(d1, 0, 0, d1, col = "blue")
  #   segments(0, d1, -d1, 0, col = "blue")
  #   segments(-d1, 0, 0, -d1, col = "blue")
  #   segments(0, -d1, d1, 0, col = "blue")
  #   arrows(lstsq_beta[1], lstsq_beta[2], lasso$beta[1, r1], lasso$beta[2, r1], len = 0.05, col = "blue")
  # })
# }

shinyApp(ui = ui, server = server)
