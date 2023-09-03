library(shiny)
require(ggplot2)

ui <- fluidPage(
  titlePanel("Time dependent coin toss"),
  sidebarLayout(ui
    sidebarPanel(
      sliderInput(inputId = "n",
                  label = "Number of players",
                  min = 2,
                  max = 50,
                  value = 10),
      sliderInput(inputId = "nt",
                  label = "Number of time steps",
                  min = 10,
                  max = 5000,
                  value = 700),
      sliderInput(inputId = "iv",
                  label = "Initial wealth",
                  min = 10,
                  max = 1000,
                  value = 500),
      sliderInput(inputId = "gain",
                  label = "Percentage wealth increase if tail",
                  min = 0.0,
                  max = 1.0,
                  value = 0.5),
      sliderInput(inputId = "loss",
                  label = "Percentage wealth decrease if head",
                  min = 0.0,
                  max = 1.0,
                  value = 0.4)
    ),
    mainPanel(
      plotOutput(outputId = "wealthTimePlot")
    )
  )
)

server <- function(input, output) {
  output$wealthTimePlot <- renderPlot({
    # x    <- faithful$waiting
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # hist(x, breaks = bins, col = "#007bc2", border = "white",
    #      xlab = "Waiting time to next eruption (in mins)",
    #      main = "Histogram of waiting times")
    nt <- input$nt
    n  <- input$n
    iv <- input$iv
    
    gain <- input$gain
    loss <- input$loss
    
    x <- numeric()
    x[1:n] <- iv
    Xt <- matrix(0.0, nrow=n, ncol=nt+1)
    Xt[, 1] = x 
    x
    for (i in 1:nt){
      rands = (runif(n, 0, 1) < 0.5)
      for (j in 1:n){
        if (rands[j]) {
          x[j] = (x[j] + x[j]*gain)
        } else {
          x[j] = (x[j] - x[j]*loss)
        }
      }
      Xt[, i+1] = x
    }
    matplot(t(Xt), type="l", lty=2, col="gray", log="y", ylab="Wealth", xlab="Time")
    XtGmean = exp(colMeans(log(Xt)))
    lines(1:(nt+1), XtGmean, col="blue")
    legend("topright", legend=c("Individual trajectories", "Geometric mean"),
           col = c("grey","blue"), pch=c("-", "-"), lwd = c(3,3))
    
    
    
  })
}

shinyApp(ui = ui, server = server)
