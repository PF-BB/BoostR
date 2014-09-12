poisson <- function() {
  require(shiny)
  shinyApp(
    ui = fluidPage(responsive = FALSE,
                   fluidRow(style = "padding-bottom: 20px;",
                            column(4, sliderInput('lambda', HTML("&lambda;:"), 0, 10, 2, 0.1))),
                   mainPanel(tabsetPanel(
                     tabPanel("Distribution de probabilité", fluidRow(plotOutput('dist'), height = "400px")),
                     tabPanel("Fonction de répartition", fluidRow(plotOutput('repa'), height = "400px")),
                     tabPanel("Moyenne et écart-type", fluidRow(tableOutput('mean_std'), height = "400px"))
                   ))
    ),
    server = function(input, output, session) { 
      
      output$dist <- renderPlot(height = 400, {
        plot(0:10, sapply(0:10, dpois, lambda=input$lambda), 
             lwd=2, type="h", xlab="x", ylab="Probabilité", 
             xlim=c(-0.1,10.1), ylim=c(0, 0.5))
        grid()
      })
      
      output$repa <- renderPlot(height = 400, {
        plot(stepfun(0:10, c(0, sapply(0:10, ppois, lambda=input$lambda))), 
             lwd=2, xlab="x", ylab="Quantile", 
             xlim=c(-0.1,10.1), ylim=0:1, verticals=F)
        grid()
      })
      
      output$mean_std <- renderTable({ 
        data.frame("Moyenne" = input$lambda,
                   "Ecart-type" = input$lambda, check.names = F) })
    },
    options = list(height = 500)
  )
}



numericInput(
  inputId = "beta",
  label = HTML("&beta;:"),
  value = 0.05,
  step = 0.01
)
