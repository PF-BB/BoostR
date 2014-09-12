binomiale <- function() {
  require(shiny)
  shinyApp(
    ui = fluidPage(responsive = FALSE,
                   fluidRow(style = "padding-bottom: 20px;",
                            column(4, sliderInput('p', 'p', 0, 1, 0, 0.1)),
                            column(4, sliderInput('n', 'n', 1, 10, 3, 1))),
                   mainPanel(tabsetPanel(
                     tabPanel("Distribution de probabilité", fluidRow(plotOutput('dist'), height = "400px")),
                     tabPanel("Fonction de répartition", fluidRow(plotOutput('repa'), height = "400px")),
                     tabPanel("Moyenne et écart-type", fluidRow(tableOutput('mean_std'), height = "400px"))
                   ))
    ),
    server = function(input, output, session) { 
      
      output$dist <- renderPlot(height = 400, {
        plot(0:5, sapply(0:5, dbinom, size=input$n, prob=input$p), 
             lwd=2, type="h", xlab="x", ylab="Probabilité", 
             xlim=c(-0.1,5), ylim=0:1)
        grid()
      })
      
      output$repa <- renderPlot(height = 400, {
        plot(stepfun(0:5, c(0, sapply(0:5, pbinom, size=input$n, prob=input$p))), 
             lwd=2, xlab="x", ylab="Quantile", 
             xlim=c(-0.1,5), ylim=0:1, verticals=F)
        grid()
      })
      
      output$mean_std <- renderTable({ 
        data.frame("Moyenne" = input$n*input$p,
                   "Ecart-type" = input$n*input$p*(1-input$p), check.names = F) })
    },
    options = list(height = 500)
  )
}
