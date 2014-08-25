gaussienne <- function() {
  require(shiny)
  shinyApp(
    ui = fluidPage(responsive = FALSE,
                   fluidRow(style = "padding-bottom: 20px;",
                            column(4, sliderInput('mu', 'Moyenne', -3, 3, 0, 0.1)), 
                            column(4, sliderInput('sigma', 'Ecart-type', 0, 2, 1, 0.1))),
                   mainPanel(tabsetPanel(
                     tabPanel("Densité de probabilité", fluidRow(plotOutput('dens'), height = "400px")),
                     tabPanel("Distribution de probabilité", fluidRow(plotOutput('dist'), height = "400px")),
                     tabPanel("Moyenne et écart-type", fluidRow(tableOutput('mean_std'), height = "400px"))
                   ))
    ),
    server = function(input, output, session) { 
      
      output$dens <- renderPlot(height = 400, {
        plot(function(x) dnorm(x,input$mu,input$sigma), 
             lwd=2, xlab="x", ylab="Densité", 
             xlim=c(-5,5), ylim=0:1,  
             n=201)
        grid()
      })
      
      output$dist <- renderPlot(height = 400, {
        plot(function(x) pnorm(x,input$mu,input$sigma), 
             lwd=2, xlab="x", ylab="Quantile", 
             xlim=c(-5,5), ylim=0:1,  
             n=201)
        grid()
      })
      
      output$mean_std <- renderTable({ 
        data.frame("Moyenne" = input$mu,
                   "Ecart-type" =input$sigma, check.names = F) })
    },
    options = list(height = 500)
  )
}
