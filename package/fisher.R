fisher <- function() {
  require(shiny)
  shinyApp(
    ui = fluidPage(responsive = FALSE,
                   fluidRow(style = "padding-bottom: 20px;",
                            column(4, sliderInput('df1', 'Degrés de liberté numérateur',   3, 100, 3, 1)), 
                            column(4, sliderInput('df2', 'Degrés de liberté dénominateur', 3, 100, 3, 1)), 
                            column(4, checkboxInput("addnorm", "Loi normale ?", value = FALSE))),
                   mainPanel(tabsetPanel(
                     tabPanel("Distribution de probabilité", fluidRow(plotOutput('dist'), height = "400px")),
                     tabPanel("Fonction de répartition", fluidRow(plotOutput('repa'), height = "400px")),
                     tabPanel("Moyenne et écart-type", fluidRow(tableOutput('mean_std'), height = "400px"))
                   ))
    ),
    server = function(input, output, session) { 
      
      output$dist <- renderPlot(height = 400, {
        plot(function(x) df(x,input$df1,input$df2), 
             lwd=2, xlab="x", ylab="Densité", 
             xlim=c(-0.1,5), ylim=c(0,2),  
             n=201)
        grid()
        if (input$addnorm) {
          plot(function(x) dnorm(x,input$df2/(input$df2-2),
                                 sqrt( (input$df1-2)/input$df1 * input$df2/(input$df2+2)) ), 
               lwd=2, col="green", lty=2, add=TRUE, xlim=c(-0.1,5))
          legend("topright", c("Student", "Normale"), lty=1:2, col=c(1,3), lwd=2, bty = "n")
        }
        
      })
      
      output$repa <- renderPlot(height = 400, {
        plot(function(x) pf(x,input$df1,input$df2), 
             lwd=2, xlab="x", ylab="Quantile", 
             xlim=c(-0.1,5), ylim=0:1,  
             n=201)
        grid()
        if (input$addnorm) {
          plot(function(x) pnorm(x,input$df2/(input$df2-2),
                                 sqrt( (input$df1-2)/input$df1 * input$df2/(input$df2+2)) ), 
               lwd=2, col="green", lty=2, add=TRUE, xlim=c(-0.1,5))
          legend("bottomright", c("Student", "Normale"), lty=1:2, col=c(1,3), lwd=2, bty = "n")
        }
      })
      
      output$mean_std <- renderTable({ 
        data.frame("Moyenne" = input$df2/(input$df2-2),
                   "Ecart-type" = sqrt( (input$df1-2)/input$df1 * input$df2/(input$df2+2) ),
                   check.names = F) })
    },
    options = list(height = 500)
  )
}
