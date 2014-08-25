student <- function() {
  require(shiny)
  shinyApp(
    ui = fluidPage(responsive = FALSE,
                   fluidRow(style = "padding-bottom: 20px;",
                            column(4, sliderInput('df', 'Degrés de liberté', 1, 20, 3, 1)), 
                            column(4, checkboxInput("addnorm", "Loi normale ?", value = FALSE))),
                   mainPanel(tabsetPanel(
                     tabPanel("Densité de probabilité", fluidRow(plotOutput('dens'), height = "400px")),
                     tabPanel("Fonction de répartition", fluidRow(plotOutput('dist'), height = "400px")),
                     tabPanel("Moyenne et écart-type", fluidRow(tableOutput('mean_std'), height = "400px"))
                   ))
    ),
    server = function(input, output, session) { 
      
      output$dens <- renderPlot(height = 400, {
        plot(function(x) dt(x,input$df), 
             lwd=2, xlab="x", ylab="Densité", 
             xlim=c(-5,5), ylim=c(0,0.5),  
             n=201)
        grid()
        if (input$addnorm) {
          plot(dnorm, lwd=2, col="green", lty=2, add=TRUE, xlim=c(-5,5))
          legend("topleft", c("Student", "Normale"), lty=1:2, col=c(1,3), lwd=2, bty = "n")
        }
          
      })
      
      output$dist <- renderPlot(height = 400, {
        plot(function(x) pt(x,input$df), 
             lwd=2, xlab="x", ylab="Quantile", 
             xlim=c(-5,5), ylim=0:1,  
             n=201)
        grid()
        if (input$addnorm) {
          plot(pnorm, lwd=2, col="green", lty=2, add=TRUE, xlim=c(-5,5))
          legend("topleft", c("Student", "Normale"), lty=1:2, col=c(1,3), lwd=2, bty = "n")
        }
      })
        
      output$mean_std <- renderTable({ 
        data.frame("Moyenne" = 0,
                   "Ecart-type" = ifelse(input$df <= 2, +Inf, sqrt(input$df/(input$df-2))) , check.names = F) })
    },
    options = list(height = 500)
  )
}
