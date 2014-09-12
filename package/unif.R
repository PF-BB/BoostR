unif <- function() {
  require(shiny)
  maxi <- 5
  shinyApp(
    ui = fluidPage(responsive = FALSE,
                   fluidRow(style = "padding-bottom: 20px;",
                            column(4, sliderInput('a', 'Paramètre a', -maxi, maxi, 0, 0.1)), 
                            column(4, sliderInput('b', 'Paramètre b', -maxi, maxi, 1, 0.1))),
                   mainPanel(tabsetPanel(
                     tabPanel("Distribution de probabilité", fluidRow(plotOutput('dist'), height = "400px")),
                     tabPanel("Fonction de répartition", fluidRow(plotOutput('repa'), height = "400px")),
                     tabPanel("Moyenne et écart-type", fluidRow(tableOutput('mean_std'), height = "375px"))
                   ))
    ),
    server = function(input, output, session) { 

      output$dist <- renderPlot(height = 375, {
        plot(function(x) dunif(x,input$a,input$b), 
             lwd=2, xlab="x", ylab="Densité", 
             xlim=extendrange(c(-maxi,maxi)), ylim=0:1,  
             n=201, type="S")
        grid()
      })

      output$repa <- renderPlot(height = 375, {
        plot(function(x) punif(x,input$a,input$b), 
             lwd=2, xlab="x", ylab="Quantile", 
             xlim=extendrange(c(-maxi,maxi)), ylim=0:1,  
             n=201)
        grid()
      })
      
      output$mean_std <- renderTable({ 
        data.frame("Moyenne" = round((input$b+input$a)/2,2),
                   "Ecart-type" = round(sqrt((input$b-input$a)**2/12),2),check.names = F) })
    },
    options = list(height = 475)
  )
}
