library(shiny)

ui <- fluidPage(
  
  titlePanel("Géiser Faithful"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      sliderInput(inputId = "dots",
                  label = "Rango de puntos:",
                  min = 1,
                  max = dim(faithful)[1],
                  value = c(1,dim(faithful)[1])),
      
      hr(),
      
      sliderInput(inputId = "bins",
                  label = "Número de Contenedores:",
                  min = 1,
                  max = 50,
                  value = 30)
      
    ),
    
    mainPanel(
      
      fluidRow(
        
        column(6,
               
               plotOutput(outputId = "scatter")
               
               ),
        
        column(6,
               
               plotOutput(outputId = "distPlot")
               
               )
        
      ),
      
      fluidRow(
        
        column(4,
               
               h4("Total observaciones"),
               textOutput("obs")
               
               ),
        
        column(4,
               
               h4("Máximo Tiempo Erupción"),
               textOutput("erupcion")
               
               ),
        
        column(4,
               
               h4("Máximo Tiempo Espera"),
               textOutput("espera")
               
               )
      )
      
    )
  )
)

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Tiempo de espera hasta la próxima erupción (en mins)",
         main = "Histograma de tiempos de esperas")
    
  })
  
  output$scatter <- renderPlot({
    
    x <- faithful[c(seq(input$dots[1], input$dots[2])),]

    plot(x$eruptions, x$waiting, col = "orange", 
         main = "Erupciones frente a Tiempos de Espera",
         xlab = "Tiempo de erupción",
         ylab = "Tiempo de espera"
         )
    
  })
  
  output$obs <- renderText(dim(faithful)[1])
  
  output$erupcion <- renderText(max(faithful$eruptions))
  
  output$espera <- renderText(max(faithful$waiting))
  
}

shinyApp(ui = ui, server = server)