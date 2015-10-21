library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Sampling Distribution - Theory and Simulation"),
  
  
  
  fluidRow(
    
    
    
    column(4,
           wellPanel(
             
             sliderInput("n", 
                         "Sample Size:", 
                         value = 100,
                         min = 2, 
                         max = 500),
             br(),
             
             sliderInput("p", 
                         "Proportion in Population (%):", 
                         value = 50,
                         step = 1,
                         min = 0, 
                         max = 100),
             br() 
           )
           ),

    column(8,
           plotOutput("binom.dist"),
           br()
    ),
    column(12,
           plotOutput("sampling.dist"),
           br()
    )
    )

))