library(shiny)


shinyUI(fluidPage(
  
  titlePanel("Sampling Distribution - Theory And Simulation Comparison"),
  
  sidebarLayout(
    
    sidebarPanel(
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
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Theory", plotOutput("binom.dist")), 
        tabPanel("Simulation", plotOutput("sampling.dist")), 
        tabPanel("Documentation", includeMarkdown("doc.Rmd"))
      )
    )
  )
))


