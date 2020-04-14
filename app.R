
# Libraries ---------------------------------------------------------------

library(shiny)
library(shinythemes)
devtools::load_all()


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  
  theme = shinythemes::shinytheme("flatly"),
  title = "BayesianReasoning",
  
  # * side panel --------------------------------------------------------------
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      
      h1(),

      sliderInput("Prevalence",
                  "Prevalence: 1 out of [ y ]",
                  min = 1,
                  max = 2000,
                  value = 1000),
      
      sliderInput("Sensitivity",
                  "Sensitivity (%)",
                  min = 50,
                  max = 100,
                  value = 90,
                  step = .1),
      
      sliderInput("FP_shinny",
                  "False Positives (FP; %)",
                  min = 0,
                  max = 10,
                  value = 2,
                  step = .1),
      
      hr(),
      
      selectInput("tipo_overlay", 
                  "Overlay", 
                  c("none", "area")), # linea
      
      sliderInput("FP_overlay",
                  "FP overlay (%)",
                  min = 0,
                  max = 10,
                  value = 1,
                  step = 0.1),
      
      HTML("<B>Prevalence of overlay:</B>"),
      sliderInput("overlay_prevalence_1",
                  "[ x ] out of y",
                  min = 1,
                  max = 2000,
                  value = 1,
                  step = 1),
      
      sliderInput("overlay_prevalence_2",
                  "x out of [ y ]",
                  min = 1,
                  max = 2000,
                  value = 200,
                  step = 1)),
  
    

    # * main panel --------------------------------------------------------------

    mainPanel(width = 10,
              
              HTML(
                paste(
                  h3("BayesianReasoning v.0.2"),
                  p("Plot Positive Predictive Values, and their relationship with Sensitivity, Specificity and Prevalence."),
                  "<a href='https://github.com/gorkang/BayesianReasoning'>GITHUB: gorkang/BayesianReasoning</a> - <a href='https://github.com/gorkang/BayesianReasoning/issues'>Issues</a>")),
              
              plotOutput("outplot"))
    
  )
)


# Server ------------------------------------------------------------------

#' Shiny app server
#'
#' @param input 
#' @param output 
#'
#' @import shiny shinythemes devtools

server <- function(input, output) {

  output$outplot <- renderPlot({
    PPV_heatmap(
      PPV_NPV = "PPV",
      label_title = input$plot_title,
      Max_FP = input$FP_shinny,
      Max_Prevalence = input$Prevalence,
      Sensitivity = input$Sensitivity,
      overlay = input$tipo_overlay,
      Min_Prevalence = 1,
      overlay_prevalence_1 = input$overlay_prevalence_1,
      overlay_prevalence_2 = input$overlay_prevalence_2,
      overlay_position_FP_FN = input$FP_overlay
    )
  }, height = 800)
}

shinyApp(ui = ui, server = server)
