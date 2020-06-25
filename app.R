
# Libraries ---------------------------------------------------------------

library(shiny)
library(shinythemes)
library(shinyjs)

devtools::load_all()
# library(BayesianReasoning)

# UI ----------------------------------------------------------------------

ui <- 
  function(request) {
    
    fluidPage(
      tags$head(includeHTML(("google-analytics.html"))),
      useShinyjs(),
  
      theme = shinythemes::shinytheme("flatly"),
      title = "BayesianReasoning",
      
      # * side panel --------------------------------------------------------------
      
      sidebarLayout(
        sidebarPanel(
          width = 2,
          
          div(
            
            HTML(paste0(
              a(img(src = "github_small.png", title = "Github repository"), href="https://github.com/gorkang/BayesianReasoning", target = "_blank"), "&nbsp;&nbsp;",
              a(img(src = "issue_small.png", title = "Report an issue!"), href="https://github.com/gorkang/BayesianReasoning/issues", target = "_blank"), "&nbsp;&nbsp;",
              a(img(src = "twitter_small.png", title = "@gorkang"), href="https://twitter.com/gorkang", target = "_blank"), "&nbsp;&nbsp;", 
              a(img(src = "https://cdn.buymeacoffee.com/buttons/bmc-new-btn-logo.svg", title = "Buy me a coffee", height = "26px"), href="https://www.buymeacoffee.com/I0rkAbM", target = "_blank"), "&nbsp;", 
              "<BR><BR>")),
            align = "center"
          ),
          
          # h1(),
          
          
          selectInput("PPV_NPV", 
                      "PPV or NPV", 
                      c("PPV", "NPV")), # linea
    
          sliderInput("Min_Prevalence",
                      "Prevalence: [ x ] out of y",
                      min = 1,
                      max = 10000,
                      value = 1),
          
          sliderInput("Max_Prevalence",
                      "Prevalence: x out of [ y ]",
                      min = 1,
                      max = 10000,
                      value = 1000),
          
          sliderInput("Sensitivity",
                      "Sensitivity (%)",
                      min = 0.1,
                      max = 100,
                      value = 90,
                      step = .1),
          
          sliderInput("FP_shinny",
                      "False Positive rate (1-Spec; %)",
                      min = 0,
                      max = 100,
                      value = 2,
                      step = .1),
          
          hr(),
          
          selectInput("tipo_overlay", 
                      "Overlay", 
                      c("none", "area")), # linea
          
          sliderInput("FP_overlay",
                      "False Positive rate overlay (%)",
                      min = 0,
                      max = 100,
                      value = 1,
                      step = 0.1),
          
          
          sliderInput("FN_overlay",
                      "False Negative rate overlay (%)",
                      min = 0,
                      max = 100,
                      value = 1,
                      step = 0.1),
          
          
          # HTML("<B>Prevalence of overlay:</B>"),
          sliderInput("overlay_prevalence_1",
                      "Prevalence of overlay: [ x ] out of y",
                      min = 1,
                      max = 10000,
                      value = 1,
                      step = 1),
          
          sliderInput("overlay_prevalence_2",
                      "Prevalence of overlay: x out of [ y ]",
                      min = 1,
                      max = 10000,
                      value = 200,
                      step = 1),
          
          hr(),
          
          div( HTML("&nbsp;&nbsp;"), style="display:inline-block;65%;text-align: center;",
               bookmarkButton(label = "Share URL")
          ), 
          HTML("&nbsp;&nbsp;"),
          div(style="display:inline-block;30%;text-align: center;",
              downloadButton('downloadPlot', 'Plot')
          ),
          
        ),
      
        
    
        # * main panel --------------------------------------------------------------
    
        mainPanel(width = 10,
                  
                  p(
                    HTML(
                      paste(
                        h3(HTML("<a href='https://gorkang.shinyapps.io/BayesianReasoning/'>BayesianReasoning</a>")),
                        p("Plot Positive Predictive Values (PPV) or Negative Predictive Values (NPV), and their relationship with Sensitivity, Specificity and Prevalence.")
                        # "<a href='https://github.com/gorkang/BayesianReasoning'>GITHUB: gorkang/BayesianReasoning</a> - <a href='https://github.com/gorkang/BayesianReasoning/issues'>Issues</a>"
                        )
                      )
                    ),
                  
                  hr(),
                  
                  plotOutput("outplot", height = "800px", width = "100%"),
                  
                  # plotOutput("outplot"),
                  
                  hr(),
                  span(
                    div(
                      HTML(
                        paste0(
                          "Positive Predictive Value (PPV) = True Positives / All Positives", br(),
                          "Negative Predictive Value (NPV) = True Negatives / All Negatives", br(),br(),
                          "False Negative rate = 1 - Sensitivity", br(),
                          "False Positive rate = 1 - Specificity", br(),
                          hr(),
                          "BayesianReasoning v.0.3. By ", a("@gorkang", href="https://twitter.com/gorkang", target = "_blank"))),
                      align = "center",
                      style = "color:darkgrey")),
                  hr())
        
      )
    )
}

# Server ------------------------------------------------------------------

#' Shiny app server
#'
#' @param input 
#' @param output 
#'
#' @import shiny shinythemes devtools

server <- function(input, output, session) {

  # Change values of sliders depending on other sliders
  observe({
    val_min_prevalence <- input$Min_Prevalence
    val_max_prevalence <- input$Max_Prevalence
    val_sensitivity <- input$Sensitivity
    val_FP <- input$FP_shinny
    val_FP <- input$FP_shinny
    val_overlay_prevalence_1 <- input$overlay_prevalence_1
    val_overlay_prevalence_2 <- input$overlay_prevalence_2

    updateSliderInput(session, "Min_Prevalence", max = val_max_prevalence)
    updateSliderInput(session, "overlay_prevalence_1", min = val_min_prevalence, max = val_overlay_prevalence_2) #value = val_min_prevalence, 
    updateSliderInput(session, "overlay_prevalence_2", min = val_overlay_prevalence_1, max = val_max_prevalence) #value = val_max_prevalence/2, 
    updateSliderInput(session, "FP_overlay", min = 0, max = val_FP) #value = val_FP/2, 
    updateSliderInput(session, "FN_overlay", min = 0, max = (100-val_sensitivity)) #value = (100-val_sensitivity)/2, 
    
  })
  
  
  
  # Hide and show sliders depending on the PPV_NPV AND tipo_overlay
  observeEvent(input$tipo_overlay,{
    
    observeEvent(input$PPV_NPV,{
      
    if (input$tipo_overlay == "none") {
      
      hide("FN_overlay")
      hide("FP_overlay")
      hide("overlay_prevalence_1")
      hide("overlay_prevalence_2")
      
    } else if (input$tipo_overlay == "area") {
      
      if (input$PPV_NPV == "PPV") {
        
        hide("FN_overlay")
        show("overlay_prevalence_1")
        show("overlay_prevalence_2")
        show("FP_overlay")
        
      } else if (input$PPV_NPV == "NPV") {
        
        hide("FP_overlay")
        show("overlay_prevalence_1")
        show("overlay_prevalence_2")
        show("FN_overlay")
        
      }
      
    }
      
  })
    
  })

  

  
  # Hide and show sliders depending on the PPV_NPV
  observeEvent(input$tipo_overlay,{
    if (input$tipo_overlay == "none") {
      hide("FN_overlay")
      hide("FP_overlay")
      hide("overlay_prevalence_1")
      hide("overlay_prevalence_2")
    } else if (input$tipo_overlay == "area") {
      show("FN_overlay")
      show("FP_overlay")
      show("overlay_prevalence_1")
      show("overlay_prevalence_2")
    }
  })
  
  
  
  
  
  
  final_plot <- reactive({
  
    # message(input$PPV_NPV)
    
    PPV_heatmap(
      PPV_NPV = input$PPV_NPV,
      label_title = input$plot_title,
      Max_FP = input$FP_shinny,
      Min_Prevalence = input$Min_Prevalence,
      Max_Prevalence = input$Max_Prevalence,
      Sensitivity = input$Sensitivity,
      overlay = input$tipo_overlay,
      # Min_Prevalence = 1,
      overlay_prevalence_1 = input$overlay_prevalence_1,
      overlay_prevalence_2 = input$overlay_prevalence_2,
      # overlay_position_FP_FN = input$FP_overlay
      overlay_position_FP = input$FP_overlay,
      overlay_position_FN = input$FN_overlay
    )
    
  })
  
  output$outplot <- renderPlot({
    
    final_plot()
    
  # }, height = 800)
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() { 
      # input$plot_title, "_",
      if (input$tipo_overlay == "none") {
        paste0(input$Prevalence, "_", input$Sensitivity, "_", input$FP_shinny, ".png") 
      } else {
        paste0(input$Prevalence, "_", input$Sensitivity, "_", input$FP_shinny, "_", input$tipo_overlay, "_", 1, "_", input$overlay_prevalence_1, "_", input$overlay_prevalence_2, "_", input$FP_overlay, ".png") 
      }
      
      },
    content = function(file) { ggsave(file, plot = final_plot(), device = "png", width = 14, height = 10) }
  )
  
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
