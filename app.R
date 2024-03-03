
# Libraries ---------------------------------------------------------------

library(shiny)
library(shinythemes)
library(shinyjs)
library(devtools)
library(ggplot2)

source("R/helper_functions_exp.R")
# REMEMBER: If deploying the app to shinyapps.io FAILS, add devtools and utf8 to the Imports field in the DESCRIPTION file -------------------
devtools::load_all()
# library(BayesianReasoning)



ui <- 
  function(request) {
    
    fluidPage(
      # tags$head(includeHTML(("google-analytics.html"))),
      useShinyjs(),
      theme = shinythemes::shinytheme("flatly"),
      title = "BayesianReasoning",
      
      
# side panel --------------------------------------------------------------
      
      sidebarLayout(
        sidebarPanel(
          width = 2,
          
        # Panel 1  --------------------------------------------------------------
        
          conditionalPanel(condition="input.tabselected==1",
          
          div(
            
            HTML(paste0(
              a(img(src = "github_small.png", title = "Github repository"), href="https://github.com/gorkang/BayesianReasoning", target = "_blank"), "&nbsp;&nbsp;",
              a(img(src = "issue_small.png", title = "Report an issue!"), href="https://github.com/gorkang/BayesianReasoning/issues", target = "_blank"), "&nbsp;&nbsp;",
              a(img(src = "twitter_small.png", title = "@gorkang"), href="https://twitter.com/gorkang", target = "_blank"), "&nbsp;&nbsp;", 
              a(img(src = "https://cdn.buymeacoffee.com/buttons/bmc-new-btn-logo.svg", title = "Buy me a coffee", height = "26px"), href="https://www.buymeacoffee.com/I0rkAbM", target = "_blank"), "&nbsp;", 
              "<BR><BR>")),
            align = "center"
          ),
          
          selectInput("PPV_NPV", 
                      "PPV or NPV", 
                      c("PPV", "NPV")), # linea
    
          sliderInput("min_Prevalence",
                      "Prevalence: [ x ] out of y",
                      min = 1,
                      max = 10000,
                      value = 1),
          
          sliderInput("max_Prevalence",
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
          
          sliderInput("Specificity",
                      "Specificity (%)",
                      min = 0,
                      max = 100,
                      value = 98,
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
          
          

  # Panel 2 -----------------------------------------------------------------
        
      conditionalPanel(condition="input.tabselected==2",
                       
                       h4("Health professionals"),
                       
                       selectInput("Disease_E1", 
                                   "Cancer or Stroke", 
                                   c("Cancer", "Stroke")), 
                       selectInput("PPV_NPV_E1", 
                                   "PPV or NPV", 
                                   c("PPV", "NPV")), # linea
                       HTML("&nbsp;&nbsp;"),
                       div(style="display:inline-block;30%;text-align: center;",
                           downloadButton('downloadPlot_E1', 'Plot'))
                       ),
  
  # Panel 3 -----------------------------------------------------------------
  
    conditionalPanel(condition="input.tabselected==3",
                   
                   h4("Patients"),
                   
                   selectInput("Disease_E2", 
                               "Cancer or Stroke", 
                               c("Cancer", "Stroke")), 
                   selectInput("PPV_NPV_E2", 
                               "PPV or NPV", 
                               c("PPV", "NPV")), # linea
                   HTML("&nbsp;&nbsp;"),
                   div(style="display:inline-block;30%;text-align: center;",
                       downloadButton('downloadPlot_E2', 'Plot'))
                   )
        ),
      
        
    
  # * main panel --------------------------------------------------------------

  mainPanel(width = 10, tabsetPanel(
    tabPanel("Plot", value=1,
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
            hr()
       
              ),
        tabPanel("Medical professionals", value = 2,
                 h4("Health professionals visual aid"),
                 br(),
                 plotOutput("outplot2", height = "800px", width = "100%")
                 ),

        tabPanel("Patients", value = 3,
                 h4("Patient's visual aid"),
                 br(),
                 plotOutput("outplot3", height = "800px", width = "100%")
                 ),
        id = "tabselected"
            )
        
        ) # Mainpanel
    ) # sidebarLayout
  ) #fluidPage
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
    val_min_prevalence <- input$min_Prevalence
    val_max_prevalence <- input$max_Prevalence
    val_sensitivity <- input$Sensitivity
    val_specificity <- input$Specificity
    # val_FP <- input$Specificity
    # val_FP <- input$Specificity
    val_overlay_prevalence_1 <- input$overlay_prevalence_1
    val_overlay_prevalence_2 <- input$overlay_prevalence_2

    updateSliderInput(session, "min_Prevalence", max = val_max_prevalence)
    updateSliderInput(session, "overlay_prevalence_1", min = val_min_prevalence, max = val_overlay_prevalence_2) #value = val_min_prevalence, 
    updateSliderInput(session, "overlay_prevalence_2", min = val_overlay_prevalence_1, max = val_max_prevalence) #value = val_max_prevalence/2, 
    # updateSliderInput(session, "FP_overlay", min = 0, max = val_FP) #value = val_FP/2, 
    updateSliderInput(session, "FP_overlay", min = 0, max = (100-val_specificity)) #value = (100-val_sensitivity)/2, 
    updateSliderInput(session, "FN_overlay", min = 0, max = (100-val_sensitivity)) #value = (100-val_sensitivity)/2, 
    
  })
  
  
  
  # Hide and show sliders depending on the PPV_NPV AND tipo_overlay
  observeEvent(input$tipo_overlay,{
    
    observeEvent(input$PPV_NPV,{
      
    if (input$tipo_overlay == "none") {
      
      shinyjs::hide("FN_overlay")
      shinyjs::hide("FP_overlay")
      shinyjs::hide("overlay_prevalence_1")
      shinyjs::hide("overlay_prevalence_2")
      
    } else if (input$tipo_overlay == "area") {
      
      if (input$PPV_NPV == "PPV") {
        
        shinyjs::hide("FN_overlay")
        shinyjs::show("overlay_prevalence_1")
        shinyjs::show("overlay_prevalence_2")
        shinyjs::show("FP_overlay")
        
      } else if (input$PPV_NPV == "NPV") {
        
        shinyjs::hide("FP_overlay")
        shinyjs::show("overlay_prevalence_1")
        shinyjs::show("overlay_prevalence_2")
        shinyjs::show("FN_overlay")
        
      }
      
    }
      
  })
    
  })

  

  # Create plot
  final_plot <- reactive({
  
    BayesianReasoning::PPV_heatmap(
      PPV_NPV = input$PPV_NPV,
      label_title = input$plot_title,
      min_Prevalence = input$min_Prevalence,
      max_Prevalence = input$max_Prevalence,
      Sensitivity = input$Sensitivity,
      Specificity = input$Specificity,
      overlay = input$tipo_overlay,
      # min_Prevalence = 1,
      overlay_prevalence_1 = input$overlay_prevalence_1,
      overlay_prevalence_2 = input$overlay_prevalence_2,
      # overlay_position_FP_FN = input$FP_overlay
      overlay_position_FP = input$FP_overlay,
      overlay_position_FN = input$FN_overlay, DEBUG = TRUE
    )$p
    
  })
  
  
  # Create plot E1 --------------------------------------------------------
  
  final_plot_E1 <- reactive({
    if (input$Disease_E1 == "Cancer") {
      data = create_table_BC()
    } else {
      data = create_table_Stroke()
    }
    plot_E1(DF = data, PPV_NPV = input$PPV_NPV_E1)
  })

  output$outplot2 <- renderPlot({
    final_plot_E1()
  })

  output$downloadPlot_E1 <- downloadHandler(
    filename = function() { 
      paste0(input$Disease_E1, "_", input$PPV_NPV_E1, ".png") 
    },
    content = function(file) { ggsave(file, plot = final_plot_E1(), device = "png", width = 14, height = 10, bg = "white") }
  )
  
  
  
# Create plot Experiment 2  --------------------------------------------------
  final_plot_E2 <- reactive({
    if (input$Disease_E2 == "Cancer") {
      data = create_table_BC()
    } else {
      data = create_table_Stroke()
    }
    plot_E2(DF = data, PPV_NPV = input$PPV_NPV_E2)
  })
  
  output$outplot3 <- renderPlot({
    final_plot_E2()
  })

  output$downloadPlot_E2 <- downloadHandler(
    filename = function() { 
        paste0(input$Disease_E2, "_", input$PPV_NPV_E2, ".png") 
      },
    content = function(file) { ggsave(file, plot = final_plot_E2(), device = "png", width = 14, height = 10, bg = "white") }
  )
  
  

# PLOT --------------------------------------------------------------------

  output$outplot <- renderPlot({
    final_plot()
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() { 
      if (input$tipo_overlay == "none") {
        paste0(input$Prevalence, "_", input$Sensitivity, "_", input$Specificity, ".png") 
      } else {
        paste0(input$Prevalence, "_", input$Sensitivity, "_", input$Specificity, "_", input$tipo_overlay, "_", 1, "_", input$overlay_prevalence_1, "_", input$overlay_prevalence_2, "_", input$FP_overlay, ".png") 
      }
      
      },
    content = function(file) { ggsave(file, plot = final_plot(), device = "png", width = 14, height = 10) }
  )
  
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
