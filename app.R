# Deploy app
# rsconnect::deployApp(appFiles = c("app.R", "www/"), appName = "BayesianReasoning")

# Libraries ---------------------------------------------------------------

# library(devtools)
# library(MASS)
# library(DBI)

library(shiny)
library(shinythemes)
library(shinyjs)
library(ggplot2)

# source("R/helper_functions_exp.R")

# REMEMBER: If deploying the app to shinyapps.io FAILS, add devtools and utf8 to the Imports field in the DESCRIPTION file
# devtools::load_all()
# pak::pkg_install("gorkang/BayesianReasoning")
library(BayesianReasoning)


# helper functions --------------------------------------------------------

create_table_Stroke <- function() {
  
  # See README.md for more information about the sources for the numbers
  
  # Brunser, A. M., Cavada, G., Venturelli, P. M., Olavarría, V., Rojo, A., Almeida, J., Díaz, V., Hoppe, A., & Lavados, P. (2018). Diffusion-weighted imaging determinants for acute ischemic stroke diagnosis in the emergency room. Neuroradiology, 60(7), 687–692. https://pubmed.ncbi.nlm.nih.gov/29789895/
  # prevalence 621 out of 711 -> 873 out of 1000: (621/711) * 1000
  # DWI demonstrated 87.3% sensitivity and 99.0% specificity,
  
  specificity = .99
  
  
  # Time dependent numbers
  # Oppenheim et al. 2000 False-negative Diffusion-weighted MR Findings in Acute Ischemic Stroke. https://pubmed.ncbi.nlm.nih.gov/11003275/
  # Extracted an approximate middle point for each time range from FIG 4.
  TABLE_raw = t(tibble::tibble(
    A = c("0-5", "873 in 1000", .55),
    B = c("6-10", "873 in 1000", .35),
    C = c("11-15", "873 in 1000", .25),
    D = c("16-20", "873 in 1000", .15),
    E = c("21-25", "873 in 1000", .1))) |> 
    tibble::as_tibble(.name_repair = "minimal")
  
  colnames(TABLE_raw) <- c("time_since_onset", "prevalence", "false_negatives")
  
  TABLE = 
    TABLE_raw |> 
    dplyr::mutate(time_since_onset = as.factor(time_since_onset),
    ) |> 
    dplyr::mutate(prevalence_1 = as.numeric(gsub(" in 1000", "", prevalence)),
           prevalence_2 = 1000,
           prevalence = prevalence_1/prevalence_2,
           sensitivity = 1 - as.numeric(false_negatives),
           specificity = specificity) |> 
    # PPV <- TRUE_positives / (TRUE_positives + FALSE_positives)
    dplyr::mutate(PPV = (prevalence * sensitivity)/((prevalence * sensitivity) + ((1-prevalence) * (1-specificity)))) |> 
    # NPV <- TRUE_negatives / (TRUE_negatives + FALSE_negatives)
    dplyr::mutate(NPV = ((1-prevalence) * specificity)/(((1-prevalence) * specificity) + (prevalence * (1-sensitivity)))) |> 
    dplyr::mutate(time_since_onset = forcats::fct_reorder(time_since_onset, PPV))
  
  
  return(TABLE)
  
}




create_table_BC <- function() {
  
  # See README.md for more information about the sources for the numbers
  
  # Influence of personal characteristics of individual women on sensitivity and specificity of mammography in the Million Women Study: cohort study <https://doi.org/10.1136/bmj.329.7464.477> <https://www.ncbi.nlm.nih.gov/pmc/articles/PMC515195/>
  sensitivity = .866
  specificity = .968
  
  # Giaquinto, A.N., Sung, H., Miller, K.D., Kramer, J.L., Newman, L.A., Minihan, A., Jemal, A. and Siegel, R.L. (2022), Breast Cancer Statistics, 2022. CA A Cancer J Clin, 72: 524-541. <https://doi.org/10.3322/caac.21754> <https://acsjournals.onlinelibrary.wiley.com/doi/full/10.3322/caac.21754>
  # Table 2:
  TABLE_raw = t(tibble::tibble(
    # A = c("20", "0.1%", "1 in 1439"),
    B = c("30", "0.5%", "1 in 204"),
    C = c("40", "1.6%", "1 in 63"),
    D = c("50", "2.4%", "1 in 41"),
    E = c("60", "3.5%", "1 in 28"),
    F = c("70", "4.1%", "1 in 24")
    # G = c("80", "3.0%", "1 in 33")
  )) |> 
    tibble::as_tibble(.name_repair = "minimal")
  
  colnames(TABLE_raw) <- c("age", "prob_cancer", "prevalence")
  
  TABLE = 
    TABLE_raw |> 
    dplyr::mutate(prevalence_1 = 1,
           prevalence_2 = as.numeric(gsub("1 in ", "", prevalence)),
           prevalence = prevalence_1/prevalence_2,
           sensitivity = sensitivity,
           specificity = specificity) |> 
    # PPV <- TRUE_positives / (TRUE_positives + FALSE_positives)
    dplyr::mutate(PPV = (prevalence * sensitivity)/((prevalence * sensitivity) + ((1-prevalence) * (1-specificity)))) |> 
    # NPV <- TRUE_negatives / (TRUE_negatives + FALSE_negatives)
    dplyr::mutate(NPV = ((1-prevalence) * specificity)/(((1-prevalence) * specificity) + (prevalence * (1-sensitivity))))
  
  
  
  return(TABLE)
  
}



plot_E1 <- function(DF, PPV_NPV) {
  
  # cli::cli_h1("{names(DF)[1]} - {PPV_NPV}")
  # targets::tar_load_globals()
  # DF = tar_read("data_Stroke")
  # DF = tar_read("data_BC")
  # PPV_NPV = "NPV"
  
  
  # Specific parameters
  
  if (names(DF)[1] == "age") {
    
    item_label = "Valor Predictivo Positivo en mujeres de 40 años sin antecedentes"
    overlay_labels = "Mujer de 40 años"
    label_title = "Mamografía digital para detectar cáncer de mama"
    min_Prevalence = 1
    max_Prevalence = 1000
    
    DATA_plot = DF |> dplyr::filter(age == 40)
    
    if(PPV_NPV == "PPV") {
      name_file = "outputs/E1/VPP_low_Cancer.png"
      limits_Specificity = c(95, 100)
    } else {
      name_file = "outputs/E1/VPN_high_Cancer.png"
      limits_Sensitivity = c(80, 100)
    } 
    
    
  } else if (names(DF)[1] == "time_since_onset") {
    
    item_label = "Valor Predictivo Positivo en personas de 40 años con inicio de síntomas hace menos de 5 horas"
    overlay_labels = "Persona de 40 años"
    label_title = "Resonancia magnética de difusión para detectar Infarto cerebral"
    min_Prevalence = 600
    max_Prevalence = 1000
    
    DATA_plot = DF |> dplyr::filter(time_since_onset == "0-5")
    
    
    if(PPV_NPV == "PPV") {
      name_file = "outputs/E1/VPP_high_Stroke.png"
      limits_Specificity = c(95, 100)
    } else {
      name_file = "outputs/E1/VPN_low_Stroke.png"
      limits_Sensitivity = c(40, 100)
    } 
    
    
  } else {
    
    # EXAMPLE PLOTS
    
    item_label = "Valor Predictivo Positivo en personas de 90 años con alguna característica"
    overlay_labels = "Persona de 90 años"
    label_title = "Prueba médica para detectar condición"
    min_Prevalence = 1
    max_Prevalence = 1000
    
    DATA_plot = tibble::tibble(prevalence = 1/20, 
                       false_negatives = ".02", prevalence_1 = 1, prevalence_2 = 20, 
                       sensitivity = .99, specificity = 0.95, 
                       PPV = 0.51, NPV = 0.97
    )
    
    
    if(PPV_NPV == "PPV") {
      name_file = "outputs/E1/Example_PPV.png"
      limits_Specificity = c(90, 100)
    } else {
      name_file = "outputs/E1/Example_NPV.png"
      limits_Sensitivity = c(95, 100)
    } 
    
  }
  
  
  # Parameters
  
  
  overlay_prevalence_1 = DATA_plot$prevalence_1
  overlay_prevalence_2 = DATA_plot$prevalence_2 
  
  if(PPV_NPV == "PPV") {
    
    limits_Sensitivity = NULL
    Sensitivity = DATA_plot$sensitivity * 100
    Specificity = NULL
    overlay_position_FP = (1 - DATA_plot$specificity)*100
    overlay_position_FN = NULL
    
  } else {
    
    limits_Specificity = NULL
    Sensitivity = NULL
    Specificity = DATA_plot$specificity * 100
    overlay_position_FP = NULL
    overlay_position_FN = (1 - DATA_plot$sensitivity)*100
    
  } 
  
  # Plot
  
  
  plot = BayesianReasoning::PPV_heatmap(
    PPV_NPV =  PPV_NPV, overlay = "area", one_out_of = FALSE, 
    min_Prevalence = min_Prevalence, max_Prevalence = max_Prevalence, 
    limits_Sensitivity = limits_Sensitivity, 
    limits_Specificity = limits_Specificity,
    Sensitivity = Sensitivity, 
    Specificity = Specificity, 
    overlay_position_FP = overlay_position_FP, 
    overlay_position_FN = overlay_position_FN,
    overlay_prevalence_1 = overlay_prevalence_1, 
    overlay_prevalence_2 = overlay_prevalence_2, 
    overlay_labels = overlay_labels,
    label_title = label_title,
    label_subtitle = item_label, 
    Language = "sp")$p
  
  plot
  
}

plot_E2 <- function(DF, PPV_NPV) {
  
  # targets::tar_load_globals()
  # DF = tar_read("data_BC")
  # PPV_NPV = "PPV"
  
  if (names(DF)[1] == "age") {
    test = "Mammogram"
    label_x = "Age of the woman"
    BreastCancer_Stroke = "Breast cancer"
    
    if(PPV_NPV == "PPV") {
      name_file = "outputs/E2/VPP_low_Cancer.png"
    } else {
      name_file = "outputs/E2/VPN_high_Cancer.png"
    } 
    
  } else if (names(DF)[1] == "time_since_onset") {
    test = "Difusion Magnetic Resonance"
    label_x = "Hours since symptoms started"
    BreastCancer_Stroke = "Stroke"
    
    if(PPV_NPV == "PPV") {
      name_file = "outputs/E2/VPP_high_Stroke.png"
    } else {
      name_file = "outputs/E2/VPN_low_Stroke.png"
    } 
    
  } else {
    test = "Test"
    label_x = ifelse (PPV_NPV == "PPV", "Age of the woman", "Hours since symptoms started") 
    BreastCancer_Stroke = "Example"
    
    if(PPV_NPV == "PPV") {
      name_file = "outputs/E2/Example_PPV.png"
    } else {
      name_file = "outputs/E2/Example_NPV.png"
    } 
  }
  
  
  
  
  if (PPV_NPV == "PPV") {
    label_y = "Positive Predictive Value"
  } else {
    label_y = "Negative Predictive Value"
  }
  
  label_title = paste0(test, " to detect ", BreastCancer_Stroke)
  label_subtitle = label_y
  
  plot = DF |> 
    dplyr::mutate(PPV = PPV *100,
                  NPV = NPV * 100) |> 
    ggplot(aes(x = get(names(DF)[1]), y = get(PPV_NPV))) +      # plot canvas
    scale_y_continuous(labels=function(x) paste0(x,"%"), # append % to y-axis value
                       
                       limits = c(0,100)) +              # set y-axis limits
    geom_point(size = 5.5, color = "#009999", shape = 19) + # insert points with ppv value
    geom_line(aes(x = get(names(DF)[1]), y = get(PPV_NPV)), color = "#009999", linewidth = 2, group = 1) +
    theme_minimal(base_size = 16) + # insert line bridging PPV-value points
    # theme(axis.text = element_text(size = 25),                             # axis-numbers size
    #       axis.title = element_text(size = 25)) +                          # axis-labels size
    geom_text(aes(label = paste0(round(get(PPV_NPV), 0), "%"), #case_when(age %in% age_ppv_to_plot ~ paste0(round(PPV_100, 0), "%"), TRUE ~ paste0("")), # keep only ages previously set to be ploted
                  hjust = .4, vjust = 2.5), size = 6) + # (position) plot ppv-values above points set in "age_ppv_to_plot"
    labs(title = label_title,
         subtitle = label_subtitle,
         y = label_y,
         x = label_x)
  
  
  # name_file = gsub(" ", "", paste0("outputs/E2/", BreastCancer_Stroke, "_", PPV_NPV, ".png"))
  # ggsave(name_file, plot,  width = 10, height = 6, dpi = 300, bg = "white")
  plot
  
}



# UI ----------------------------------------------------------------------


ui <- 
  function(request) {
    
    fluidPage(
      # tags$head(includeHTML(("google-analytics.html"))),
      useShinyjs(),
      theme = shinythemes::shinytheme("flatly"),
      title = "BayesianReasoning",
      
      
# SIDE panel --------------------------------------------------------------
      
      sidebarLayout(
        sidebarPanel(
          width = 2,
          
        ## 1: plot  --------------------------------------------------------------
        
          conditionalPanel(condition="input.tabselected==1",
          
          div(
            
            HTML(paste0(
              a(img(src = "github_small.png", title = "Github repository"), href="https://github.com/gorkang/BayesianReasoning", target = "_blank"), "&nbsp;&nbsp;",
              a(img(src = "issue_small.png", title = "Report an issue!"), href="https://github.com/gorkang/BayesianReasoning/issues", target = "_blank"), "&nbsp;&nbsp;",
              a(img(src = "mastodon_small.png", title = "@gorkang"), href="https://fosstodon.org/@gorkang", target = "_blank"), "&nbsp;&nbsp;", 
              # a(img(src = "https://cdn.buymeacoffee.com/buttons/bmc-new-btn-logo.svg", title = "Buy me a coffee", height = "26px"), href="https://www.buymeacoffee.com/I0rkAbM", target = "_blank"), "&nbsp;", 
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
          
          

  ## 2: professionals -----------------------------------------------------------------
        
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
  
  ## 3: patients -----------------------------------------------------------------
  
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
      
        
    
  # MAIN panel --------------------------------------------------------------

  mainPanel(width = 10, tabsetPanel(
    

  ## 1: plot -----------------------------------------------------------------

    
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
  
    ## 2: professionals -----------------------------------------------------------------
        tabPanel("Medical professionals", value = 2,
                 h4("Health professionals visual aid"),
                 br(),
                 plotOutput("outplot2", height = "800px", width = "100%"),
                 br(),
                 div(
                   HTML(
                     "<H4>Cancer</H4>",
                     "<U>Prevalence</U>: Giaquinto, A.N., Sung, H., Miller, K.D., Kramer, J.L., Newman, L.A., Minihan, A., Jemal, A. and Siegel, R.L. (2022), Breast Cancer Statistics, 2022. CA A Cancer J Clin, 72: 524-541. <a href='https://doi.org/10.3322/caac.21754'>doi: 10.3322/caac.21754</a><BR>",
                     "<U>Sensitivity and Specificity:</U> Influence of personal characteristics of individual women on sensitivity and specificity of mammography in the Million Women Study: cohort study. <a href='https://doi.org/10.1136/bmj.329.7464.477'>doi: 10.1136/bmj.329.7464.477</a>",
                     
                     "<H4>Stroke</H4>",
                     "<U>Prevalence</U>: Brunser, A. M., Cavada, G., Venturelli, P. M., Olavarría, V., Rojo, A., Almeida, J., Díaz, V., Hoppe, A., & Lavados, P. (2018). Diffusion-weighted imaging determinants for acute ischemic stroke diagnosis in the emergency room. Neuroradiology, 60(7), 687–692. <a href='https://pubmed.ncbi.nlm.nih.gov/29789895/'>pubmed: 29789895</a><BR>",
                     "<U>Sensitivity and Specificity</U>: Oppenheim et al. 2000 False-negative Diffusion-weighted MR Findings in Acute Ischemic Stroke. <a href='https://pubmed.ncbi.nlm.nih.gov/11003275/'>pubmed: 11003275</a>"),
                     style = "color:darkgrey; font-size: 0.9em !important;"
                   ),
                 ),

    ## 3: patients -----------------------------------------------------------------
        tabPanel("Patients", value = 3,
                 h4("Patient's visual aid"),
                 br(),
                 plotOutput("outplot3", height = "800px", width = "100%"),
                 br(),
                 div(
                   HTML(
                     "<H4>Cancer</H4>",
                     "<U>Prevalence</U>: Giaquinto, A.N., Sung, H., Miller, K.D., Kramer, J.L., Newman, L.A., Minihan, A., Jemal, A. and Siegel, R.L. (2022), Breast Cancer Statistics, 2022. CA A Cancer J Clin, 72: 524-541. <a href='https://doi.org/10.3322/caac.21754'>doi: 10.3322/caac.21754</a><BR>",
                     "<U>Sensitivity and Specificity:</U> Influence of personal characteristics of individual women on sensitivity and specificity of mammography in the Million Women Study: cohort study. <a href='https://doi.org/10.1136/bmj.329.7464.477'>doi: 10.1136/bmj.329.7464.477</a>",
                     
                     "<H4>Stroke</H4>",
                     "<U>Prevalence</U>: Brunser, A. M., Cavada, G., Venturelli, P. M., Olavarría, V., Rojo, A., Almeida, J., Díaz, V., Hoppe, A., & Lavados, P. (2018). Diffusion-weighted imaging determinants for acute ischemic stroke diagnosis in the emergency room. Neuroradiology, 60(7), 687–692. <a href='https://pubmed.ncbi.nlm.nih.gov/29789895/'>pubmed: 29789895</a><BR>",
                     "<U>Sensitivity and Specificity</U>: Oppenheim et al. 2000 False-negative Diffusion-weighted MR Findings in Acute Ischemic Stroke. <a href='https://pubmed.ncbi.nlm.nih.gov/11003275/'>pubmed: 11003275</a>"),
                   style = "color:darkgrey; font-size: 0.9em !important;"
                 ),
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
