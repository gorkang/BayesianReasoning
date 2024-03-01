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
    mutate(time_since_onset = as.factor(time_since_onset),
    ) |> 
    mutate(prevalence_1 = as.numeric(gsub(" in 1000", "", prevalence)),
           prevalence_2 = 1000,
           prevalence = prevalence_1/prevalence_2,
           sensitivity = 1 - as.numeric(false_negatives),
           specificity = specificity) |> 
    # PPV <- TRUE_positives / (TRUE_positives + FALSE_positives)
    mutate(PPV = (prevalence * sensitivity)/((prevalence * sensitivity) + ((1-prevalence) * (1-specificity)))) |> 
    # NPV <- TRUE_negatives / (TRUE_negatives + FALSE_negatives)
    mutate(NPV = ((1-prevalence) * specificity)/(((1-prevalence) * specificity) + (prevalence * (1-sensitivity)))) |> 
    mutate(time_since_onset = forcats::fct_reorder(time_since_onset, PPV))
  
  
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
    mutate(prevalence_1 = 1,
           prevalence_2 = as.numeric(gsub("1 in ", "", prevalence)),
           prevalence = prevalence_1/prevalence_2,
           sensitivity = sensitivity,
           specificity = specificity) |> 
    # PPV <- TRUE_positives / (TRUE_positives + FALSE_positives)
    mutate(PPV = (prevalence * sensitivity)/((prevalence * sensitivity) + ((1-prevalence) * (1-specificity)))) |> 
    # NPV <- TRUE_negatives / (TRUE_negatives + FALSE_negatives)
    mutate(NPV = ((1-prevalence) * specificity)/(((1-prevalence) * specificity) + (prevalence * (1-sensitivity))))
  
  
  
  return(TABLE)
  
}










# Function to capture messages with filename so we can automagically rename
capture_log1 <- function(f) {
  function(...) {
    logs <- list()
    add_log <- function(type, message) {
      new_l <- logs
      new_log <- list(timestamp = format(Sys.time(), tz = 'UTC', format = '%Y-%m-%d %H:%M:%S'),
                      type = type,
                      message =  message)
      new_l[[length(new_l) + 1]]  <- new_log
      logs <<- new_l
    }
    res <- withCallingHandlers(
      tryCatch(f(...), error=function(e) {
        add_log("error", conditionMessage(e))
        NULL
      }), warning=function(w) {
        add_log("warning", conditionMessage(w))
        invokeRestart("muffleWarning")
      }, message = function(m) {
        add_log("message", conditionMessage(m))
        invokeRestart("muffleMessage")
      })
    list(res, logs = logs)
  }
  
}


plot_E1 <- function(DF, PPV_NPV) {
  
  # cli::cli_h1("{names(DF)[1]} - {PPV_NPV}")
  # targets::tar_load_globals()
  # DF = tar_read("data_Stroke")
  # DF = tar_read("data_BC")
  # PPV_NPV = "NPV"
  

  # Specific parameters -----------------------------------------------------

  if (names(DF)[1] == "age") {
    
    item_label = "Valor Predictivo Positivo en mujeres de 40 años sin antecedentes"
    overlay_labels = "Mujer de 40 años"
    label_title = "Mamografía digital para detectar cáncer de mama"
    min_Prevalence = 1
    max_Prevalence = 1000
    
    DATA_plot = DF |> filter(age == 40)
    
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
    
    DATA_plot = DF |> filter(time_since_onset == "0-5")
    
    
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
    
    DATA_plot = tibble(prevalence = 1/20, 
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
  

# Parameters --------------------------------------------------------------

 
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
  
  # Plot --------------------------------------------------------------------

  
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
      Language = "sp")
  
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

