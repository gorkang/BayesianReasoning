#' Title
#'
#' @param PPV_melted 
#' @param PPV_NPV 
#' @param overlay_position_FP 
#'
#' @return
#' @export
#' @importFrom dplyr mutate filter pull
#' @importFrom magrittr %>%
#' 
#' @examples
.get_point_ppv_npv <- function(PPV_melted, PPV_NPV = "PPV", overlay_position_FP) {
    
  # BUG: THIS SHOULDN'T BE HERE. SHOULD READ FROM THE APPROPR
  # prevalence_label = " / "
  # decimals_x = 1
  if (exists("overlay_labels") == FALSE) { overlay_labels = ""}
  if (exists("prevalence_label") == FALSE) { prevalence_label = ""}
  if (exists("decimals_x") == FALSE) { decimals_x = 1}
  
  
    # Get PPV or NPV value ----------------------------------------------------
    
    if (PPV_NPV == "PPV")  {
      
      DF_point_PPV_NPV = PPV_melted %>%
        dplyr::filter(
          # Closest value to overlay_position_Prevalence & overlay_position_FP
          abs(Prevalence - point_Prevalence) == min(abs(Prevalence - point_Prevalence)) &
            abs(FP - overlay_position_FP) == min(abs(FP - overlay_position_FP)))
      
      Details_point_PPV_NPV = paste0(
        overlay_labels,
        "\n ", Min_Prevalence, " ", prevalence_label, " ", point_Prevalence,
        "\n ", overlay_prevalence_1, " ", prevalence_label, " ", overlay_prevalence_1,
        "\n FP = ", paste0(round(DF_point_PPV_NPV$FP, decimals_x), "%"),
        "\n PPV = ", paste0(round(DF_point_PPV_NPV$PPV, 2) * 100, "%"))
      
      point_PPV_NPV = DF_point_PPV_NPV %>% 
        dplyr::mutate(PPV = round(PPV * 100, 2)) %>%
        dplyr::pull(PPV)
      
    } else if (PPV_NPV == "NPV")  {
      
      DF_point_PPV_NPV = PPV_melted %>%
        dplyr::filter(
          # Closest value to overlay_position_Prevalence & overlay_position_FP
          abs(Prevalence - point_Prevalence) == min(abs(Prevalence - point_Prevalence)) &
            abs(FN - overlay_position_FP) == min(abs(FN - overlay_position_FP)))
      
      Details_point_PPV_NPV = paste0(
        overlay_labels,
        "\n ", Min_Prevalence, " ", prevalence_label, " ", point_Prevalence,
        "\n ", overlay_prevalence_1, " ", prevalence_label, " ", overlay_prevalence_2,
        "\n FN = ", paste0(round(DF_point_PPV_NPV$FN, decimals_x), "%"),
        "\n NPV = ", paste0(round(DF_point_PPV_NPV$NPV, 2) * 100, "%"))
      
      point_PPV_NPV = DF_point_PPV_NPV %>% 
        dplyr::mutate(NPV = round(NPV * 100, 2)) %>%
        dplyr::pull(NPV)
      
    } else {
      
      DF_point_PPV_NPV = PPV_melted %>%
        dplyr::filter(
          # Closest value to overlay_position_Prevalence & overlay_position_FP
          abs(Prevalence - point_Prevalence) == min(abs(Prevalence - point_Prevalence)) &
            abs(FP - overlay_position_FP) == min(abs(FP - overlay_position_FP)))
      
      Details_point_PPV_NPV = paste0(
        overlay_labels,
        "\n ", Min_Prevalence, " ", prevalence_label, " ", point_Prevalence,
        "\n ", overlay_prevalence_1, " ", prevalence_label, " ", overlay_prevalence_1,
        "\n FP = ", paste0(round(DF_point_PPV_NPV$FP, decimals_x), "%"),
        "\n PPV = ", paste0(round(DF_point_PPV_NPV$PPV, 2) * 100, "%"))
      
      point_PPV_NPV = DF_point_PPV_NPV %>% 
        dplyr::mutate(PPV = round(PPV * 100, 2)) %>% 
        dplyr::pull(PPV)
      
    }
  
  # Function outputs
  Details_point_PPV_NPV <<- Details_point_PPV_NPV
  point_PPV_NPV <<- point_PPV_NPV
  
}