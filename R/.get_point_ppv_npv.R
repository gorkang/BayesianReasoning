#' Get point ppv-npv
#'
#' @param PPV_melted 
#' @param PPV_NPV 
#' @param overlay_position_FP_FN 
#'
#' @return
#' @export
#' @importFrom magrittr "%>%"#'
#' @examples
.get_point_ppv_npv <- function(PPV_melted, PPV_NPV = "PPV", overlay_position_FP_FN) {
    
  # BUG: THIS SHOULDN'T BE HERE. SHOULD READ FROM THE APPROPR
  # prevalence_label = " / "
  # decimals_x = 1
  if (exists("overlay_labels") == FALSE) { overlay_labels = ""}
  if (exists("prevalence_label") == FALSE) { prevalence_label = ""}
  if (exists("decimals_x") == FALSE) { decimals_x = 1}
  
  
    # Get PPV or NPV value ----------------------------------------------------
    DF_point_PPV_NPV = PPV_melted %>%
      dplyr::filter(
        # Closest value to overlay_position_Prevalence & overlay_position_FP_FN
        abs(Prevalence - point_Prevalence) == min(abs(Prevalence - point_Prevalence)) &
          abs(FP - overlay_position_FP_FN) == min(abs(FP - overlay_position_FP_FN)))
    
      if (PPV_NPV == "NPV")  {
        
        calculated_NPV = paste0(round( ( (100 - overlay_position_FP_FN) * overlay_prevalence_2) / (((100 - overlay_position_FP_FN) * overlay_prevalence_2) + (overlay_prevalence_1) * Sensitivity), 2) * 100, "%")
        # NPV <<- round(((Prevalence - Min_Prevalence) * (100 - Max_FP)) / (((Prevalence - Min_Prevalence) * (100 - Max_FP)) + (Prevalence  %o% Sensitivity_range)), 2)
        
        Details_point_PPV_NPV = paste0(
          overlay_labels,
          # "\n ", Min_Prevalence, " ", prevalence_label, " ", point_Prevalence,
          "\n ", overlay_prevalence_1, " ", prevalence_label, " ", overlay_prevalence_2,
          "\n FN = ", paste0(overlay_position_FP_FN,
                             # round(DF_point_PPV_NPV$FN, decimals_x) # BUG
                             "%"),
          "\n NPV = ", calculated_NPV
          # paste0(round(DF_point_PPV_NPV$NPV, 2) * 100, "%")
          )
        
        point_PPV_NPV = DF_point_PPV_NPV %>% mutate(NPV = round(NPV * 100, 2))  %>% dplyr::pull(NPV)
        
      } else {
        
        calculated_PPV = paste0(
          round( 
            (Sensitivity * overlay_prevalence_1) / ((Sensitivity * overlay_prevalence_1) + (overlay_prevalence_2 - overlay_prevalence_1) * overlay_position_FP_FN),
            2) * 100, "%")
        
        Details_point_PPV_NPV = paste0(
          overlay_labels,
          # "\n ", Min_Prevalence, " ", prevalence_label, " ", point_Prevalence,
          "\n ", overlay_prevalence_1, " ", prevalence_label, " ", overlay_prevalence_2,
          "\n FP = ", paste0(round(DF_point_PPV_NPV$FP, decimals_x), "%"),
          "\n PPV = ", calculated_PPV
          # paste0(round(DF_point_PPV_NPV$PPV, 2) * 100, "%")
          )
        
        point_PPV_NPV = DF_point_PPV_NPV %>% dplyr::mutate(PPV = round(PPV * 100, 2))  %>% dplyr::pull(PPV)
        
      }
  
  # Function outputs
  Details_point_PPV_NPV <<- Details_point_PPV_NPV
  point_PPV_NPV <<- point_PPV_NPV
  
}