#' Preparation of the population description table
#'
#' @param cov_expo_data A data.frame containing covariates data
#' @param path A string defining path to save the file
#' @param file_name A string defining file name
#'
#' @return A .csv file containing the descriptive stats
#' @export
#' @import dplyr
#' @importFrom compareGroups compareGroups

PopulationCharacteristics <- function(cov_expo_data, 
                                      path, 
                                      file_name) {
  
  # Recode vars
  tab_data <- cov_expo_data %>%
    dplyr::mutate(
      child_sex = 
        dplyr::recode(as.factor(child_sex),
                      "1" = "Male",
                      "2" = "Female"),
      smoke_high_t2 = 
        dplyr::recode(as.factor(smoke_high),
                      "0" = "No",
                      "1" = "Yes"))
  
  res <- tab_data %>% 
    dplyr::select(mom_weight:smoke_high) %>% 
    compareGroups::compareGroups(data = .,
                                 include.miss = TRUE,
                                 method = c(mom_weight = 2,
                                            mom_height = 2,
                                            mom_age = 2,
                                            gestational_duration_weeks = 2)) %>%
    compareGroups::createTable(show.n = TRUE)
  
  # Save to file
  compareGroups::export2csv(res,
                            file = paste0(here::here(path, file_name), ".csv"))
  
  return(res)
}

#' Preparation of the description of exposure (phenols) levels table
#'
#' @param expo_data A data.frame containing exposure data
#' @param limits A numeric vector with LODs for each phenol
#'
#' @return A data.frame with exposure levels for each phenol
#' @export 
#' 
#' @import tidyr
#' @importFrom stringr str_detect
#' @import stats

ExposureCharacteristics <- function(expo_data, 
                                    limits) {
  
  # Select exposures of interest
  exp_data <- expo_data %>%
    dplyr::transmute(
      ident = as.character(ident), 
      
      # non-standardized exposures
      bpa_t2ns = mo_BPA_total_i_t2,
      bpa_t3ns = mo_BPA_total_i_t3,
      
      bps_t2ns = mo_BPS_total_string_t2,
      bps_t3ns = mo_BPS_total_string_t3,
      
      tcs_t2ns = mo_TRCS_total_i_t2,
      tcs_t3ns = mo_TRCS_total_i_t3,
      
      bp3_t2ns = mo_OXBE_total_i_t2,
      bp3_t3ns = mo_OXBE_total_i_t3,
      
      bupa_t2ns = mo_BUPA_total_string_t2,
      bupa_t3ns = mo_BUPA_total_string_t3,
      
      etpa_t2ns = mo_ETPA_total_i_t2,
      etpa_t3ns = mo_ETPA_total_i_t3,
      
      mepa_t2ns = mo_MEPA_total_i_t2,
      mepa_t3ns = mo_MEPA_total_i_t3,
      
      prpa_t2ns = mo_PRPA_total_i_t2,
      prpa_t3ns = mo_PRPA_total_i_t3,
      
      # standardized exposures
      bpa_t2s = mo_BPA_total_i_cor_t2,
      bpa_t3s = mo_BPA_total_i_cor_t3,
      
      bp3_t2s = mo_OXBE_total_i_cor_t2,
      bp3_t3s = mo_OXBE_total_i_cor_t3,
      
      etpa_t2s = mo_ETPA_total_i_cor_t2,
      etpa_t3s = mo_ETPA_total_i_cor_t3,
      
      mepa_t2s = mo_MEPA_total_i_cor_t2,
      mepa_t3s = mo_MEPA_total_i_cor_t3,
      
      prpa_t2s = mo_PRPA_total_i_cor_t2,
      prpa_t3s = mo_PRPA_total_i_cor_t3
    ) %>% 
    
    # Change "Non-detected" (ND) in the categorised exposures (BPS, BUPA) into 0
    dplyr::mutate_all(function(x) stringr::str_replace(x, 
                                                pattern = "ND", 
                                                replacement = "0")) %>% 
    dplyr::mutate_all(as.numeric) %>% 
    
    # Change non-detected records in the categorised exposures into values: LOD / sqrt(2)
    dplyr::mutate_at(c("bps_t2ns", "bps_t3ns"), function(x) ifelse(x == 0, 0.1 / sqrt(2), x)) %>%
    dplyr::mutate_at(c("bupa_t2ns", "bupa_t3ns"), function(x) ifelse(x == 0, 0.07 / sqrt(2), x))
  
  # set to long format
  exp_long <- exp_data %>%
    tidyr::pivot_longer(
      cols = -ident,
      names_to = "exp",
      values_to = "val"
    ) %>%
    
    # set compound and add LOD
    dplyr::mutate(compound = dplyr::case_when(
      stringr::str_detect(exp, "bpa") ~ "BPA_total",
      stringr::str_detect(exp, "bps") ~ "BPS_total",
      stringr::str_detect(exp, "tcs") ~ "TRCS_total",
      stringr::str_detect(exp, "bp3") ~ "BP3_total",
      stringr::str_detect(exp, "bupa") ~ "BUPA_total",
      stringr::str_detect(exp, "etpa") ~ "ETPA_total",
      stringr::str_detect(exp, "mepa") ~ "MEPA_total",
      stringr::str_detect(exp, "prpa") ~ "PRPA_total"
    )) %>%
    dplyr::left_join(limits, by = "compound")
  
  # descriptive exp table for continuous exposures
  exp_tab <- exp_long %>%
    dplyr::group_by(exp, compound) %>%
    dplyr::summarise(n = sum(!is.na(val)),
                     pct_det = (sum(val >= LOD, na.rm = TRUE) / n) * 100,
                     p5 = stats::quantile(val, 0.05, na.rm = TRUE),
                     median = stats::quantile(val, 0.5, na.rm = TRUE),
                     p95 = stats::quantile(val, 0.95, na.rm = TRUE)) %>% 
    dplyr::left_join(dplyr::select(limits, compound, LOD), by = "compound") %>%
    dplyr::rowwise() %>% 
    dplyr::mutate(p5 = ifelse(p5 < LOD, NaN, p5),
                  median = ifelse(median < LOD, NaN, median)) %>% 
    dplyr::mutate_at(dplyr::vars(pct_det:p95), round, 1) %>% 
    dplyr::select(-compound)
  
  
  # wide on period
  exp_tab_final <- exp_tab %>%
    tidyr::separate(exp, into = c("compound", "period"), sep = "_") %>%
    tidyr::pivot_wider(
      id_cols = c("compound", "period", "LOD"),
      names_from = "period",
      values_from = c("n", "pct_det", "p5", "median", "p95")
    ) %>%
    dplyr::select(compound, 
                  LOD, 
                  dplyr::contains("t2ns"), 
                  dplyr::contains("t3ns"), 
                  dplyr::contains("t2s"), 
                  dplyr::contains("t3s")) %>% # reorder cols
    dplyr::mutate(compound = factor(compound, 
                                    levels = c("bpa",
                                               "bps", 
                                               "bpscat", 
                                               "bp3", 
                                               "tcs", 
                                               "bupa", 
                                               "bupacat", 
                                               "etpa", 
                                               "mepa", 
                                               "prpa"))) %>% # reorder rows
    dplyr::arrange(compound) 
  
  return(exp_tab_final)
}

#' Preparation of the description of exposure (phthalates) levels table
#'
#' @param expo_data A data.frame containing exposure data
#' @param limits A numeric vector with LODs for each phenol
#'
#' @return A data.frame with exposure levels for each phenol
#' @export 
#' 
#' @import tidyr
#' @importFrom stringr str_detect
#' @import stats
#' 
ExposureCharacteristicsPhthalates <- function(expo_data, 
                                              limits) {
  
  # Select exposures of interest
  exp_data <- expo_data %>%
    dplyr::transmute(
      ident = as.character(ident), 
      
      # non-standardized exposures
      MBzP_t2ns = mo_MBzP_i_t2,
      MBzP_t3ns = mo_MBzP_i_t3,
      MEP_t2ns = mo_MEP_i_t2,
      MEP_t3ns = mo_MEP_i_t3,
      MiBP_t2ns = mo_MiBP_i_t2,
      MiBP_t3ns = mo_MiBP_i_t3,
      MnBP_t2ns = mo_MnBP_i_t2,
      MnBP_t3ns = mo_MnBP_i_t3,
      
      ohMPHP_t2ns = mo_ohMPHP_i_t2,
      ohMPHP_t3ns = mo_ohMPHP_i_t3,
      
      # MECPP_t2ns = mo_MECPP_i_t2,
      # MECPP_t3ns = mo_MECPP_i_t3,
      # MEHHP_t2ns = mo_MEHHP_i_t2,
      # MEHHP_t3ns = mo_MEHHP_i_t3,
      # MEHP_t2ns = mo_MEHP_i_t2,
      # MEHP_t3ns = mo_MEHP_i_t3,
      # MEOHP_t2ns = mo_MEOHP_i_t2,
      # MEOHP_t3ns = mo_MEOHP_i_t3,
      # MMCHP_t2ns = mo_MMCHP_i_t2,
      # MMCHP_t3ns = mo_MMCHP_i_t3,
      DEHP_t2ns = mo_DEHP_ms_i_t2,
      DEHP_t3ns = mo_DEHP_ms_i_t3,
      
      # cxMiNP_t2ns = mo_cxMiNP_i_t2,
      # cxMiNP_t3ns = mo_cxMiNP_i_t3,
      # ohMiNP_t2ns = mo_ohMiNP_i_t2,
      # ohMiNP_t3ns = mo_ohMiNP_i_t3,
      # oxoMiNP_t2ns = mo_oxoMiNP_i_t2,
      # oxoMiNP_t3ns = mo_oxoMiNP_i_t3,
      DiNP_t2ns = mo_DiNP_ms_i_t2,
      DiNP_t3ns = mo_DiNP_ms_i_t3,
      
      # ohMINCH_t2ns = mo_ohMINCH_i_t2,
      # ohMINCH_t3ns = mo_ohMINCH_i_t3,
      # oxoMINCH_t2ns = mo_oxoMINCH_i_t2,
      # oxoMINCH_t3ns = mo_oxoMINCH_i_t3,
      DINCH_t2ns = mo_DINCH_ms_i_t2,
      DINCH_t3ns = mo_DINCH_ms_i_t3,
      
      # standardized exposures
      MBzP_t2s = mo_MBzP_i_cor_t2, 
      MBzP_t3s = mo_MBzP_i_cor_t3,
      MEP_t2s = mo_MEP_i_cor_t2, 
      MEP_t3s = mo_MEP_i_cor_t3, 
      MiBP_t2s = mo_MiBP_i_cor_t2, 
      MiBP_t3s = mo_MiBP_i_cor_t3, 
      MnBP_t2s = mo_MnBP_i_cor_t2,
      MnBP_t3s = mo_MnBP_i_cor_t3,
      
      ohMPHP_t2s = mo_ohMPHP_i_cor_t2,
      ohMPHP_t3s = mo_ohMPHP_i_cor_t3,
      
      # MECPP_t2s = mo_MECPP_i_cor_t2, 
      # MECPP_t3s = mo_MECPP_i_cor_t3,
      # MEHHP_t2s = mo_MEHHP_i_cor_t2,
      # MEHHP_t3s = mo_MEHHP_i_cor_t3,
      # MEHP_t2s = mo_MEHP_i_cor_t2,
      # MEHP_t3s = mo_MEHP_i_cor_t3,
      # MEOHP_t2s = mo_MEOHP_i_cor_t2,
      # MEOHP_t3s = mo_MEOHP_i_cor_t3,
      # MMCHP_t2s = mo_MMCHP_i_cor_t2, 
      # MMCHP_t3s = mo_MMCHP_i_cor_t3, 
      DEHP_t2s = mo_DEHP_ms_i_cor_t2,
      DEHP_t3s = mo_DEHP_ms_i_cor_t3,
      
      # cxMiNP_t2s = mo_cxMiNP_i_cor_t2,
      # cxMiNP_t3s = mo_cxMiNP_i_cor_t3,
      # ohMiNP_t2s = mo_ohMiNP_i_cor_t2,
      # ohMiNP_t3s = mo_ohMiNP_i_cor_t3,
      # oxoMiNP_t2s = mo_oxoMiNP_i_cor_t2, 
      # oxoMiNP_t3s = mo_oxoMiNP_i_cor_t3,
      DiNP_t2s = mo_DiNP_ms_i_cor_t2,
      DiNP_t3s = mo_DiNP_ms_i_cor_t3,
      
      # ohMINCH_t2s = mo_ohMINCH_i_cor_t2,
      # ohMINCH_t3s = mo_ohMINCH_i_cor_t3,
      # oxoMINCH_t2s = mo_oxoMINCH_i_cor_t2,
      # oxoMINCH_t3s = mo_oxoMINCH_i_cor_t3,
      DINCH_t2s = mo_DINCH_ms_i_cor_t2,
      DINCH_t3s = mo_DINCH_ms_i_cor_t3
    ) %>% 
    
    dplyr::mutate_all(as.numeric)
  
  # set to long format
  exp_long <- exp_data %>%
    tidyr::pivot_longer(
      cols = -ident,
      names_to = "exp",
      values_to = "val"
    ) %>%
    
    # set compound and add LOD
    dplyr::mutate(compound = dplyr::case_when(
      stringr::str_detect(exp, "MBzP") ~ "MBzP",
      stringr::str_detect(exp, "MEP") ~ "MEP",
      stringr::str_detect(exp, "MiBP") ~ "MiBP",
      stringr::str_detect(exp, "MnBP") ~ "MnBP",
      
      stringr::str_detect(exp, "ohMPHP") ~ "ohMPHP",
      
      # stringr::str_detect(exp, "MECPP") ~ "MECPP",
      # stringr::str_detect(exp, "MEHP") ~ "MEHP",
      # stringr::str_detect(exp, "MEHHP") ~ "MEHHP",
      # stringr::str_detect(exp, "MEOHP") ~ "MEOHP",
      # stringr::str_detect(exp, "MMCHP") ~ "MMCHP",
      stringr::str_detect(exp, "DEHP") ~ "DEHP",
      
      # stringr::str_detect(exp, "cxMiNP") ~ "cxMiNP",
      # stringr::str_detect(exp, "ohMiNP") ~ "ohMiNP",
      # stringr::str_detect(exp, "oxoMiNP") ~ "oxoMiNP",
      stringr::str_detect(exp, "DiNP") ~ "DiNP",
      
      # stringr::str_detect(exp, "ohMINCH") ~ "ohMINCH",
      # stringr::str_detect(exp, "oxoMINCH") ~ "oxoMINCH",
      stringr::str_detect(exp, "DINCH") ~ "DINCH")) %>% 
  
  dplyr::left_join(limits, by = "compound")
  
  # descriptive exp table for continuous exposures
  exp_tab <- exp_long %>%
    dplyr::group_by(exp, compound) %>%
    dplyr::summarise(n = sum(!is.na(val)),
                     pct_det = (sum(val >= LOD, na.rm = TRUE) / n) * 100,
                     p5 = stats::quantile(val, 0.05, na.rm = TRUE),
                     median = stats::quantile(val, 0.5, na.rm = TRUE),
                     p95 = stats::quantile(val, 0.95, na.rm = TRUE)) %>% 
    dplyr::left_join(dplyr::select(limits, compound, LOD), by = "compound") %>%
    dplyr::rowwise() %>% 
    dplyr::mutate(p5 = ifelse(p5 < LOD, NaN, p5),
                  median = ifelse(median < LOD, NaN, median)) %>% 
    dplyr::mutate_at(dplyr::vars(pct_det:p95), round, 1) %>% 
    dplyr::select(-compound)
  
  
  # wide on period
  exp_tab_final <- exp_tab %>%
    tidyr::separate(exp, into = c("compound", "period"), sep = "_") %>%
    tidyr::pivot_wider(
      id_cols = c("compound", "period", "LOD"),
      names_from = "period",
      values_from = c("n", "pct_det", "p5", "median", "p95")
    ) %>%
    dplyr::select(compound, 
                  LOD, 
                  dplyr::contains("t2ns"), 
                  dplyr::contains("t3ns"), 
                  dplyr::contains("t2s"), 
                  dplyr::contains("t3s")) %>% # reorder cols
    dplyr::mutate(compound = factor(compound, levels = c("MBzP", "MEP", "MiBP", "MnBP", 
                                                         "ohMPHP",
                                                         # "MECPP", "MEHHP", "MEHP", "MEOHP", "MMCHP", 
                                                         "DEHP",
                                                         # "cxMiNP", "ohMiNP", "oxoMiNP", 
                                                         "DiNP",
                                                         # "ohMINCH", "oxoMINCH", 
                                                         "DINCH"))) %>%  # reorder rows
    dplyr::arrange(compound) 
  
  return(exp_tab_final)
}

