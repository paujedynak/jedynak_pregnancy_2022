#' Prepare data from the bdd grossesse (population characteristics)
#'
#' @param covariates_data A .dta file containing bdd_grossesse_v4 data
#' @param expo_data A .csv file containing phenol exposure data
#'
#' @return A data.frame containing covariates data
#' @export
#' @import dplyr
#' @import tidyr
#' @import stringr str_detect

CreatePopData <- function(covariates_data, 
                          expo_data) {
  
  # Note for deleted ID 18147, email sarah 18/06/2020:
  # >"cette femme a eu deux grossesses et la grossesse Sepages n'a pas aboutit
  # elle a eu un 2eme enfant ensuite et a accouché en décembre 2016 (mais cet
  # enfant n'est pas un enfant Sepages).  Quand nosu sommes allées dans les
  # maternités récupérer les poids de naissance / taille manquants, l'equipe
  # Sepages a du récupérer les info de cette deuxième grossesse... 
  # >Donc Matthieu : supprime cette volontaire de ton analyse."
  
  id_list <- expo_data %>% 
    dplyr::pull(ident) %>% 
    unique() %>% 
    sort()
  
  # create population characteristics data
  pop_data <- covariates_data %>%
    dplyr::filter(ident %in% id_list,
                  ident != 18147) %>% # see comment above
    dplyr::mutate(
      ident = as.character(ident),
      mom_weight = mo_we_bepr,
      mom_height = mo_he,
      mom_age = mo_age,
      mom_edu = 
        dplyr::case_when(
          q21 %in% c(1, 2, 3, 4, 5, 6) ~ "Low",
          q21 == 7 ~ "Middle",
          q21 == 8 ~ "High",
          TRUE ~ NA_character_
        ),
      mom_edu = factor(mom_edu,
                       levels = c("Low", "Middle", "High")),
      
      parity = case_when(
        mo_par == 0 ~ "First", 
        mo_par == 1 ~ "Second",
        mo_par == 2 ~ "Third_and_more"),
      parity = factor(parity),
      child_sex = ifelse(ch_sex == 1, "Male", "Female"),
      child_sex = factor(child_sex),
      echo_T1_1 = ddg_echo_T1_1,
      echo_T1_2 = ddg_echo_T1_2,
      echo_T1_3 = ddg_echo_T1_3,
      date_lmp = po_datelmp,
      date_delivery = po_datedel,
      .keep = "none") %>% 
    
    dplyr::rowwise() %>% 
    dplyr::mutate(mean_ddg_echo = mean.Date(c(echo_T1_1, echo_T1_2, echo_T1_3), na.rm = TRUE),
                  .keep = "unused") %>% 
    
    dplyr::mutate(diff = as.numeric(mean_ddg_echo - (date_lmp + 14)),
                  
                  # Based on the date of the LMP or gestational duration assessed by the obstetrician if it differed 
                  # from the LMP-based estimate by more than 2 weeks   
                  corr_lmp = dplyr::case_when(abs(diff) > 14 ~ mean_ddg_echo,
                                              TRUE ~ date_lmp), # one woman 17668 had the difference of >14 days
                  gestational_duration = date_delivery - corr_lmp,
                  gestational_duration = ifelse(ident == 18147, NA, gestational_duration), # see comment above
                  gestational_duration_weeks = gestational_duration / 7)
  
  # correct missing parity
  pop_data$parity[pop_data$ident == 17095] <- "First"
  
  # correct missing child sex
  pop_data$child_sex[pop_data$ident == 25015] <- "Male"
  
  # correct erroneous child sex
  pop_data$child_sex[pop_data$ident == 17617] <- "Male"
  
  pop_data <- dplyr::select(pop_data, -c(date_lmp:corr_lmp))
  
  return(pop_data)
}

#' Prepare smoking data from Ariane's dataset
#'
#' @param tobacco_data A data.frame containing smoking data 
#'
#' @return A data.frame containing smoking covariates
#' @export

CreateSmokeData <- function(tobacco_data) {
  
  # rename vars and select the one for model
  # Correction of the variables	made by Ariane and implemented in the new 
  # tobaccopregnancy_ag_20201218_16.dta replacing the previous smoking used by Matthieu
  # mo_tob_grt3 -> mo_tob_grt3_n2 
  # mo_tob_grt3_yn -> mo_tob_grt3_yn_n2 
  # mo_tob_grt3_yn1 -> mo_tob_grt3_yn1_n2
  # Details of the change to be found in the readme file
  # C:\Users\pauli\Documents\WORK\SEPAGES\phenols_growth\data\raw_data\tobaccopregnancy_ag_20201218_16\
  
  smoke_data <- tobacco_data %>%
    dplyr::filter(ident != 18147) %>%
    dplyr::transmute(
      ident = as.character(ident),
      smoke_pre_detection = mo_tob_grstt1,
      smoke_n_cig_t1 = mo_tob_grt1,
      smoke_t1 = mo_tob_grt1_yn,
      smoke_high_t1 = mo_tob_grt1_yn1,
      smoke_n_cig_t2 = mo_tob_grt2,
      smoke_t2 = mo_tob_grt2_yn,
      smoke_high_t2 = mo_tob_grt2_yn1,
      smoke_n_cig_t3 = mo_tob_grt3_n2, # replacing mo_tob_grt3
      smoke_t3 = mo_tob_grt3_yn_n2, # replacing mo_tob_grt3_yn
      smoke_high_t3 = mo_tob_grt3_yn1_n2 # replacing mo_tob_grt3_yn1
    ) %>%
    dplyr::select(ident, smoke_high_t2) %>% 
    dplyr::rename(smoke_high = smoke_high_t2) %>% 
    dplyr::mutate(smoke_high = factor(smoke_high,
                                      levels = c("0", "1"),
                                      labels = c("No", "Yes")))
  
  return(smoke_data)
}

#' Prepare birth outcomes data
#'
#' @param covariates_data A .dta file containing bdd_grossesse_v4 data
#' @param pop_data A data.frame containing covariates data
#'
#' @return A data.frame containing birth outcomes data
#' @export

CreateBirthOutcomes <- function(covariates_data,
                                pop_data) {
  
  # Note for deleted ID 18147, email sarah 18/06/2020:
  # >"cette femme a eu deux grossesses et la grossesse Sepages n'a pas aboutit
  # elle a eu un 2eme enfant ensuite et a accouché en décembre 2016 (mais cet
  # enfant n'est pas un enfant Sepages).  Quand nosu sommes allées dans les
  # maternités récupérer les poids de naissance / taille manquants, l'equipe
  # Sepages a du récupérer les info de cette deuxième grossesse... 
  # >Donc Matthieu : supprime cette volontaire de ton analyse."
  
  # rename vars and set to long format
  birth_outcomes <- covariates_data %>%
    dplyr::filter(ident != 18147) %>% # see comment above
    dplyr::transmute(ident = as.character(ident), 
                     birth_weight = po_w / 1000,
                     birth_length = po_he,
                     head_circumference = po_hc * 10) %>% 
    dplyr::right_join(dplyr::select(pop_data, ident, gestational_duration), by = "ident") %>%
    dplyr::rename(gestational_age = gestational_duration) %>%
    tidyr::pivot_longer(cols = -c(ident, gestational_age),
                        names_to = "outcome",
                        values_to = "outcome_val") %>%
    dplyr::mutate(trimester = "birth")
  
  # impute missing GA
  birth_outcomes$gestational_age[is.na(birth_outcomes$gestational_age)] <- median(birth_outcomes$gestational_age, na.rm = TRUE)
  
  return(birth_outcomes)
}

#' Create exposure data (phenols)
#'
#' @param expo_data A .csv file containing phenol exposure data
#'
#' @return A data.frame containing phenol exposure data
#' @export 

CreateExpoData <- function(expo_data) {
  
  # select analysis exposures and log trans exposures
  exposures <- expo_data %>%
    dplyr::transmute(
      ident = as.character(ident),
      log_BPA_t2 = log(mo_BPA_total_i_cor_t2), 
      log_BPA_t3 = log(mo_BPA_total_i_cor_t3), 
      log_TCS_t2 = log(mo_TRCS_total_i_cor_t2), 
      log_TCS_t3 = log(mo_TRCS_total_i_cor_t3), 
      cat_BPS_t2 = mo_BPS_total_cat_t2,
      cat_BPS_t3 = mo_BPS_total_cat_t3,
      log_BP3_t2 = log(mo_OXBE_total_i_cor_t2), 
      log_BP3_t3 = log(mo_OXBE_total_i_cor_t3),
      cat_BUPA_t2 = mo_BUPA_total_cat_t2,
      cat_BUPA_t3 = mo_BUPA_total_cat_t3,
      log_ETPA_t2 = log(mo_ETPA_total_i_cor_t2),
      log_ETPA_t3 = log(mo_ETPA_total_i_cor_t3),
      log_MEPA_t2 = log(mo_MEPA_total_i_cor_t2),
      log_MEPA_t3 = log(mo_MEPA_total_i_cor_t3),
      log_PRPA_t2 = log(mo_PRPA_total_i_cor_t2),
      log_PRPA_t3 = log(mo_PRPA_total_i_cor_t3),
      
      # not ln-transformed exposures
      BPA_t2 = mo_BPA_total_i_cor_t2, 
      BPA_t3 = mo_BPA_total_i_cor_t3, 
      TCS_t2 = mo_TRCS_total_i_cor_t2, 
      TCS_t3 = mo_TRCS_total_i_cor_t3, 
      BP3_t2 = mo_OXBE_total_i_cor_t2, 
      BP3_t3 = mo_OXBE_total_i_cor_t3,
      ETPA_t2 = mo_ETPA_total_i_cor_t2,
      ETPA_t3 = mo_ETPA_total_i_cor_t3,
      MEPA_t2 = mo_MEPA_total_i_cor_t2,
      MEPA_t3 = mo_MEPA_total_i_cor_t3,
      PRPA_t2 = mo_PRPA_total_i_cor_t2,
      PRPA_t3 = mo_PRPA_total_i_cor_t3,
      
      # add urine collection dates
      urine_date_T2 = mo_urine_sample_date_t2,
      urine_date_T3 = mo_urine_sample_date_t3) %>%
    
    # add mean value of exposure for T2 and T3
    dplyr::rowwise() %>%
    dplyr::mutate(log_BPA_both = mean(c(log_BPA_t2, log_BPA_t3), na.rm = TRUE),
                  log_TCS_both = mean(c(log_TCS_t2, log_TCS_t3), na.rm = TRUE),
                  log_BP3_both = mean(c(log_BP3_t2, log_BP3_t3), na.rm = TRUE),
                  log_ETPA_both = mean(c(log_ETPA_t2, log_ETPA_t3), na.rm = TRUE),
                  log_MEPA_both = mean(c(log_MEPA_t2, log_MEPA_t3), na.rm = TRUE),
                  log_PRPA_both = mean(c(log_PRPA_t2, log_PRPA_t3), na.rm = TRUE),
                  cat_BPS_both = case_when((cat_BPS_t2 == 1 | is.na(cat_BPS_t2)) & (cat_BPS_t3 == 1 | is.na(cat_BPS_t3)) ~ 1,
                                           (cat_BPS_t2 != 1 & (cat_BPS_t3 == 1 | is.na(cat_BPS_t3))) | (cat_BPS_t3 != 1 & (cat_BPS_t2 == 1 | is.na(cat_BPS_t2))) ~ 2,
                                           TRUE ~ 3),
                  cat_BUPA_both = case_when((cat_BUPA_t2 == 1 | is.na(cat_BUPA_t2)) & (cat_BUPA_t3 == 1 | is.na(cat_BUPA_t3)) ~ 1,
                                            (cat_BUPA_t2 != 1 & (cat_BUPA_t3 == 1 | is.na(cat_BUPA_t3))) | (cat_BUPA_t3 != 1 & (cat_BUPA_t2 == 1 | is.na(cat_BUPA_t2))) ~ 2,
                                            TRUE ~ 3)) %>% 
    
    # change categorized exposures to factors
    dplyr::mutate(dplyr::across(dplyr::contains("cat"), ~ factor(.x, levels = c("1", "2", "3"), 
                                                                 labels = c("<LOD", "LOD-LOQ", ">LOQ"))))
  
  # Non standardized exposures
  exposures_non_sd <- expo_data %>%
    dplyr::transmute(
      ident = as.character(ident),
      log_BPA_nosd_t2 = log(mo_BPA_total_i_t2), 
      log_BPA_nosd_t3 = log(mo_BPA_total_i_t3),
      log_BP3_nosd_t2 = log(mo_OXBE_total_i_t2), 
      log_BP3_nosd_t3 = log(mo_OXBE_total_i_t3),
      log_ETPA_nosd_t2 = log(mo_ETPA_total_i_t2),
      log_ETPA_nosd_t3 = log(mo_ETPA_total_i_t3),
      log_MEPA_nosd_t2 = log(mo_MEPA_total_i_t2),
      log_MEPA_nosd_t3 = log(mo_MEPA_total_i_t3),
      log_PRPA_nosd_t2 = log(mo_PRPA_total_i_t2),
      log_PRPA_nosd_t3 = log(mo_PRPA_total_i_t3)) %>%
    
    # add mean value of exposure for T2 and T3
    rowwise() %>%
    dplyr::mutate(log_BPA_nosd_both = mean(c(log_BPA_nosd_t2, log_BPA_nosd_t3), na.rm = TRUE),
                  log_BP3_nosd_both = mean(c(log_BP3_nosd_t2, log_BP3_nosd_t3), na.rm = TRUE),
                  log_ETPA_nosd_both = mean(c(log_ETPA_nosd_t2, log_ETPA_nosd_t3), na.rm = TRUE),
                  log_MEPA_nosd_both = mean(c(log_MEPA_nosd_t2, log_MEPA_nosd_t3), na.rm = TRUE),
                  log_PRPA_nosd_both = mean(c(log_PRPA_nosd_t2, log_PRPA_nosd_t3), na.rm = TRUE)) %>% 
    
    # Remove T3 exposures
    dplyr::select(ident, contains("t2"), contains("both"))
  
  # add non-standardized exposures to model data
  expo_data <- exposures %>%
    dplyr::left_join(exposures_non_sd, by = "ident") %>% 
    dplyr::arrange(ident) %>%
    dplyr::filter(ident != 18147)
}

#' Create exposure data (phthalates)
#'
#' @param expo_data A .csv file containing phenol exposure data
#'
#' @return A data.frame containing phenol exposure data
#' @export 

CreateExpoDataPhthalates <- function(expo_data) {
  
  # select analysis exposures and log trans exposures
  exposures <- expo_data %>%
    dplyr::transmute(
      ident = as.character(ident),
      
      log_MBzP_t2 = log(mo_MBzP_i_cor_t2), 
      log_MBzP_t3 = log(mo_MBzP_i_cor_t3),
      log_MEP_t2 = log(mo_MEP_i_cor_t2), 
      log_MEP_t3 = log(mo_MEP_i_cor_t3), 
      log_MiBP_t2 = log(mo_MiBP_i_cor_t2), 
      log_MiBP_t3 = log(mo_MiBP_i_cor_t3), 
      log_MnBP_t2 = log(mo_MnBP_i_cor_t2),
      log_MnBP_t3 = log(mo_MnBP_i_cor_t3),
      
      log_ohMPHP_t2 = log(mo_ohMPHP_i_cor_t2),
      log_ohMPHP_t3 = log(mo_ohMPHP_i_cor_t3),
      
      log_MECPP_t2 = log(mo_MECPP_i_cor_t2),
      log_MECPP_t3 = log(mo_MECPP_i_cor_t3),
      log_MEHHP_t2 = log(mo_MEHHP_i_cor_t2),
      log_MEHHP_t3 = log(mo_MEHHP_i_cor_t3),
      log_MEHP_t2 = log(mo_MEHP_i_cor_t2),
      log_MEHP_t3 = log(mo_MEHP_i_cor_t3),
      log_MEOHP_t2 = log(mo_MEOHP_i_cor_t2),
      log_MEOHP_t3 = log(mo_MEOHP_i_cor_t3),
      log_MMCHP_t2 = log(mo_MMCHP_i_cor_t2),
      log_MMCHP_t3 = log(mo_MMCHP_i_cor_t3),
      log_DEHP_t2 = log(mo_DEHP_ms_i_cor_t2),
      log_DEHP_t3 = log(mo_DEHP_ms_i_cor_t3),
      
      log_cxMiNP_t2 = log(mo_cxMiNP_i_cor_t2),
      log_cxMiNP_t3 = log(mo_cxMiNP_i_cor_t3),
      log_ohMiNP_t2 = log(mo_ohMiNP_i_cor_t2),
      log_ohMiNP_t3 = log(mo_ohMiNP_i_cor_t3),
      log_oxoMiNP_t2 = log(mo_oxoMiNP_i_cor_t2),
      log_oxoMiNP_t3 = log(mo_oxoMiNP_i_cor_t3),
      log_DiNP_t2 = log(mo_DiNP_ms_i_cor_t2),
      log_DiNP_t3 = log(mo_DiNP_ms_i_cor_t3),
      
      log_ohMINCH_t2 = log(mo_ohMINCH_i_cor_t2),
      log_ohMINCH_t3 = log(mo_ohMINCH_i_cor_t3),
      log_oxoMINCH_t2 = log(mo_oxoMINCH_i_cor_t2),
      log_oxoMINCH_t3 = log(mo_oxoMINCH_i_cor_t3),
      log_DINCH_t2 = log(mo_DINCH_ms_i_cor_t2),
      log_DINCH_t3 = log(mo_DINCH_ms_i_cor_t3),
      
      # not ln-transformed exposures
      
      MBzP_t2 = mo_MBzP_i_cor_t2, 
      MBzP_t3 = mo_MBzP_i_cor_t3,
      MEP_t2 = mo_MEP_i_cor_t2, 
      MEP_t3 = mo_MEP_i_cor_t3, 
      MiBP_t2 = mo_MiBP_i_cor_t2, 
      MiBP_t3 = mo_MiBP_i_cor_t3, 
      MnBP_t2 = mo_MnBP_i_cor_t2,
      MnBP_t3 = mo_MnBP_i_cor_t3,
      
      ohMPHP_t2 = mo_ohMPHP_i_cor_t2,
      ohMPHP_t3 = mo_ohMPHP_i_cor_t3,
      
      # MECPP_t2 = mo_MECPP_i_cor_t2, 
      # MECPP_t3 = mo_MECPP_i_cor_t3,
      # MEHHP_t2 = mo_MEHHP_i_cor_t2,
      # MEHHP_t3 = mo_MEHHP_i_cor_t3,
      # MEHP_t2 = mo_MEHP_i_cor_t2,
      # MEHP_t3 = mo_MEHP_i_cor_t3,
      # MEOHP_t2 = mo_MEOHP_i_cor_t2,
      # MEOHP_t3 = mo_MEOHP_i_cor_t3,
      # MMCHP_t2 = mo_MMCHP_i_cor_t2, 
      # MMCHP_t3 = mo_MMCHP_i_cor_t3, 
      DEHP_t2 = mo_DEHP_ms_i_cor_t2,
      DEHP_t3 = mo_DEHP_ms_i_cor_t3,
      
      # cxMiNP_t2 = mo_cxMiNP_i_cor_t2,
      # cxMiNP_t3 = mo_cxMiNP_i_cor_t3,
      # ohMiNP_t2 = mo_ohMiNP_i_cor_t2,
      # ohMiNP_t3 = mo_ohMiNP_i_cor_t3,
      # oxoMiNP_t2 = mo_oxoMiNP_i_cor_t2, 
      # oxoMiNP_t3 = mo_oxoMiNP_i_cor_t3,
      DiNP_t2 = mo_DiNP_ms_i_cor_t2,
      DiNP_t3 = mo_DiNP_ms_i_cor_t3,
      
      # ohMINCH_t2 = mo_ohMINCH_i_cor_t2,
      # ohMINCH_t3 = mo_ohMINCH_i_cor_t3,
      # oxoMINCH_t2 = mo_oxoMINCH_i_cor_t2,
      # oxoMINCH_t3 = mo_oxoMINCH_i_cor_t3,
      DINCH_t2 = mo_DINCH_ms_i_cor_t2,
      DINCH_t3 = mo_DINCH_ms_i_cor_t3,
      
      # add urine collection dates
      urine_date_T2 = mo_urine_sample_date_t2,
      urine_date_T3 = mo_urine_sample_date_t3) %>%
    
    # add mean value of exposure for T2 and T3
    dplyr::rowwise() %>%
    dplyr::mutate(log_MBzP_both = mean(c(log_MBzP_t2, log_MBzP_t3), na.rm = TRUE),
                  log_MEP_both = mean(c(log_MEP_t2, log_MEP_t3), na.rm = TRUE),
                  log_MiBP_both = mean(c(log_MiBP_t2, log_MiBP_t3), na.rm = TRUE),
                  log_MnBP_both = mean(c(log_MnBP_t2, log_MnBP_t3), na.rm = TRUE),
                  
                  log_ohMPHP_both = mean(c(log_ohMPHP_t2, log_ohMPHP_t3), na.rm = TRUE),
                  
                  # log_MECPP_both = mean(c(log_MECPP_t2, log_MECPP_t3), na.rm = TRUE),
                  # log_MEHP_both = mean(c(log_MEHP_t2, log_MEHP_t3), na.rm = TRUE),
                  # log_MEHHP_both = mean(c(log_MEHHP_t2, log_MEHHP_t3), na.rm = TRUE),
                  # log_MEOHP_both = mean(c(log_MEOHP_t2, log_MEOHP_t3), na.rm = TRUE),
                  # log_MMCHP_both = mean(c(log_MMCHP_t2, log_MMCHP_t3), na.rm = TRUE),
                  log_DEHP_both = mean(c(log_DEHP_t2, log_DEHP_t3), na.rm = TRUE),
                  
                  # log_cxMiNP_both = mean(c(log_cxMiNP_t2, log_cxMiNP_t3), na.rm = TRUE),
                  # log_ohMiNP_both = mean(c(log_ohMiNP_t2, log_ohMiNP_t3), na.rm = TRUE),
                  # log_oxoMiNP_both = mean(c(log_oxoMiNP_t2, log_oxoMiNP_t3), na.rm = TRUE),
                  log_DiNP_both = mean(c(log_DiNP_t2, log_DiNP_t3), na.rm = TRUE),
                  
                  # log_ohMINCH_both = mean(c(log_ohMINCH_t2, log_ohMINCH_t3), na.rm = TRUE),
                  # log_oxoMINCH_both = mean(c(log_oxoMINCH_t2, log_oxoMINCH_t3), na.rm = TRUE),
                  log_DINCH_both = mean(c(log_DINCH_t2, log_DINCH_t3), na.rm = TRUE)
    )
  
  # Non standardized exposures
  exposures_non_sd <- expo_data %>%
    dplyr::transmute(
      ident = as.character(ident),
      
      log_MBzP_nosd_t2 = log(mo_MBzP_i_t2), 
      log_MBzP_nosd_t3 = log(mo_MBzP_i_t3),
      log_MEP_nosd_t2 = log(mo_MEP_i_t2), 
      log_MEP_nosd_t3 = log(mo_MEP_i_t3),
      log_MiBP_nosd_t2 = log(mo_MiBP_i_t2), 
      log_MiBP_nosd_t3 = log(mo_MiBP_i_t3),   
      log_MnBP_nosd_t2 = log(mo_MnBP_i_t2), 
      log_MnBP_nosd_t3 = log(mo_MnBP_i_t3),
      
      log_ohMPHP_nosd_t2 = log(mo_ohMPHP_i_t2), 
      log_ohMPHP_nosd_t3 = log(mo_ohMPHP_i_t3),
      
      # log_MECPP_nosd_t2 = log(mo_MECPP_i_t2), 
      # log_MECPP_nosd_t3 = log(mo_MECPP_i_t3),
      # log_MEHHP_nosd_t2 = log(mo_MEHHP_i_t2),
      # log_MEHHP_nosd_t3 = log(mo_MEHHP_i_t3),
      # log_MEHP_nosd_t2 = log(mo_MEHP_i_t2),
      # log_MEHP_nosd_t3 = log(mo_MEHP_i_t3),
      # log_MEOHP_nosd_t2 = log(mo_MEOHP_i_t2),
      # log_MEOHP_nosd_t3 = log(mo_MEOHP_i_t3),
      # log_MMCHP_nosd_t2 = log(mo_MMCHP_i_t2), 
      # log_MMCHP_nosd_t3 = log(mo_MMCHP_i_t3),  
      log_DEHP_nosd_t2 = log(mo_DEHP_ms_i_t2),
      log_DEHP_nosd_t3 = log(mo_DEHP_ms_i_t3),
      
      # log_cxMiNP_nosd_t2 = log(mo_cxMiNP_i_t2),
      # log_cxMiNP_nosd_t3 = log(mo_cxMiNP_i_t3),
      # log_ohMiNP_nosd_t2 = log(mo_ohMiNP_i_t2), 
      # log_ohMiNP_nosd_t3 = log(mo_ohMiNP_i_t3),
      # log_oxoMiNP_nosd_t2 = log(mo_oxoMiNP_i_t2), 
      # log_oxoMiNP_nosd_t3 = log(mo_oxoMiNP_i_t3),
      log_DiNP_nosd_t2 = log(mo_DiNP_ms_i_t2),
      log_DiNP_nosd_t3 = log(mo_DiNP_ms_i_t3),
      
      # log_ohMINCH_nosd_t2 = log(mo_ohMINCH_i_t2),
      # log_ohMINCH_nosd_t3 = log(mo_ohMINCH_i_t3),
      # log_oxoMINCH_nosd_t2 = log(mo_oxoMINCH_i_t2),
      # log_oxoMINCH_nosd_t3 = log(mo_oxoMINCH_i_t3),
      log_DINCH_nosd_t2 = log(mo_DINCH_ms_i_t2),
      log_DINCH_nosd_t3 = log(mo_DINCH_ms_i_t3)) %>%
    
    # add mean value of exposure for T2 and T3
    rowwise() %>%
    dplyr::mutate(log_MBzP_nosd_both = mean(c(log_MBzP_nosd_t2, log_MBzP_nosd_t3), na.rm = TRUE),
                  log_MEP_nosd_both = mean(c(log_MEP_nosd_t2, log_MEP_nosd_t3), na.rm = TRUE),
                  log_MiBP_nosd_both = mean(c(log_MiBP_nosd_t2, log_MiBP_nosd_t3), na.rm = TRUE),
                  log_MnBP_nosd_both = mean(c(log_MnBP_nosd_t2, log_MnBP_nosd_t3), na.rm = TRUE),
                  
                  log_ohMPHP_nosd_both = mean(c(log_ohMPHP_nosd_t2, log_ohMPHP_nosd_t3), na.rm = TRUE),
                  
                  # log_MECPP_nosd_both = mean(c(log_MECPP_nosd_t2, log_MECPP_nosd_t3), na.rm = TRUE),
                  # log_MEHHP_nosd_both = mean(c(log_MEHHP_nosd_t2, log_MEHHP_nosd_t3), na.rm = TRUE),
                  # log_MEHP_nosd_both = mean(c(log_MEHP_nosd_t2, log_MEHP_nosd_t3), na.rm = TRUE),
                  # log_MEOHP_nosd_both = mean(c(log_MEOHP_nosd_t2, log_MEOHP_nosd_t3), na.rm = TRUE),
                  # log_MMCHP_nosd_both = mean(c(log_MMCHP_nosd_t2, log_MMCHP_nosd_t3), na.rm = TRUE),
                  log_DEHP_nosd_both = mean(c(log_DEHP_nosd_t2, log_DEHP_nosd_t3), na.rm = TRUE),
                  
                  # log_cxMiNP_nosd_both = mean(c(log_cxMiNP_nosd_t2, log_cxMiNP_nosd_t3), na.rm = TRUE),
                  # log_ohMiNP_nosd_both = mean(c(log_ohMiNP_nosd_t2, log_ohMiNP_nosd_t3), na.rm = TRUE),
                  # log_oxoMiNP_nosd_both = mean(c(log_oxoMiNP_nosd_t2, log_oxoMiNP_nosd_t3), na.rm = TRUE),
                  log_DiNP_nosd_both = mean(c(log_DiNP_nosd_t2, log_DiNP_nosd_t3), na.rm = TRUE),
                  
                  # log_ohMINCH_nosd_both = mean(c(log_ohMINCH_nosd_t2, log_ohMINCH_nosd_t3), na.rm = TRUE),
                  # log_oxoMINCH_nosd_both = mean(c(log_oxoMINCH_nosd_t2, log_oxoMINCH_nosd_t3), na.rm = TRUE),
                  log_DINCH_nosd_both = mean(c(log_DINCH_nosd_t2, log_DINCH_nosd_t3), na.rm = TRUE)
    ) %>% 
    
    # Remove T3 exposures
    dplyr::select(ident, dplyr::contains("t2"), dplyr::contains("both"))
  
  # add non-standardized exposures to model data
  expo_data <- exposures %>%
    dplyr::left_join(exposures_non_sd, by = "ident") %>% 
    dplyr::arrange(ident) %>%
    dplyr::filter(ident != 18147)
}


#' Impute missing data in covariates
#'
#' @param data_w_missing A data.frame with covariates containing missing values
#'
#' @return A data.frame with covariates with missing values imputed
#' @export 

ImputeMissingCov <- function(data_w_missing) {
  
  # # NOTE: in theory the generic code below should work, but for some reason (ppbly presence of metadata?)
  # # it doesn't, that is why it is hard-coded
  # Mode <- function(x) {
  #    ux <- unique(x)
  #    ux[which.max(tabulate(match(x, ux)))]
  # }
  # 
  # data_complete <- data_w_missing %>%
  #    dplyr::mutate_if(is.factor, funs(tidyr::replace_na(., Mode(na.omit(.))))) %>% 
  #    dplyr::mutate_if(is.numeric, funs(replace(., is.na(.), mean(., na.rm = TRUE))))
  
  data_complete <- data_w_missing %>%
    dplyr::mutate(mom_edu = tidyr::replace_na(mom_edu, "High"),
                  child_sex = tidyr::replace_na(child_sex, "Male"),
                  smoke_high = tidyr::replace_na(smoke_high, "No"))
  
  data_complete$mom_height[is.na(data_complete$mom_height)] <- median(data_complete$mom_height, na.rm = TRUE)
  data_complete$gestational_duration[is.na(data_complete$gestational_duration)] <- median(data_complete$gestational_duration, na.rm = TRUE)
  data_complete$gestational_duration_weeks[is.na(data_complete$gestational_duration_weeks)] <- median(data_complete$gestational_duration_weeks, na.rm = TRUE)
  
  return(data_complete)
}

#' Transform the dataset into a long format 
#'
#' @param model_data A data.frame containing all variables of interest that
#' will be used in the regression models (in a wide format)
#' @param cols_to_long A character vector containing names of variables to be 
#' turned into long format
#'
#' @return A data.frame containing all variables of interest that
#' will be used in the regression models (in a long format)
#' @export
#' 

ModelDataAllVarsLong <- function(model_data,
                                 cols_to_long) {
  
  model_data_long <- model_data %>%
    tidyr::pivot_longer(cols = dplyr::all_of(cols_to_long),
                        names_to = "exp",
                        values_to = "exp_val") %>% 
    dplyr::select(ident:trimester, 
                  mom_weight:smoke_high, 
                  outcome_val_sd:exp_val, 
                  period_rank)
  
  return(model_data_long)
}

ColsToLong <- function(x, y = NULL) {
  cols_to_long <- c(paste0(x, "_t2"),
                    paste0(x, "_t3"),
                    paste0(x, "_both"))
  
  if (!is.null(y)) {
    cols_to_long <- c(cols_to_long,
                      paste0(y, "_nosd_t2"),
                      paste0(y, "_nosd_both"))
  }
  
  return(cols_to_long)
}

RemoveConditions <- function(data) {
  data_clean <- data %>%
    filter(!stringr::str_detect(exp, "nosd|t3"),
           !(stringr::str_detect(exp, "both") & trimester == "US2"),
           !(stringr::str_detect(exp, "t2") & (trimester == "US3" | trimester == "birth")))
  
  return(data_clean)
}
