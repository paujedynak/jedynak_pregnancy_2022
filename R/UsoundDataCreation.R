#' Helper function to produce long format outcome
#'
#' @param data_wide A data.frame of ultrasound data in the wide format
#' @param var_name A string defining which variables should be turned into long format
#'
#' @return A long format data.frame
#' @export
#' @import dplyr
#' @import stringr
#' 

TidyVar <- function(data_wide, var_name) {
  
  # set to long format
  data_long <- data_wide %>%
    dplyr::select(ident, dplyr::contains(var_name)) %>%
    tidyr::pivot_longer(
      cols = -ident, 
      names_to = "var_name", 
      values_to = "val"
    )
  
  # create rank
  data_long <- data_long %>%
    
    # period rank comes from the label on the US measurement: "Date_examen_T2_3" will return a period_rank
    # = T2_3. This does not have to conform with trimesters as they are calculated basing on GA
    dplyr::mutate(period_rank = dplyr::if_else(!stringr::str_detect(var_name, "man"), 
                                 stringr::str_sub(var_name, -4, -1), 
                                 stringr::str_sub(var_name, -8, -1))) # Period_rank becomes NA for birth outcomes
  
  # final cleaning manipulations
  data_long <- data_long %>%
    dplyr::filter(!is.na(val)) %>%
    dplyr::filter(!stringr::str_detect(period_rank, "T1")) %>% 
    dplyr::select(-var_name) %>%
    dplyr::mutate(ident = as.character(ident),
                  period_rank = as.character(period_rank)) %>%
    dplyr::rename(!!var_name := val)
  
  return(data_long)
}

#' Main ultrasound data creation function
#'
#' @param bdd_grossesse A .dta file containing bdd_grossesse_v4 data
#' @param ultrasound_manual_data A .dta file containing manually recorded usound data donnees_echo_saisies_quest_mt1caj1.dta
#'
#' @return A dataframe with manual ultrasound data
#' @export
#' @import dplyr
#' @import stringr
#' @import tidyr

CreateUsoundDdata <- function(bdd_grossesse, 
                              ultrasound_manual_data) {
  
  # Manual ultrasound variables ----
  # Only variables from US2 and US3 are taken into account (except of the exam date) and all variables
  # containing only missing data were excluded
  us_man <- dplyr::rename(ultrasound_manual_data,
                          
                          # Biparietal diameter
                          biometrie_bip_t2_man_1 = mt1caj1_q02p32,
                          biometrie_bip_t2_man_2 = mt1caj1_q02p89,
                          biometrie_bip_t3_man_1 = mt1caj1_q03p31,
                          
                          # Head circumference
                          perimetre_cranien_t2_man_1 = mt1caj1_q02p33,
                          perimetre_cranien_t2_man_2 = mt1caj1_q02p90,
                          perimetre_cranien_t3_man_1 = mt1caj1_q03p32,
                          
                          # Abdominal circumference
                          perimetre_abdominal_t2_man_1 = mt1caj1_q02p45,
                          perimetre_abdominal_t2_man_2 = mt1caj1_q02p102,
                          perimetre_abdominal_t3_man_1 = mt1caj1_q03p36,
                          
                          # Femur length
                          longueur_femur_t2_man_1 = mt1caj1_q02p46,
                          longueur_femur_t2_man_2 = mt1caj1_q02p103,
                          longueur_femur_t3_man_1 = mt1caj1_q03p37,
                          
                          # Age gestationnel semaines
                          age_gest_semaines_t2_man_1 = mt1caj1_q02p12, # Age gestationnel SEMAINES 
                          age_gest_semaines_t3_man_1 = mt1caj1_q03p10, # Age gestationnel SEMAINES 
                          
                          # Age gestationnel jours
                          age_gest_jours_t2_man_1 = mt1caj1_q02p13, # Age gestationnel JOUR(S) 
                          age_gest_jours_t3_man_1 = mt1caj1_q03p11 # Age gestationnel JOUR(S) 
  )
  
  # Select variables of interest from the manual US dataset (38 x 17)
  us_manual <- dplyr::select(us_man, 
                             -dplyr::contains("mt1caj1"),
                             -mother_token)
  
  # Select variables of interest from the bdd_grossesse (484 x 59)
  us_digital <- dplyr::select(bdd_grossesse,
                              ident,
                              dplyr::contains("date_exam"),
                              dplyr::contains("ddg_echo"),
                              dplyr::contains("datelmp"),
                              dplyr::contains("biometrie_bip"),
                              dplyr::contains("perimetre_cranien"),
                              dplyr::contains("perimetre_abdominal"),
                              dplyr::contains("longueur_fem"))
  
  # Merge manual US and bdd_grossesse database
  us_data <- dplyr::left_join(us_digital, us_manual, by = "ident") %>% 
    dplyr::mutate(ident = as.character(ident)) %>% 
    dplyr::arrange(ident)
  
  # Long format ----
  # Ultrasound measures of interest
  biparietal_diameter <- TidyVar(us_data, "biometrie_bip")
  head_circumference <- TidyVar(us_data, "perimetre_cranien")
  abdominal_circumference <- TidyVar(us_data, "perimetre_abdominal")
  femur_length <- TidyVar(us_data, "longueur_fem")
  
  # Exam dates
  exam_dates <- TidyVar(us_data, "date_exam")
  
  # Gestational age for manual database
  age_gest_sem <- TidyVar(us_data, "age_gest_semaines")
  age_gest_j <- TidyVar(us_data, "age_gest_jours")
  
  # LMP dates
  lmp_dates <- dplyr::select(us_data, ident, po_datelmp)
  
  # US estimated beginning of the pregnancy. If more than one, pick US estimate of the 
  # beginning of pregnancy that is closer to the LMP
  averaged_ddg_echo <- dplyr::select(us_data, ident, contains("ddg_echo")) %>% 
    dplyr::rowwise() %>% 
    
    # Calculate the average of US based estimate of beginning of pregnancy
    dplyr::mutate(diff_ddg_echo = as.numeric(ddg_echo_T1_1 - ddg_echo_T1_2),
                  averaged_ddg_echo = dplyr::case_when(!is.na(diff_ddg_echo) & ddg_echo_T1_1 < ddg_echo_T1_2 ~ ddg_echo_T1_1,
                                                       !is.na(diff_ddg_echo)& ddg_echo_T1_1 > ddg_echo_T1_2 ~ ddg_echo_T1_2,
                                                       TRUE ~ dplyr::coalesce(ddg_echo_T1_1, ddg_echo_T1_2)),
                  .keep = "unused")
  
  # Merge all variables
  us_data_long <- dplyr::left_join(biparietal_diameter, head_circumference, by = c("ident", "period_rank")) %>% 
    dplyr::left_join(abdominal_circumference, by = c("ident", "period_rank")) %>% 
    dplyr::left_join(femur_length, by = c("ident", "period_rank")) %>% 
    dplyr::left_join(exam_dates, by = c("ident", "period_rank")) %>% 
    dplyr::left_join(age_gest_sem, by = c("ident", "period_rank")) %>% 
    dplyr::left_join(age_gest_j, by = c("ident", "period_rank")) %>% 
    dplyr::left_join(lmp_dates, by = c("ident")) %>%
    dplyr::left_join(averaged_ddg_echo, by = c("ident")) %>%
    
    # Add gestational ages
    dplyr::mutate(
      # Calculate difference between LMP and US based estimate of beginning of pregnancy - 14 days
      diff = difftime(averaged_ddg_echo, po_datelmp + 14, units = "days"),
      
      # LMP date estimated from the US
      lmp_from_us = averaged_ddg_echo - 14,
      
      # If the above difference > 14 days, use US instead of LMP based estimate of beginning of pregnancy
      corr_lmp = dplyr::case_when(abs(diff) > 14 ~ lmp_from_us,
                                  TRUE ~ po_datelmp),
      
      # For manual US database use age gestationnel (exam date for manual is missing), for bdd_grossesse use date_exam - lmp corrected 
      # (and we use age gestationnel because this is the only variable available for T2 and T3)
      gestational_age = dplyr::case_when(stringr::str_detect(period_rank, "man") ~ age_gest_semaines * 7 + age_gest_jours,
                                  TRUE ~ as.numeric(date_exam - corr_lmp)),
      
      # Create a period rank based on trimester to allow for GA imputation per period_rank
      period_rank_temp = toupper(stringr::str_sub(period_rank, 1, 2))) %>% 
    
    # impute missing GA
    dplyr::group_by(period_rank_temp) %>% 
    dplyr::mutate(gestational_age = tidyr::replace_na(as.numeric(gestational_age), median(as.numeric(gestational_age), na.rm = TRUE)))

  # Clean trimester variable
  # Ref: Claire's email from 07/06/19 + Remy's paper on pollution
  # Trimester 1: from the estimated fertilization day – day 14 counting from the last menstrual period – to day 105
  # Trimester 2: from day 106 to day 197
  # Trimester 3: from day 198 to birth
  # These are the cutoff used in the ESCAPE project to define trimester of exposure 
  
  us_data_trim <- dplyr::mutate(us_data_long,
                                trimester = cut(gestational_age,
                                                breaks = c(0, 105, 197, max(gestational_age, na.rm = TRUE)),
                                                labels = c("US1", "US2", "US3"))) %>% 
    # remove T1 if any appeared
    dplyr::filter(trimester != "US1") %>% 
    droplevels()
  
  # Flag ultrasound to keep ----
  # When multiple ultrasounds per trimester, keep one closest to sepages median
  # ultrasound time
  
  # Compute median ultrasound times using as reference women with only 1
  # ultrasound per trimester
  ultrasound_medians <- us_data_trim %>%
    dplyr::mutate(rank = stringr::str_sub(period_rank, -1)) %>% # rank of ultrasound in trimester
    dplyr::group_by(ident) %>%
    dplyr::mutate(n_t2 = sum(trimester == "US2", na.rm = TRUE), # n ultrasounds at T2
                  n_t3 = sum(trimester == "US3", na.rm = TRUE)) %>% # n ultrasounds at T3
    dplyr::filter(n_t2 == 1 & n_t3 == 1) %>%
    dplyr::group_by(trimester) %>%
    dplyr::summarise(median_ga = median(gestational_age, na.rm = TRUE))
  
  # compute for each ultrasound time to ref and flag the closest
  ultrasound_long <- us_data_trim %>%
    dplyr::left_join(ultrasound_medians, by = "trimester") %>%
    dplyr::mutate(time_to_med = abs(gestational_age - median_ga)) %>%
    dplyr::arrange(time_to_med) %>%
    dplyr::group_by(ident, trimester) %>%
    dplyr::mutate(keep_flag = dplyr::row_number(),
                  keep_flag = ifelse(keep_flag == 1, TRUE, FALSE)) %>% # try to drop the false flags already here
    dplyr::ungroup() %>% 
    
    # Rename
    dplyr::rename(biparietal_diameter = biometrie_bip, 
                  head_circumference = perimetre_cranien,
                  abdominal_circumference = perimetre_abdominal,
                  femur_length = longueur_fem, 
                  exam_date = date_exam,
                  lmp_date = po_datelmp
    )
  
  # list outcomes of interest
  ultrasound_outcomes <- c("biparietal_diameter", "head_circumference", 
                           "abdominal_circumference", "femur_length")
  
  # select outcomes and set to long format
  ultrasound_final <- ultrasound_long %>%
    dplyr::select(ident, 
                  dplyr::all_of(ultrasound_outcomes), 
                  trimester, 
                  gestational_age, 
                  exam_date,
                  lmp_date,
                  corr_lmp,
                  averaged_ddg_echo,
                  keep_flag, 
                  period_rank
    ) %>% 
    
    tidyr::pivot_longer(cols = dplyr::all_of(ultrasound_outcomes),
                        names_to = "outcome",
                        values_to = "outcome_val")
  
  return(ultrasound_final)
}

