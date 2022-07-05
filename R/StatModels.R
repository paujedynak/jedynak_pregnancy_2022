#' Run linear regression model without interaction
#'
#' @param exp A string defining phenol exposure
#' @param model_data_long A data.frame containing all variables of interest that
#' will be used in the regression models (in a long format)
#'
#' @return An lm object
#' @export
#' @import glue
#' @import stats
#' 

GrowthModelNoInter <- function(exp, 
                               model_data_long) {
  
  model_formula <- glue::glue(
    "outcome_val_sd ~ {exp} + 
      mom_age + 
      mom_weight + 
      mom_height + 
      gestational_age + 
      I(gestational_age^2) + 
      mom_edu + 
      parity + 
      smoke_high +
      child_sex"
  ) %>%
    stats::as.formula(.)
  
  fit <- stats::lm(model_formula, data = model_data_long)
  
  return(fit)
}

#' Run anova III to obtain the overall effect of a categorical exposure
#'
#' @param exp A string defining phenol exposure
#' @param model_data_long A data.frame containing all variables of interest that
#' @param sex A logical defining if to stratify be sex, default = FALSE
#'
#' @return An anova object
#' @export 
#' @import glue
#' @import stats
#' @importFrom car::Anova
#' 
OverallEffect <- function(exp, 
                          model_data_long,
                          sex = FALSE) {
  
  if (isTRUE(sex)) {
    
    model_formula <- glue::glue(
      "outcome_val_sd ~ {exp} + 
      mom_age + 
      mom_weight + 
      mom_height + 
      gestational_age + 
      I(gestational_age^2) + 
      mom_edu + 
      parity + 
      smoke_high"
    ) %>%
      stats::as.formula(.)
    
  } else {
    
    model_formula <- glue::glue(
      "outcome_val_sd ~ {exp} + 
      mom_age + 
      mom_weight + 
      mom_height + 
      gestational_age + 
      I(gestational_age^2) + 
      mom_edu + 
      parity + 
      smoke_high +
      child_sex"
    ) %>%
      stats::as.formula(.)
  }
  
  # By default, R uses Type I (“one”) Sum of Squares for ANOVAs — Type 1 is perfectly fine for an ANOVA 
  # with only one independent variable. However, when you add more independent variables (technically an ANCOVA),
  # Type I will produce incorrect results. For regressions (when using the lm() function), R uses Type III (“three”) 
  # Sum of Squares by default.
  # https://towardsdatascience.com/one-of-the-most-common-unintentional-mistakes-when-running-an-anova-in-r-cfa55d332a
  
  aov_fit <- stats::aov(model_formula, data = model_data_long)
  ancova_fit <- car::Anova(aov_fit, type = "III")
  
  return(ancova_fit)
}


#' Run linear regression model with interaction
#'
#' @param exp A string defining phenol exposure
#' @param model_data_long A data.frame containing all variables of interest that
#' will be used in the regression models (in a long format)
#'
#' @return An lm object
#' @export
#' @import glue
#' @import stats

GrowthModelInter <- function(exp, 
                             model_data_long) {
  
  model_formula <- glue::glue(
    "outcome_val_sd ~ {exp} * child_sex +
      mom_age +
      mom_weight +
      mom_height +
      gestational_age +
      I(gestational_age^2) +
      mom_edu +
      parity +
      smoke_high"
  ) %>%
    stats::as.formula(.)
  
  fit <- stats::lm(model_formula, data = model_data_long)
  
  return(fit)
}

#' Run linear regression model stratified by sex
#'
#' @param exp A string defining phenol exposure
#' @param model_data_long A data.frame containing all variables of interest that
#' will be used in the regression models (in a long format)
#'
#' @return An lm object
#' @export
#' 
#' @import glue
#' @import stats

GrowthModelSexStrat <- function(exp, 
                                model_data_long) {
  
  # to remember for the future when working on wide format:
  # lm(data = data_use, expr_boy, subset = (ch_sex == 0))
  
  model_formula <- glue::glue(
    "outcome_val_sd ~ {exp} + 
      mom_age + 
      mom_weight + 
      mom_height + 
      gestational_age + 
      I(gestational_age^2) + 
      mom_edu + 
      parity + 
      smoke_high"
  ) %>%
    stats::as.formula(.)
  
  fit <- stats::lm(model_formula, data = model_data_long)
  
  return(fit)
}

#' Compute regressions 
#'
#' @param model_data_long A data.frame containing all variables of interest that will be used in the regression models (in a long format)
#' @param cat A logical indicating if the categorical variable is processed, default = FALSE
#' @param sex A logical indicating if a sex stratified analysis is to be performed, default = FALSE
#'
#' @return A data.frame with regression outputs
#' @export
#' 
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map
#' @import stats
#' @importFrom broom tidy
#' @import stringr

ComputeGrowthModels <- function(model_data_long, 
                                cat = FALSE,
                                sex = FALSE) {
  
  # compute interaction p-values
  int_pvalue <- model_data_long %>%
    dplyr::group_by(exp, outcome, trimester) %>%
    dplyr::group_modify(~broom::tidy(GrowthModelInter("exp_val", .), conf.int = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(stringr::str_detect(term, ":child_sexMale")) %>%
    dplyr::mutate(p_value_int = p.value,
                  term = stringr::str_extract(term, "[^:]+")) %>% 
    dplyr::select(exp:term, p_value_int)
  
  if (isTRUE(sex)) {
    
    # Analysis: both categorical and non categorical exposure plus stratified by sex
    
    # compute models with interaction
    growth_models_int <- model_data_long %>%
      dplyr::group_by(child_sex, exp, outcome, trimester) %>%
      dplyr::group_modify(~broom::tidy(GrowthModelSexStrat("exp_val", .), conf.int = TRUE))
    
    if (isTRUE(cat)) {
      
      # add model Ns
      model_n_int <- model_data_long %>%
        dplyr::group_by(child_sex, exp, outcome, trimester) %>%
        dplyr::group_map(~GrowthModelSexStrat("exp_val", .)) %>% 
        purrr::map(~table(.$model$exp_val)) %>% 
        data.frame() %>% 
        dplyr::select(Var1, contains("Freq"))
      
      colnames(model_n_int) <- c("exp_val", seq(2:dim(model_n_int)[2]))
      
      model_n_int <- model_n_int %>% 
        tidyr::pivot_longer(cols = -exp_val,
                            names_to = "freq",
                            values_to = "N") %>% 
        dplyr::filter(exp_val != "<LOD") %>% 
        dplyr::arrange(as.numeric(freq))
      
    } else {
      
      # add model Ns
      model_n_int <- model_data_long %>%
        dplyr::group_by(child_sex, exp, outcome, trimester) %>%
        dplyr::group_modify(~as.data.frame(length(stats::fitted(GrowthModelSexStrat("exp_val", .)))))
      
      colnames(model_n_int)[5] <- "N"
    }
    
  } else {
    
    # Analysis: both categorical and non categorical exposure not stratified by sex
    
    # compute models without interaction
    growth_models_no_int <- model_data_long %>%
      dplyr::group_by(exp, outcome, trimester) %>%
      dplyr::group_modify(~broom::tidy(GrowthModelNoInter("exp_val", .), conf.int = TRUE))
    
    if (isTRUE(cat)) {
      
      # add model Ns
      model_n_no_int <- model_data_long %>%
        dplyr::group_by(exp, outcome, trimester) %>%
        dplyr::group_map(~GrowthModelNoInter("exp_val", .)) %>% 
        purrr::map(~table(.$model$exp_val)) %>% 
        data.frame() %>% 
        dplyr::select(Var1, contains("Freq"))
      
      colnames(model_n_no_int) <- c("exp_val", seq(2:dim(model_n_no_int)[2]))
      
      model_n_no_int <- model_n_no_int %>% 
        tidyr::pivot_longer(cols = -exp_val,
                            names_to = "freq",
                            values_to = "N") %>% 
        dplyr::filter(exp_val != "<LOD") %>% 
        dplyr::arrange(as.numeric(freq))
      
    } else {
      
      # add model Ns
      model_n_no_int <- model_data_long %>%
        dplyr::group_by(exp, outcome, trimester) %>%
        dplyr::group_modify(~as.data.frame(length(stats::fitted(GrowthModelNoInter("exp_val", .)))))
      
      colnames(model_n_no_int)[4] <- "N"
    }
  }
  
  if (isTRUE(cat)) {
    
    if (isTRUE(sex)) {
      
      # Analysis: categorical exposure stratified by sex
      
      # compute overall effect
      overall_effect_pvalue <- model_data_long %>%
        dplyr::group_by(child_sex, exp, outcome, trimester) %>%
        dplyr::group_modify(~broom::tidy(OverallEffect("exp_val", sex = sex, .))) %>% 
        dplyr::ungroup() %>%
        dplyr::filter(stringr::str_detect(term, "exp_val")) %>%
        dplyr::mutate(p_value_main_eff = p.value,
                      stat_main_eff = statistic) %>% 
        dplyr::select(child_sex:trimester, stat_main_eff, p_value_main_eff)
      
      
      growth_models_final <- growth_models_int %>%
        dplyr::filter(stringr::str_detect(term, "exp_val")) %>% 
        dplyr::bind_cols(dplyr::select(model_n_int, N)) %>%
        merge(overall_effect_pvalue, by = c("child_sex", "exp", "outcome", "trimester")) %>% 
        merge(int_pvalue, by = c("exp", "outcome", "trimester", "term")) %>% 
        dplyr::rename_all(~sub("[.]", "_", .x)) %>% 
        dplyr::arrange(exp, outcome, trimester, term, child_sex)
      
    } else {
      
      # Analysis: categorical exposure not stratified by sex
      
      # compute overall effect
      overall_effect_pvalue <- model_data_long %>%
        dplyr::group_by(exp, outcome, trimester) %>%
        dplyr::group_modify(~broom::tidy(OverallEffect("exp_val", .))) %>% 
        dplyr::ungroup() %>%
        dplyr::filter(stringr::str_detect(term, "exp_val")) %>%
        dplyr::mutate(p_value_main_eff = p.value,
                      stat_main_eff = statistic) %>% 
        dplyr::select(exp:trimester, stat_main_eff, p_value_main_eff)
      
      growth_models_final <- growth_models_no_int %>%
        dplyr::filter(stringr::str_detect(term, "exp_val")) %>% 
        dplyr::bind_cols(dplyr::select(model_n_no_int, N)) %>%
        merge(int_pvalue, by = c("exp", "outcome", "trimester", "term")) %>% 
        merge(overall_effect_pvalue, by = c("exp", "outcome", "trimester")) %>% 
        dplyr::rename_all(~sub("[.]", "_", .x))
    }
    
  } else {
    
    if (isTRUE(sex)) {
      
      # Analysis: continuous exposure plus stratified by sex
      
      # compute overall effect
      overall_effect_pvalue <- model_data_long %>%
        dplyr::group_by(child_sex, exp, outcome, trimester) %>%
        dplyr::group_modify(~broom::tidy(OverallEffect("exp_val", sex = sex, .))) %>% 
        dplyr::ungroup() %>%
        dplyr::filter(stringr::str_detect(term, "exp_val")) %>%
        dplyr::mutate(p_value_main_eff = p.value,
                      stat_main_eff = statistic) %>% 
        dplyr::select(child_sex:trimester, stat_main_eff, p_value_main_eff)
      
      growth_models_final <- growth_models_int %>% 
        dplyr::filter(stringr::str_detect(term, "exp_val")) %>% 
        merge(model_n_int, by = c("child_sex", "exp", "outcome", "trimester")) %>%
        merge(overall_effect_pvalue, by = c("child_sex", "exp", "outcome", "trimester")) %>% 
        merge(int_pvalue, by = c("exp", "outcome", "trimester", "term")) %>% 
        dplyr::rename_all(~sub("[.]", "_", .x)) %>% 
        dplyr::arrange(exp, outcome, trimester, child_sex)
      
    } else {
      
      # Analysis: continuous exposure not stratified by sex
      
      growth_models_final <- growth_models_no_int %>%
        dplyr::filter(stringr::str_detect(term, "exp_val")) %>% 
        merge(model_n_no_int, by = c("exp", "outcome", "trimester")) %>%
        merge(int_pvalue, by = c("exp", "outcome", "trimester", "term")) %>% 
        dplyr::rename_all(~sub("[.]", "_", .x))
    }
  }
  
  return(growth_models_final)
}


#' Preprocess model output
#'
#' @param model_output A data.frame with regression outputs
#' @param exp A string defining phenol exposure
#'
#' @return A data.frame with formatted regression outputs
#' @export
#' @import dplyr
#' @import stringr

PreprocessOutput <- function(model_output, 
                             expo, 
                             phthalates = FALSE) {
  
  if (isTRUE(phthalates)) {
    
    # combine outcome and trimester
    final_model_output <- model_output %>%
      dplyr::rename(tri_out = trimester) %>%
      dplyr::mutate(
        outcome_period = stringr::str_c(outcome, tri_out, sep = "_"),
        exposure = dplyr::case_when(
          stringr::str_detect(exp, "MEP") ~ "Monoethyl phthalate (MEP)",
          stringr::str_detect(exp, "MiBP") ~ "Mono-iso-butyl phthalate (MiBP)",
          stringr::str_detect(exp, "MnBP") ~ "Mono-n-butyl phthalate (MnBP)",
          stringr::str_detect(exp, "MBzP") ~ "Monobenzyl phthalate (MBzP)",
          # stringr::str_detect(exp, "MEHP") ~ "Mono(2-ethylhexyl) phthalate (MEHP)",
          # stringr::str_detect(exp, "MEHHP") ~ "Mono(2-ethyl-5-hydroxyhexyl) phthalate (MEHHP)",
          # stringr::str_detect(exp, "MEOHP") ~ "Mono(2-ethyl-5-oxohexyl) phthalate (MEOHP)",
          # stringr::str_detect(exp, "MECPP") ~ "Mono(2-ethyl-5-carboxypentyl) phthalate (MECPP)",
          # stringr::str_detect(exp, "MMCHP") ~ "Mono-2-carboxymethyl hexyl phthalate (MMCHP)",
          # stringr::str_detect(exp, "ohMiNP") ~ "Mono-4-methyl-7-hydroxyoctyl phthalate (ohMiNP)",
          # stringr::str_detect(exp, "oxoMiNP") ~ "Mono-4-methyl-7-oxooctyl phthalate (oxoMiNP)",
          # stringr::str_detect(exp, "cxMiNP") ~ "Mono-4-methyl-7-carboxyoctyl phthalate (cxMiNP)",
          # stringr::str_detect(exp, "ohMINCH") ~ "2-(((hydroxy-4-methyloctyl)oxy)carbonyl)cyclohexanecarboxylic acid (ohMINCH)",
          # stringr::str_detect(exp, "oxoMINCH") ~ "2-(((4-methyl-7-oxyooctyl)oxy)carbonyl)cyclohexanecarboxylic acid (oxoMINCH)",
          stringr::str_detect(exp, "ohMPHP") ~ "6-hydroxy monopropylheptylphthalate (ohMPHP)",
          stringr::str_detect(exp, "DEHP") ~ paste0("Molar sum of DEHP metabolites (", "\u03a3", "DEHP)"),
          stringr::str_detect(exp, "DiNP") ~ paste0("Molar sum of DiNP metabolites (", "\u03a3", "DiNP)"),
          stringr::str_detect(exp, "DINCH") ~ paste0("Molar sum of DINCH metabolites (", "\u03a3", "DINCH)")
        )
      )
    
  } else {
    
    # combine outcome and trimester
    final_model_output <- model_output %>%
      dplyr::rename(tri_out = trimester) %>%
      dplyr::mutate(
        outcome_period = stringr::str_c(outcome, tri_out, sep = "_"),
        exposure = dplyr::case_when(
          stringr::str_detect(exp, "BPA") ~ "Bisphenol A",
          stringr::str_detect(exp, "BPS") ~ "Bisphenol S",
          stringr::str_detect(exp, "TCS") ~ "Triclosan",
          stringr::str_detect(exp, "BP3") ~ "Benzophenone-3",
          stringr::str_detect(exp, "BUPA") ~ "Butylparaben",
          stringr::str_detect(exp, "ETPA") ~ "Ethylparaben",
          stringr::str_detect(exp, "MEPA") ~ "Methylparaben",
          stringr::str_detect(exp, "PRPA") ~ "Propylparaben"
        )
      )
  }
  
  # final model output
  final_model_output <- final_model_output %>%
    dplyr::filter(exposure == expo)
  
  return(final_model_output)
}

#' Format regressions outcomes
#'
#' @param model_output A data.frame with regression outputs
#' @param expo A string defining phenol exposure
#' @param cat A logical indicating if the categorical variable is processed, default = FALSE
#' @param sex A logical indicating if a sex stratified analysis is to be performed, default = FALSE
#'
#' @return A data.frame with formatted regression outputs
#' @export
#' @import dplyr
#' @import stringr
#' @importFrom tidyr pivot_wider

PrintTab <- function(model_output, 
                     expo,
                     cat = FALSE,
                     sex = FALSE, 
                     phthalates = FALSE) {
  
  # pre process model output
  processed_model_output <- PreprocessOutput(model_output, 
                                             expo,
                                             phthalates)
  
  if (isTRUE(cat)) {
    
    if (isTRUE(sex)) {
      
      tab <- processed_model_output %>%
        dplyr::select(child_sex,
                      exposure, 
                      outcome, 
                      tri_out, 
                      term,
                      estimate, 
                      conf_low, 
                      conf_high, 
                      p_value, 
                      p_value_main_eff,
                      p_value_int,
                      stat_main_eff,
                      N) %>%
        dplyr::mutate(tri_out = factor(tri_out,
                                       levels = c("US2", "US3", "birth")),
                      est_ci = stringr::str_c(format(round(estimate, 2), nsmall = 2), " (", 
                                              format(round(conf_low, 2), nsmall = 2), "; ", 
                                              format(round(conf_high, 2), nsmall = 2), ")"),
                      p_value = as.character(signif(as.numeric(round(p_value, 4)), digits = 1)),
                      p_value_main_eff = dplyr::if_else(p_value_main_eff < 0.1, 
                                                        stringr::str_c("F = ",
                                                                       format(round(stat_main_eff, 1), nsmall = 1), 
                                                                       " (p-value = ", 
                                                                       as.character(signif(round(p_value_main_eff, 4), digits = 1)),
                                                                       ")"),
                                                        as.character(signif(round(p_value_main_eff, 2), digits = 1))),
                      p_value_int = as.character(signif(round(p_value_int, 4), digits = 1)),
                      N = as.integer(N)
        ) %>%
        dplyr::arrange(tri_out) %>% 
        dplyr::select(-estimate, -conf_low, -conf_high) %>%
        tidyr::pivot_wider(
          id_cols = c(child_sex, exposure, N, outcome, tri_out, term, est_ci),
          names_from = outcome,
          values_from = c(est_ci, N, p_value, p_value_int, p_value_main_eff)
        ) %>%
        dplyr::select(child_sex,
                      exposure, 
                      tri_out, 
                      term,
                      dplyr::contains("head"), 
                      dplyr::contains("abdominal"), 
                      dplyr::contains("biparietal"), 
                      dplyr::contains("femur"),
                      dplyr::contains("birth_weight"), 
                      dplyr::contains("birth_length"))
    } else {
      
      tab <- processed_model_output %>%
        dplyr::select(exposure, 
                      outcome, 
                      tri_out, 
                      term,
                      estimate, 
                      conf_low, 
                      conf_high, 
                      p_value, 
                      p_value_int,
                      p_value_main_eff,
                      stat_main_eff,
                      N) %>%
        dplyr::mutate(tri_out = factor(tri_out,
                                       levels = c("US2", "US3", "birth")),
                      est_ci = stringr::str_c(format(round(estimate, 2), nsmall = 2), " (", 
                                              format(round(conf_low, 2), nsmall = 2), "; ", 
                                              format(round(conf_high, 2), nsmall = 2), ")"),
                      p_value = as.character(signif(as.numeric(round(p_value, 4)), digits = 1)),
                      p_value_main_eff = dplyr::if_else(p_value_main_eff < 0.1, 
                                                        stringr::str_c("F = ",
                                                                       format(round(stat_main_eff, 1), nsmall = 1), 
                                                                       " (p-value = ", 
                                                                       as.character(signif(round(p_value_main_eff, 4), digits = 1)),
                                                                       ")"),
                                                        as.character(signif(round(p_value_main_eff, 2), digits = 1))),
                      int_sign = dplyr::if_else(p_value_int < 0.2, as.character(signif(round(p_value_int, 2), digits = 1)), "No"),
                      N = as.integer(N)
        ) %>%
        dplyr::arrange(tri_out) %>% 
        dplyr::select(-estimate, -conf_low, -conf_high) %>%
        tidyr::pivot_wider(
          id_cols = c(exposure, N, outcome, tri_out, term, est_ci),
          names_from = outcome,
          values_from = c(est_ci, N, p_value, int_sign, p_value_main_eff)
        ) %>%
        dplyr::select(exposure, 
                      tri_out, 
                      term,
                      dplyr::contains("head"), 
                      dplyr::contains("abdominal"), 
                      dplyr::contains("biparietal"), 
                      dplyr::contains("femur"),
                      dplyr::contains("birth_weight"), 
                      dplyr::contains("birth_length"))
    }
    
  } else {
    
    if (isTRUE(sex)) {
      
      tab <- processed_model_output %>%
        dplyr::select(child_sex,
                      exposure, 
                      outcome, 
                      tri_out, 
                      term,
                      estimate, 
                      conf_low, 
                      conf_high, 
                      p_value_int,
                      p_value, 
                      N) %>%
        dplyr::mutate(tri_out = factor(tri_out,
                                       levels = c("US2", "US3", "birth")),
                      est_ci = stringr::str_c(format(round(estimate, 2), nsmall = 2), " (", 
                                              format(round(conf_low, 2), nsmall = 2), "; ", 
                                              format(round(conf_high, 2), nsmall = 2), ")"),
                      p_value = as.character(signif(as.numeric(round(p_value, 4)), digits = 1)),
                      p_value_int = as.character(signif(round(p_value_int, 4), digits = 1)),
                      N = as.integer(N)
        ) %>%
        dplyr::arrange(tri_out) %>% 
        dplyr::select(-estimate, -conf_low, -conf_high) %>%
        tidyr::pivot_wider(
          id_cols = c(child_sex, exposure, N, outcome, tri_out, term, est_ci),
          names_from = outcome,
          values_from = c(est_ci, N, p_value, p_value_int)
        ) %>%
        dplyr::select(child_sex,
                      exposure, 
                      tri_out, 
                      term,
                      dplyr::contains("head"), 
                      dplyr::contains("abdominal"), 
                      dplyr::contains("biparietal"), 
                      dplyr::contains("femur"),
                      dplyr::contains("birth_weight"), 
                      dplyr::contains("birth_length"))
      
    } else {
      
      tab <- processed_model_output %>%
        dplyr::select(exposure, 
                      outcome, 
                      tri_out, 
                      term,
                      estimate, 
                      conf_low, 
                      conf_high, 
                      p_value, 
                      p_value_int,
                      N) %>%
        dplyr::mutate(tri_out = factor(tri_out,
                                       levels = c("US2", "US3", "birth")),
                      est_ci = stringr::str_c(format(round(estimate, 2), nsmall = 2), " (", 
                                              format(round(conf_low, 2), nsmall = 2), "; ", 
                                              format(round(conf_high, 2), nsmall = 2), ")"),
                      p_value = as.character(signif(as.numeric(round(p_value, 4)), digits = 1)),
                      int_sign = dplyr::if_else(p_value_int < 0.2, as.character(signif(round(p_value_int, 2), digits = 1)), "No"),
                      N = as.integer(N)
        ) %>%
        dplyr::arrange(tri_out) %>% 
        dplyr::select(-estimate, -conf_low, -conf_high) %>%
        tidyr::pivot_wider(
          id_cols = c(exposure, N, outcome, tri_out, term, est_ci),
          names_from = outcome,
          values_from = c(est_ci, N, p_value, int_sign)
        ) %>%
        dplyr::select(exposure, 
                      tri_out, 
                      term,
                      dplyr::contains("head"), 
                      dplyr::contains("abdominal"), 
                      dplyr::contains("biparietal"), 
                      dplyr::contains("femur"),
                      dplyr::contains("birth_weight"), 
                      dplyr::contains("birth_length"))
    }
  }
  
  return(tab)
}

