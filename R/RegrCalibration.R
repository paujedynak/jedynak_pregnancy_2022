#' Prepare data taking into account partial replicates
#'
#' @param expo  A string specifying a name of exposure variables
#' @param expo_rep A string specifying a name of exposure with "_rep" added top distinguish from the original exposure name
#' @param data_restr A data.frame restricted to outcome, confounders, expo (exposure), and expo_rep (exposure replicate)
#' @param outcome A string specifying a name of outcome variable
#' @param confounders A character vector listing names of confounder variables
#' @param confounders_dummy A character vector listing names of confounder variables 
#' including names of newly created dummy variables
#'
#' @return A data.frame containing exposures, their replicates, confounders and outcomes. The exposures are swapped with
#' replicates to minimize the number of missing values
#' @export
#' @import dplyr
#' @importFrom rlang sym
#' @importFrom tidyr drop_na

RegrCalDataPrep <- function(expo, 
                            expo_rep,
                            data_restr, 
                            outcome,
                            confounders, 
                            confounders_dummy) {
  
  data_restr <- data_restr %>%
    
    # In theory, for the MLE method, the order of the replicates does not matter.
    # This means, that R1 could be swapped with R2. Then partial replicates study is possible where
    # R1 is a complete case but R2 may be missing. This means that I could replace missing R1 with
    # R2 (then R2 would become missing) to have the higher n (as now I only include complete cases
    # for both R1 and R2)
    
    dplyr::mutate(expo_temp = !!(rlang::sym(expo)),
                  !!(expo) := dplyr::coalesce(!!(rlang::sym(expo)), !!(rlang::sym(expo_rep))))
  
  # Now replace the R2 with NA everywhere where R2 replaced R1
  for (i in seq_along(data_restr$expo_temp)) {
    if (is.na(data_restr$expo_temp[i])) {
      data_restr[i, expo_rep] <- NA
    }
  }
  
  data_restr <- data_restr %>% 
    dplyr::select(-expo_temp) %>% 
    tidyr::drop_na(-(!!(expo_rep)))
  
  return(data_restr)
}


#' Obtain regression estimated corrected using the regression calibration from the mecor package
#' (single outcome, single exposure)
#'
#' @param outcome A string specifying a name of outcome variable
#' @param expo A string specifying a name of exposure variables
#' @param data A data.frame in a wide format containing exposure, confounder, and outcome variables
#' @param confounders A character vector listing names of confounder variables
#' @param confounders_dummy A character vector listing names of confounder variables 
#' including names of newly created dummy variables
#' @param method Inherited from mecor::mecor 
#' @param seed A scalar defining a seed
#' @param B A scalar defining the number of bootstraps, inherited from mecor::mecor 
#' @param US2 A logical indicating if the T2~US2 data is processed (default = FALSE)
#'
#' @return A data.frame containing statistics for regression with corrected beta estimates and CIs
#' for single outcome and single exposure
#' @export
#' @import dplyr
#' @importFrom fastDummies dummy_cols
#' @importFrom stats as.formula
#' @importFrom mecor mecor
#' 
RegrCalibrationMecor <- function(expo,
                                 outcome,
                                 data, 
                                 confounders, 
                                 confounders_dummy,
                                 method, 
                                 seed,
                                 B, 
                                 US2) {
  
  expo_rep <- paste0(expo, "_rep")

  data_sel <- data_restr <- dplyr::select(data, tidyselect::all_of(c(outcome, confounders, expo, expo_rep)))

  # If working on data other than t2 -> US2, use info from both t2 and t3 (coalesce)
  if (isFALSE(US2)) {

    data_sel <- RegrCalDataPrep(expo = expo,
                                expo_rep = expo_rep,
                                data_restr = data_restr,
                                outcome = outcome,
                                confounders = confounders,
                                confounders_dummy = confounders_dummy)

  } else {

    # If working on t2 -> US2 data, use info from only t2
    data_sel <- data_sel[!is.na(data_sel[, expo]), ]
    data_sel <- data_sel[!is.na(data_sel[, outcome]), ]

  }

  data_sel <- data_sel %>%

    # Change categorical variables to dummies so they are correctly recognized by the mecor functions
    fastDummies::dummy_cols(remove_first_dummy = TRUE,
                            remove_selected_columns = TRUE)

  formula_rc <- paste(paste0(outcome,
                             " ~ mecor::MeasError(",
                             expo,
                             ", replicate = ",
                             expo_rep,
                             ") + ",
                             paste0(confounders_dummy,
                                    collapse = " + "))) %>%
    stats::as.formula()

  print(outcome)
  print(expo)

  set.seed(seed)
  list_expo <- mecor::mecor(formula = formula_rc,
                            data = data_sel,
                            method = method,
                            B = B)

  summary_res <- summary(object = list_expo)

  uncorr <- c(summary_res$uc$ci[2, ],
              "p_value" = summary_res$uc$coefficients[2, 4],
              "p_value_int" = 1)

  # Since the BS method does not provide the p-values for the CIs, generate
  # p-values basing on the fact if the CI contains 0 or not (just to be able
  # to make plots, but this is not the final solution as these p values may not be accurate)
  p_value <- ifelse((summary_res$c$ci[2, 4] > 0 & summary_res$c$ci[2, 5] > 0) |
                      (summary_res$c$ci[2, 4] < 0 & summary_res$c$ci[2, 5] < 0), 0, 1)

  corr <- c(summary_res$c$ci[2, c(1, 4:5)],
            "p_value" = p_value,
            "p_value_int" = 1)

  res <- data.frame(uncorr = uncorr,
                    corr = corr) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::transmute(estimate = Estimate,
                     conf_low = LCI,
                     conf_high = UCI,
                     est_ci = paste0(format(round(Estimate, 3), nsmall = 3),
                                     " (",
                                     format(round(LCI, 3), nsmall = 3),
                                     ";",
                                     format(round(UCI, 3), nsmall = 3),
                                     ")"),
                     N = dim(data_sel)[1],
                     p_value = p_value,
                     p_value_int = p_value_int)

  return(res)
}

#' Obtain regression estimated corrected using the regression calibration from the mecor package
#' (single outcome, several exposures)
#'
#' @param outcome A string specifying a name of outcome variable
#' @param exposures A character vector listing names of exposure variables
#' @param data A data.frame in a wide format containing exposure, confounder, and outcome variables
#' @param confounders A character vector listing names of confounder variables
#' @param confounders_dummy A character vector listing names of confounder variables 
#' including names of newly created dummy variables
#' @param method Inherited from mecor::mecor 
#' @param seed A scalar defining a seed
#' @param B A scalar defining the number of bootstraps, inherited from mecor::mecor 
#' @param US2 A logical indicating if the T2~US2 data is processed (default = FALSE)
#'
#' @return A data.frame containing statistics for regression with corrected beta estimates and CIs
#' for single outcome and multiple exposures
#' @export
#' 

RegrCalibrationMecorPerExpo <- function(exposures, 
                                        outcome,
                                        data,
                                        confounders, 
                                        confounders_dummy,
                                        method, 
                                        seed,
                                        B, 
                                        US2) {
  
  res_expo <- sapply(X = exposures,
                     FUN = RegrCalibrationMecor, 
                     outcome = outcome,
                     data = data,
                     confounders = confounders, 
                     confounders_dummy = confounders_dummy,
                     method = method, 
                     seed = seed,
                     B = B, 
                     US2 = US2, 
                     USE.NAMES = TRUE, 
                     simplify = FALSE) %>% 
    do.call(rbind, .)
  
  return(res_expo)
}

#' Obtain regression estimated corrected using the regression calibration from the mecor package
#'
#' @param outcomes A character vector listing names of outcome variables
#' @param exposures A character vector listing names of exposure variables
#' @param data A data.frame in a wide format containing exposure, confounder, and outcome variables
#' @param confounders A character vector listing names of confounder variables
#' @param confounders_dummy A character vector listing names of confounder variables 
#' including names of newly created dummy variables
#' @param method Inherited from mecor::mecor 
#' @param seed A scalar defining a seed
#' @param B A scalar defining the number of bootstraps, inherited from mecor::mecor 
#' @param US2 A logical indicating if the T2~US2 data is processed (default = FALSE)
#'
#' @return A data.frame containing statistics for regression with corrected beta estimates and CIs
#' for multiple outcomes and exposures
#' @export
#' @import dplyr
#' @importFrom stringr str_split

RegrCalibration <- function(exposures,
                            outcomes,
                            data,
                            confounders, 
                            confounders_dummy,
                            method, 
                            seed,
                            B = 100, 
                            US2 = FALSE) {
  
  list_outcome <- sapply(X = outcomes,
                         FUN = RegrCalibrationMecorPerExpo, 
                         exposures = exposures, 
                         data = data,
                         confounders = confounders, 
                         confounders_dummy = confounders_dummy,
                         method = method, 
                         seed = seed,
                         B = B, 
                         US2 = US2, 
                         USE.NAMES = TRUE, 
                         simplify = FALSE) %>% 
    do.call(rbind, .)
  
  extra_info <- do.call(rbind, stringr::str_split(rownames(list_outcome), "[.]"))
  
  list_outcome_final <- list_outcome %>% 
    dplyr::mutate(exp = factor(extra_info[, 2]),
                  calibration = factor(extra_info[, 3]),
                  outcome = factor(extra_info[, 1])) %>% 
    dplyr::mutate_at(dplyr::vars(estimate:conf_high, p_value, p_value_int), 
                     as.numeric) %>% 
    dplyr::arrange(exp) %>% 
    dplyr::select(exp, outcome, N, dplyr::everything())
  
  rownames(list_outcome_final) <- NULL
  
  return(list_outcome_final)
}





