scaleFUN <- function(x) sprintf("%.1f", x)

#' Prepare  analysis results for plotting
#'
#' @param model_cont A dataframe containing results of linear regressions for continuous exposures
#' @param model_cat A dataframe containing results of linear regressions for categorical exposures
#' @param phthalates A logical indicating if the plots will be for phthalates (default = FALSE)
#' @import stringr
#' @import dplyr
#'
#' @return A dataframe ready for plotting

.PreprocessOutputPlots <- function(model_cont,
                                   model_cat,
                                   phthalates = FALSE) {
  
  # Transform variables into factors so they can be plotted in the right order
  model_output <- dplyr::bind_rows(model_cont, model_cat) %>% 
    dplyr::mutate(term = factor(term, 
                                levels = c("exp_val", "exp_valLOD-LOQ", "exp_val>LOQ"),
                                labels = c("Continuous", "Cat. 2", "Cat. 3")),
                  trimester = factor(trimester, 
                                     levels = c("US2", "US3", "birth"),
                                     labels = c("trimester 2", "trimester 3", "birth")))
  
  # combine outcome and trimester
  
  if (isTRUE(phthalates)) {
    
    model_output <- model_output %>%
      dplyr::rename(tri_out = trimester) %>%
      dplyr::mutate(
        outcome_period = stringr::str_c(outcome, tri_out, sep = " "),
        compound = dplyr::case_when(
          stringr::str_detect(exp, "MBzP") ~ "MBzP",
          stringr::str_detect(exp, "MEP") ~ "MEP",
          stringr::str_detect(exp, "MiBP") ~ "MiBP",
          stringr::str_detect(exp, "MnBP") ~ "MnBP",
          
          stringr::str_detect(exp, "ohMPHP") ~ "ohMPHP",
          
          # stringr::str_detect(exp, "MECPP") ~ "MECPP",
          # stringr::str_detect(exp, "MEHHP") ~ "MEHHP",
          # stringr::str_detect(exp, "MEHP") ~ "MEHP",
          # stringr::str_detect(exp, "MEOHP") ~ "MEOHP",
          # stringr::str_detect(exp, "MMCHP") ~ "MMCHP",
          stringr::str_detect(exp, "DEHP") ~ paste0("\u03a3", "DEHP"),
          
          # stringr::str_detect(exp, "cxMiNP") ~ "cxMiNP",
          # stringr::str_detect(exp, "ohMiNP") ~ "ohMiNP",
          # stringr::str_detect(exp, "oxoMiNP") ~ "oxoMiNP",
          stringr::str_detect(exp, "DiNP") ~ paste0("\u03a3", "DiNP"),
          
          # stringr::str_detect(exp, "ohMINCH") ~ "ohMINCH",
          # stringr::str_detect(exp, "oxoMINCH") ~ "oxoMINCH",
          stringr::str_detect(exp, "DINCH") ~ paste0("\u03a3", "DINCH")
        ),
        
        compound = factor(compound,
                          levels = c("MBzP", "MEP", "MiBP", "MnBP", 
                                     # "MECPP", "MEHHP", "MEHP", "MEOHP", "MMCHP", 
                                     "ohMPHP",
                                     paste0("\u03a3", "DEHP"),
                                     # "cxMiNP", "ohMiNP", "oxoMiNP", 
                                     paste0("\u03a3", "DiNP"),
                                     # "ohMINCH", "oxoMINCH", 
                                     paste0("\u03a3", "DINCH")))
      )
    
  } else {
    
    model_output <- model_output %>%
      dplyr::rename(tri_out = trimester) %>%
      dplyr::mutate(
        outcome_period = stringr::str_c(outcome, tri_out, sep = " "),
        compound = dplyr::case_when(
          stringr::str_detect(exp, "BPA") ~ "Bisphenol A",
          stringr::str_detect(exp, "BPS") ~ "Bisphenol S",
          stringr::str_detect(exp, "TCS") ~ "Triclosan",
          stringr::str_detect(exp, "BP3") ~ "Benzophenone-3",
          stringr::str_detect(exp, "BUPA") ~ "Butylparaben",
          stringr::str_detect(exp, "ETPA") ~ "Ethylparaben",
          stringr::str_detect(exp, "MEPA") ~ "Methylparaben",
          stringr::str_detect(exp, "PRPA") ~ "Propylparaben"
        ),
        compound = factor(compound, 
                          levels = c("Bisphenol A",
                                     "Bisphenol S",
                                     "Triclosan",
                                     "Benzophenone-3",
                                     "Butylparaben",
                                     "Ethylparaben",
                                     "Methylparaben",
                                     "Propylparaben")
        ))
  }
  
  # order and rename outcomes
  model_output <- model_output %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      outcome = dplyr::recode(outcome,
                              abdominal_circumference = "Abdominal circumference",
                              birth_length = "Length",
                              birth_weight = "Weight",
                              femur_length = "Femur length",
                              head_circumference = "Head circumference",
                              head_circumference_b = "Head circumference",
                              biparietal_diameter = "Biparietal diameter"),
      outcome_period = stringr::str_c(outcome, tri_out, sep = " at "),
      outcome_period = forcats::fct_rev(factor(outcome_period, 
                                               # levels = c("Abdominal circumference at trimester 2",
                                               #                            "Abdominal circumference at trimester 3",
                                               #                            "Biparietal diameter at trimester 2",
                                               #                            "Biparietal diameter at trimester 3",
                                               #                            "Femur length at trimester 2",
                                               #                            "Femur length at trimester 3",
                                               #                            "Head circumference at trimester 2",
                                               #                            "Head circumference at trimester 3",
                                               #                            "Head circumference at birth",
                                               #                            "Weight at birth",
                                               #                            "Length at birth")
                                               
                                               levels = c("Abdominal circumference at trimester 2",
                                                          "Biparietal diameter at trimester 2",
                                                          "Femur length at trimester 2",
                                                          "Head circumference at trimester 2",
                                                          "Abdominal circumference at trimester 3",
                                                          "Biparietal diameter at trimester 3",
                                                          "Femur length at trimester 3",
                                                          "Head circumference at trimester 3",
                                                          "Head circumference at birth",
                                                          "Weight at birth",
                                                          "Length at birth")
      ))
    )
  
  return(model_output)
}

#' Plot regression results for each phenol, trimester and outcome
#'
#' @param plot_title A string defining plot title
#' @param sex A logical indicating if the plots will be for sex-stratififed analysis (default = FALSE)
#' @param phthalates A logical indicating if the plots will be for phthalates (default = FALSE)
#' @param ncol An integer defining in how many columns the plot will be divided
#' @param model_cont A dataframe containing results of linear regressions for continuous exposures
#' @param model_cat A dataframe containing results of linear regressions for categorical exposures
#' @import ggplot2
#'
#' @return A regression estimates plot faceted on each phenol
#' @export

PlotModel <- function(model_cont,
                      model_cat, 
                      plot_title,
                      sex = FALSE,
                      phthalates = FALSE,
                      ncol) {
  
  # pre process model output
  plot_data <- .PreprocessOutputPlots(model_cont,
                                      model_cat,
                                      phthalates) # %>% 
  # dplyr::mutate(p_col = ifelse(p_value < 0.05, "p < 0.05", "p >= 0.05"),
  #               p_col = factor(p_col, levels = c("p >= 0.05", "p < 0.05")))
  
  # plot
  
  if (isTRUE(sex)) {
    
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = outcome_period,
                                                 y = estimate,
                                                 shape = term,
                                                 color = child_sex)) +
      ggplot2::geom_point(size = 2,
                          position = ggplot2::position_dodge(width = -0.65)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = conf_low,
                                          ymax = conf_high),
                             width = 0.2, 
                             position = ggplot2::position_dodge(width = -0.65))
    
  } else {
    
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = outcome_period,
                                                 y = estimate,
                                                 color = term)) +
      ggplot2::geom_point(ggplot2::aes(shape = term),
                          size = 2,
                          position = ggplot2::position_dodge(width = -0.65),
                          color = "black")
  }
  
  p <- p +
    ggplot2::geom_hline(yintercept = 0, 
                        linetype = "dashed", 
                        color = "gray30", 
                        size = 0.5) +
    ggplot2::facet_wrap(. ~ compound,
                        scales = "free_x",
                        ncol = ncol) +
    ggplot2::coord_flip() +
    ggplot2::ylab(expression(paste(beta, " +/-(95% CI)"))) +
    ggplot2::xlab("") +
    ggplot2::ggtitle(plot_title) + 
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(labels = scaleFUN)
  
  if (isTRUE(phthalates)) {
    
    if (isTRUE(sex)) {
      
      p <- p +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = conf_low,
                                            ymax = conf_high),
                               width = 0.2,
                               position = ggplot2::position_dodge(width = -0.65))
      
    } else {
      
      p <- p +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = conf_low,
                                            ymax = conf_high),
                               width = 0.2,
                               position = ggplot2::position_dodge(width = -0.65),
                               color = "black")
    }
    
    p <- p +
      ggplot2::theme(legend.position = "none",
                     text = ggplot2::element_text(size = 13))
    
  } else {
    
    p <- p +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = conf_low,
                                          ymax = conf_high),
                             width = 0.2,
                             position = ggplot2::position_dodge(width = -0.65)) +
      ggplot2::theme(legend.title = ggplot2::element_blank(),
                     panel.border = ggplot2::element_rect(color = "black", fill = NA),
                     panel.grid.major.x = ggplot2::element_blank(),
                     # panel.grid.major.y = element_line(color = "darkgray", size = 0.1, linetype = 3),
                     axis.text.x = ggplot2::element_text(size = 8),
                     axis.text.y = ggplot2::element_text(size = 11),
                     strip.text.x = ggplot2::element_text(size = 11),
                     text = ggplot2::element_text(size = 11))
  }
  
  if (!isTRUE(sex) & !isTRUE(phthalates)) {
    p <- p +
      ggplot2::scale_color_manual(labels = c("Continuous", "Cat. 2", "Cat. 3"), 
                                  values = c("black", "black", "black"))
  }
  
  return(p)
}
