---
title: "Pregnancy exposure to phenols and anthropometric measures in gestation and at birth"
---

## Article info

Repository contains reproducible analyses for the paper: [Jedynak et al., 2022](https://pubmed.ncbi.nlm.nih.gov/35700189).

## Analysis overview

Analyses depend on R (>= 3.5.0).

Code used to produce the files is found in the corresponding files: `jedynak_pregnancy_2022_Epidemiology.Rmd` and `jedynak_pregnancy_2022_supplement_Epidemiology.Rmd` (executable files).

The corresponding knitted files are: `jedynak_pregnancy_2022_Epidemiology.html` and 
`jedynak_pregnancy_2022_supplement_Epidemiology.html` (read-only files).

This analysis was performed under Windows 10 x64 (build 19041) using:    
* [R 4.1.2](https://cran.r-project.org/bin/windows/base) (2021-11-01)    
* [renv 0.15.0](https://cran.r-project.org/web/packages/renv/index.html) dependency management package


### Packages

All packages used in the analyses are saved in the `renv/library/R-4.1/x86_64-w64-mingw32` folder at the version used to produce these results, under control of the `renv` package manager. Re-running the analysis requires execution of `renv::restore()` which will upgrade/ downgrade user's packages to the versions used in the present study. This operation will modify the packages only locally (for the project), so it will not affect user's package library.


### To run

Re-running the analysis requires an additional `data/raw_data` folder that is not shared here. These data can only be provided upon request and after approval by the SEPAGES consortium (contact: [Sarah Lyon-Caen](sarah.lyon-caen@univ-grenoble-alpes.fr)). Running two scripts: `jedynak_pregnancy_2022_Epidemiology.Rmd` and `jedynak_pregnancy_2022_supplement_Epidemiology.Rmd` will allow to fully reproduce the analysis.


## Repo organization

### data/raw_data folder

Analysis input data-files are not made available as they contain sensitive information. The analysis input data files would be:

* `bdd_grossesse_v4_1.dta` = dataset containing information on the anthropometric measures (digital ultrasound and measures collected at birth), n = 484
* `phenols_phthalates_pregnancy_mr_2020-03-26_448.csv` = dataset containing information on phenols exposure, n = 479
* `tobaccopregnancy_ag_20201218_16_copy.dta` = dataset containing information on smoking during pregnancy, n = 484
* `donnees_echo_saisies_quest_mt1caj1.dta` = dataset containing manual ultrasound data, n = 38
* `limits_data_2022-01-24.csv` =  dataset containing information on LOD and LOQ values for each phenol


### results/phenols folder

It contains figures and tables subfolders as well as results obtained in the main analyses:


### R/ folder

This folder contains all the source files and functions used for the analyses.


## Session info

```
R version 4.1.2 (2021-11-01)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19044)

Matrix products: default

locale:
[1] LC_COLLATE=English_Europe.65001 
[2] LC_CTYPE=C                      
[3] LC_MONETARY=English_Europe.65001
[4] LC_NUMERIC=C                    
[5] LC_TIME=English_Europe.65001    
system code page: 65001

attached base packages:
[1] stats     graphics  grDevices datasets  utils     methods  
[7] base     

other attached packages:
[1] forcats_0.5.1   stringr_1.4.0   dplyr_1.0.7    
[4] purrr_0.3.4     readr_2.1.1     tidyr_1.1.4    
[7] tibble_3.1.6    ggplot2_3.3.5   tidyverse_1.3.1

loaded via a namespace (and not attached):
 [1] fs_1.5.2            lubridate_1.8.0    
 [3] bit64_4.0.5         webshot_0.5.2      
 [5] httr_1.4.2          rprojroot_2.0.2    
 [7] tools_4.1.2         backports_1.4.1    
 [9] utf8_1.2.2          R6_2.5.1           
[11] DBI_1.1.2           colorspace_2.0-2   
[13] nnet_7.3-16         withr_2.4.3        
[15] tidyselect_1.1.1    bit_4.0.4          
[17] compiler_4.1.2      chron_2.3-56       
[19] textshaping_0.3.6   cli_3.1.0          
[21] rvest_1.0.2         HardyWeinberg_1.7.4
[23] flextable_0.6.10    mice_3.14.0        
[25] xml2_1.3.3          officer_0.4.1      
[27] labeling_0.4.2      scales_1.1.1       
[29] systemfonts_1.0.3   digest_0.6.29      
[31] rmarkdown_2.11      svglite_2.0.0      
[33] base64enc_0.1-3     pkgconfig_2.0.3    
[35] htmltools_0.5.2     sessioninfo_1.2.2  
[37] dbplyr_2.1.1        fastmap_1.1.0      
[39] rlang_0.4.12        readxl_1.3.1       
[41] rstudioapi_0.13     farver_2.1.0       
[43] generics_0.1.1      jsonlite_1.7.2     
[45] vroom_1.5.7         car_3.0-12         
[47] zip_2.2.0           magrittr_2.0.1     
[49] kableExtra_1.3.4    Matrix_1.4-0       
[51] Rcpp_1.0.7          munsell_0.5.0      
[53] fansi_1.0.0         abind_1.4-5        
[55] gdtools_0.2.3       lifecycle_1.0.1    
[57] stringi_1.7.6       yaml_2.2.1         
[59] carData_3.0-5       grid_4.1.2         
[61] parallel_4.1.2      crayon_1.4.2       
[63] lattice_0.20-45     haven_2.4.3        
[65] splines_4.1.2       hms_1.1.1          
[67] knitr_1.37          pillar_1.6.4       
[69] uuid_1.0-3          compareGroups_4.5.1
[71] reprex_2.0.1        glue_1.6.0         
[73] evaluate_0.14       data.table_1.14.2  
[75] renv_0.15.0         modelr_0.1.8       
[77] vctrs_0.3.8         tzdb_0.2.0         
[79] cellranger_1.1.0    gtable_0.3.0       
[81] assertthat_0.2.1    xfun_0.29          
[83] broom_0.7.11        Rsolnp_1.16        
[85] ragg_1.2.1          survival_3.2-13    
[87] viridisLite_0.4.0   truncnorm_1.0-8    
[89] writexl_1.4.0       ellipsis_0.3.2     
[91] here_1.0.1        
```

