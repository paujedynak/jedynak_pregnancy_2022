# Pregnancy Exposure to Phenols and Anthropometric Measures in Gestation and at Birth

## Article info

Repository contains reproducible analyses for the paper: [Jedynak et al., 2022](https://pubmed.ncbi.nlm.nih.gov/35700189/).

## Analysis overview

Analyses depend on R (>= 4.2.0).

Code used to produce the files is found in the corresponding files: `jedynak_pregnancy_2022_Epidemiology.Rmd` and `jedynak_pregnancy_2022_supplement_Epidemiology.Rmd` (executable files).

The corresponding knitted files are: `phenols_growth.html` and 
`phenols_growth.html` (read-only files).

This analysis was performed under Windows 10 x64 (build 19041) using:    
* [R 4.2.2](https://cran.r-project.org/bin/windows/base) (2022-11-01)    
* [renv 0.15.0](https://cran.r-project.org/web/packages/renv/index.html) dependency management package


### Packages

All packages used in the analyses are saved in the `renv/library/R-4.2/x86_64-w64-mingw32` folder at the version used to produce these results, under control of the `renv` package manager. Re-running the analysis requires execution of `renv::restore()` which will upgrade/ downgrade user's packages to the versions used in the present study. This operation will modify the packages only locally (for the project), so it will not affect user's package library.


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


### results/ folder

It contains figures and tables subfolders as well as results obtained in the main analyses


### R/ folder

This folder contains all the source files and functions used for the analyses.


## Session info

```
R version 4.2.2 (2021-11-01)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19045)

Matrix products: default

locale:
[1] LC_COLLATE=English_Europe.utf8 
[2] LC_CTYPE=English_Europe.utf8   
[3] LC_MONETARY=English_Europe.utf8
[4] LC_NUMERIC=C                   
[5] LC_TIME=English_Europe.utf8    

attached base packages:
[1] stats     graphics  grDevices datasets 
[5] utils     methods   base     

other attached packages:
 [1] Hmisc_4.7-2     Formula_1.2-4  
 [3] survival_3.4-0  lattice_0.20-45
 [5] ggpubr_0.5.0    magrittr_2.0.3 
 [7] forcats_0.5.2   stringr_1.5.0  
 [9] dplyr_1.1.0     purrr_0.3.5    
[11] readr_2.1.3     tidyr_1.1.0    
[13] tibble_3.1.8    ggplot2_3.4.0  
[15] tidyverse_1.2.0

loaded via a namespace (and not attached):
 [1] colorspace_2.0-3    ggsignif_0.6.4     
 [3] deldir_1.0-6        ellipsis_0.3.2     
 [5] rprojroot_2.0.3     flextable_0.8.3    
 [7] htmlTable_2.4.1     base64enc_0.1-3    
 [9] rstudioapi_0.14     mice_3.15.0        
[11] farver_2.1.0        bit64_4.0.5        
[13] fansi_1.0.3         lubridate_1.9.0    
[15] xml2_1.3.3          splines_4.2.3      
[17] knitr_1.41          jsonlite_1.8.4     
[19] broom_0.8.0         cluster_2.1.4      
[21] png_0.1-8           compiler_4.2.3     
[23] httr_1.4.4          backports_1.4.1    
[25] Matrix_1.5-3        fastmap_1.1.0      
[27] cli_3.4.1           htmltools_0.5.3    
[29] tools_4.2.3         gtable_0.3.1       
[31] glue_1.6.2          Rcpp_1.0.8.3       
[33] carData_3.0-5       cellranger_1.1.0   
[35] vctrs_0.5.2         writexl_1.4.1      
[37] svglite_2.1.0       xfun_0.35          
[39] rvest_1.0.3         timechange_0.1.1   
[41] lifecycle_1.0.3     renv_0.15.0        
[43] rstatix_0.7.1       scales_1.2.1       
[45] vroom_1.6.0         ragg_1.2.5         
[47] hms_1.1.2           parallel_4.2.3     
[49] RColorBrewer_1.1-3  HardyWeinberg_1.7.5
[51] yaml_2.3.6          gridExtra_2.3      
[53] gdtools_0.2.4       rpart_4.1.19       
[55] latticeExtra_0.6-30 stringi_1.7.6      
[57] checkmate_2.1.0     zip_2.2.2          
[59] truncnorm_1.0-8     chron_2.3-58       
[61] rlang_1.0.6         pkgconfig_2.0.3    
[63] systemfonts_1.0.4   Rsolnp_1.16        
[65] evaluate_0.18       labeling_0.4.2     
[67] compareGroups_4.6.0 htmlwidgets_1.5.4  
[69] bit_4.0.5           tidyselect_1.2.0   
[71] here_1.0.1          R6_2.5.1           
[73] generics_0.1.2      pillar_1.8.1       
[75] haven_2.5.1         foreign_0.8-84     
[77] withr_2.5.0         abind_1.4-5        
[79] nnet_7.3-18         modelr_0.1.8       
[81] crayon_1.5.2        car_3.0-12         
[83] uuid_1.1-0          interp_1.1-3       
[85] utf8_1.2.2          tzdb_0.3.0         
[87] rmarkdown_2.18      officer_0.4.4      
[89] jpeg_0.1-10         grid_4.2.3         
[91] readxl_1.4.1        data.table_1.14.6  
[93] digest_0.6.30       webshot_0.5.4      
[95] textshaping_0.3.6   munsell_0.5.0      
[97] viridisLite_0.4.1   kableExtra_1.3.4 
```

