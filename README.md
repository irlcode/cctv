# Data and Code For `A tale of four cities: Exploring environmental characteristics of CCTV equipment placement`

This is the code and data to replicate the analysis in [Serebrennikov, Skougarevskiy (2023)](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4106472).

## Repository structure

```
/
├── code
    ├── 1._Parameter_searching_and_cross-validation.R   # Parameters search, block cross-validation, final estimation
    ├── 2._SHAP_Analysis.R                              # SHAP-analysis and Plots based on it.
    ├── Table_1_code.R                                  # Produce Table 1 
    ├── Figure_1_code.R                                 # Produce Figure 1
    ├── 1a._Robustness_check_Parameter_searching_and_cross-validation.R # Robustness check for city centers
    └── 2a._Robustness_check_SHAP_Analysis.R # Robustness check for city centers
├── data
    ├── BE_100_catboost_best_params.rds                 # Best parameters for Brussels' model
    ├── BE_all_osm_objects_Freqtab.RDS                  # Frequency tab for Brussels
    ├── BE_buffer_long_100_RandSamp_GEOM.rds            # Brussels geodata for block cross-validation
    ├── BE_buffer_wide_100_RandSamp.csv                 # Brussels data for analysis
    ├── data_all_catboost_buffer_100.rdata              # Brussels prepared data
    ├── feature_importance_catboost_buffer_100.rdata    # Data for feature importance plot
    ├── FR_100_catboost_best_params.rds                 # Best parameters for Paris' model
    ├── FR_all_osm_objects_Freqtab.RDS                  # Frequency tab for Paris
    ├── FR_buffer_long_100_RandSamp_GEOM.rds            # Paris data for analysis
    ├── FR_buffer_wide_100_RandSamp.csv                 # Paris prepared data
    ├── RU_100_catboost_best_params.rds                 # Best parameters for Moscow's model
    ├── RU_all_osm_objects_Freqtab.RDS                  # Frequency tab for Moscow
    ├── RU_buffer_long_100_RandSamp_GEOM.rds            # Moscow data for analysis
    ├── RU_buffer_wide_100_RandSamp.csv                 # Moscow prepared data
    ├── SC_100_catboost_best_params.rds                 # Best parameters for Edinburgh's model
    ├── SC_all_osm_objects_Freqtab.RDS                  # Frequency tab for Edinburgh
    ├── SC_buffer_long_100_RandSamp_GEOM.rds            # Edinburgh data for analysis
    └── SC_buffer_wide_100_RandSamp.csv                 # Edinburgh prepared data
```

## Licence
<a rel="license" href="https://creativecommons.org/licenses/by-nc-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br />
Creative Commons License Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0).

Copyright © the respective contributors, as shown by the `AUTHORS` file.

## Contacts
Dmitriy Serebrennikov, assosiated researcher IRL EUSP

serebrennikov.dmtr@eu.spb.ru
