## Contains scripts used to tailor data, develop model and perform various supplemental analyses with the overall goal of developing a model to predict stream attractiveness to stray hatchery-origin chum salmon

### Molly Payne
### Main analysis scripts

____

**01_stray_data_load_and_clean.R** Load and tailor stray data (Master_dataset.csv),   which contains the number of strays and surveys by stream and year as well as the covariate data <br>

**02_EDA.R** Exploratory data analysis for response variable and possible covariates to predict the number of hatchery-origin chum salmon straying into streams <br>

**03_fit_NB_model.R** Fit GLMM with negative binomial error distribution to predict numbers of strays in streams. Perform preliminary diagnostics for global model, then proceed to choose final model based on AICc values <br>

**04_model_diagnostics.R** Complete diagnostics for best candidate model fit in 03_fit_NB_model.R <br>

**05_figures.R** Create figures for manuscript and thesis using results from scripts 01-04 (primarily 03_fit_NB_model.R) <br>

**06_tables.R** Create tables for manuscript and thesis using results from scripts 01-04 (primarily 03_fit_NB_model.R) <br>

**07_supplemental_materials.R** Create figures and tables for supplemental sections of manuscript and thesis <br>

**Scripts 01-07 all use relative paths which pull from a 'data' folder. CONTACT ME AT**
**MKPAYNE18@GMAIL.COM TO ACCESS DATA. Other scripts (below) may or not may not use**
**relative paths but data can be provided to run them if requested.**


### Other scripts

____
**Flow_dat_check.R** Undoubtedly this is the oldest script in this folder, since the covariates referenced within are transformed and I no longer do that in my model because transformation is unnecessary. In this script, I show that including the approx. 40% of streams in my dataset that have loosely estimated CV of flow covariate data does not affect the model results. I.e., if I exclude these streams and only fit the model with the streams that have very certain CV_flow data, the conclusions do not change. Therefore it is OK to include the estimated streams. Have a look at "Hydro_methods_detail.docx" within the ***Stream_Hydro_Flow*** folder in ***Data Sources*** for more explanation. <br>

**CV_flow_side_analysis.R** In thinking about what mechanism might explain the significant U-shaped quadratic relationship of CV_flow with my model response variable (average number of strays), I checked to see what other watershed characteristics might be associated with the streams that had the attractive CV_flow values in this script. The conclusion was that [attractive] high and low CVs of flow were the streams that were fed by snowmelt, hence the CV_flow effect might be a proxy for snowmelt water sources being attractive. I create a figure in 05_figures.R to show this.   <br>

**PCA_spatial_analysis.R** Run principal components analysis on latitude and longitude of streams, extract first PC and regress it against covariates to see if any of the covariates might be spatially autocorrelated based on stream location. The conclusion was that none of them appeared to be. <br>

**dead_count_analysis.R** This script was created in May 2022 after the issue was raised at my thesis defense that there might be bias in my response variable, whereby fewer total strays are detected in streams where the proportion of total dead that got sampled was not 100% In this script, I show that there is indeed bias and I adjust for it by inflating the number of strays detected in a survey by the inverse of the proportion sampled (so that streams where hatchery fish might have been missed bc not all fish were sampled were 'inflated' to have more strays). The main output of the script was an object containing the new model response variable to be used in the model in scripts 01-03. For a complete description of why we needed to account for model response variable bias and how this was done, see "docs/Bayes_intro&Model_devo.docx" <br>

**App.to_MasterDataset.csv** This is the script where I added the covariate data to the Master_dataset.csv. As with the "Data Sources" folder, this script was created before I had good Rproject management skills and therefore does not utilize relative paths. All you need to know is that at some point I created a .csv that tailored all of the data in the first 9 columns of the Master_dataset.csv from the data in 2008_2019_HW_data_copy.csv, and then in this script I added the corresponding covariate data to each stream-year. Covariate data came from the "Data Sources" folder <br>

**dead_count_additional_notes** -> not a script but a folder containing a couple of Rmd files that provide additional context or help with the dead_count_analysis.R script, which is a script that contains the analysis and methods for accounting for bias in the model response variable (which was not a simple task, hence the additional notes). For a complete description of why we needed to account for model response variable bias and how this was done, see "docs/Bayes_intro&Model_devo.docx" <br>




