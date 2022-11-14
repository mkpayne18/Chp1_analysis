
## Molly Payne
## 5/25/2022

**new_response_var.rds** was created in **dead_count_analysis.R** and indicates the total effective number of strays by stream-year, which is the number of hatchery fish detected / proportion of total dead sampled (adjusts for bias in response variable). The Avg_number_strays is the Total_effective_strays divided by the number of surveys and is what I used to substitute my response variable after May 2022 <br>

**best_model.rds** was created in **03_fit_NB_model.R** and is a glmer.nb() object of the best, or top candidate model selected from my model fitting process <br>

**preds_and_obs_vals_unaveraged.rds** This object is called "bm1_pred" in the script **03_fit_NB_model.R** where it was created. It is a dataframe containing the observed average [effective] number of strays in a stream and the model predicted value for each stream-year <br>

**mean_pred_and_obs_vals.rds** This object is called "mean_bm1_pred" in **03_fit_NB_model.R** and gives the mean predicted and observed average [effective] number of strays for streams averaged across years. So, you get one pred and obs value for each stream across all years sampled, not individual values for each year. I.e., this object is the same as **preds_and_obs_vals_unaveraged.rds** but the pred and obs values are averaged over all years a stream was sampled <br>

**flow_plus.rds** This object was created in **CVflow_side_analysis.R** and gives the CV_flow values for all streams linked to the other watershed data given in Sergeant et al. 2020 for those streams. I used this to create the CV_flow plot in **05_figures.R** <br>

**stray_dat_scaled.rds** This object was created in **03_fit_NB_model.R** and gives the final tailored dataset used for modeling. All covariates are z-score scaled <br>

**survey_data8.rds** This object was created in **dead_count_analysis.R** and is the final tailored version of **data/survey_data.csv**. It gives stream survey information and importantly, the total dead count and number of specimens, or fish sampled. I used these data to correct model response variable bias and create the **new_response_var.rds** object. Note that in **survey_data8.rds** the dead count is assumed to be new dead (previously unsampled fish only), not previously unsampled + previously sampled. I dealt with this in the **dead_count_analysis.R** script <br>

**prop_1.rds** was created in **dead_count_analysis.R** and is the subset of surveys from **data/survey_data.csv** where the proportion of fish sampled of the total dead was less than or equal to 1 (some streams had "live" fish sampled, so their proportion was greater than 1) <br>

**NB_mod_dredge.rds** was created in **03_fit_NB_mod.R** and is the object that lists all models and their AICc information that were considered. I used this object to choose the final model within the model fitting script <br>
