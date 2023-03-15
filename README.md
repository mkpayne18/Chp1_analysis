## SEAK stream attractiveness to stray hatchery-origin chum salmon analysis

### Molly Payne

### 5/25/2022

Purpose:

To develop a modeling framework to predict the number of hatchery-origin chum salmon straying into (spawning in) streams in Southeast Alaska from 2008-2019 using stream characteristics as model covariates (chapter 1 of my MS thesis). All of the folders in this repository have their own README.md files that explain the contents (except for figs, which is self-explanatory). However, I note two of the folders which might warrant further explanation:


1. old_stuff is the first version of my repository containing all the files that I used for my analysis before improving my file organization. I’ve held onto them just in case they might need to be referenced at some point. See “Folder_Guide.md” within this folder for more information


2. Data Sources is the folder containing the raw data, tailored data, and all associated information about my model covariates and my response variable (find response variable information and raw raw response data in the “Stray_Data” folder within). There aren’t any official README.md files in this folder but you’ll find within each covariate folder (e.g., “Cons_Dens&Abundance”, “Release_Sites_Age”) that there are documents with “…Methods_Detail” in the title that are functionally READMEs for those folders. I have not reorganized Data Sources or created relative paths since I think this folder is too far gone to easily do that.

All you need to know is that raw covariate data, raw response (number of strays from stream surveys), and associated information about those data can be found in the Data Sources folder, and that the tailored versions of these data all went into data/Master_dataset.csv, which is what I used for modeling
The user may also find it helpful to know is that within the “docs” folder is a document called Bayes_intro&Model_dev.docx. This document was effectively my lab notebook for this entire analysis and is where I noted any changes to my process, things I was thinking about or that were suggested by co-authors, and probably the explanation for whatever else you might be wondering about when going through these files

Update March 2023: The data folder is no longer on Github since I am downgrading to a free (and public) Github account and hatchery data is proprietary. Please contact me to access data to run code