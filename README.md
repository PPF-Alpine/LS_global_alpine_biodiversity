# General info

This repo contains all R code used to process and analyse data for the outputs of the article "Contrasting patterns of alpine biodiversity across mountains and taxa" (Schultz et. al, under review)

## Main structure of the Repo

This repo follows a template structure for R projects developed by Ondrej Mottl (<https://ondrejmottl.github.io/>)

-   R / 01_Data_processing: Folder for sourcing, restructuring and joining data to create the species checklists
-   R / 02_Main_analyses: Folder where main (richness) analyses of the paper are done
-   R / 03_Figures_and_tables: Folder where figures and tables for the paper are produced
-   R / Functions: Scripts with R functions.
-   00_Config_file: file to define variables (e.g., data storage path)
-   Init_project.R: first file to run when setting up the project. Installs packages

## Data availability

data to run code can be downloaded from Zenodo (LINK). Set your data storage path in the 00_Config_file to the folder.

#### Raw data 

*mammal distribution data*: Mammal Diversity Database (MDD) Marsh et al., 2022; <https://onlinelibrary.wiley.com/doi/10.1111/jbi.14330>

*mammal elevation data*: Handbook of Mammals of the World. (machine readable version): <https://github.com/jhpoelen/hmw>

*reptile distribution data*: global assessment reptile distribution Caetano et al., 2022 and Roll et al., 2017: <https://datadryad.org/stash/dataset/doi:10.5061/dryad.9cnp5hqmb>

*reptile elevation data*: SquamBase, Meiri, 2024: <https://onlinelibrary.wiley.com/doi/10.1111/geb.13812>

*birds distribution data*: BirdLife International (after formal application): <https://www.birdlife.org/>

*birds elevation data*: Quintero&Jetz, 2018: <https://www.nature.com/articles/nature25794>

*Plants checklists with elevation data*: Global Inventory of Floras and Traits (GIFT) database: <https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.14213> and: <https://onlinelibrary.wiley.com/doi/10.1111/jbi.13623>

*GMBA Mountain inventory*: Snethlage et. al, 2022: https://ilias.unibe.ch/goto_ilias3_unibe_cat_1000515.html 


## Information on R/Folders

### Data_processing

*mammals, birds and reptiles*: Here, species distribution data is sourced. Then distribution ranges are overlapped with mountain ranges. Elevational ranges are sourced from the respective literature sources and approximated using DEM. Eventually, data is reshaped to prepare for manual expert validation process.

*Plants*: All plant data is sourced from the GIFT database. See detailed tutorials on how to use GIFT in R here: <https://biogeomacro.github.io/GIFT/articles/GIFT.html> and here: <https://biogeomacro.github.io/GIFT/articles/GIFT_advanced_users.html> We first sourced GIFT regions that contain alpine area and then joined elevational data for the species in these regions.

GIFT checklist data includes species checklists for GIFT regions (geo_entity), elevational data and polygons that delineate GIFT regions

### Main_analyses

*post_validataion_processing*: compile files that have been validated by experts.

*Plants*: Here, we categorize plants based on their elevational ranges relative to the upper forest line within each GIFT region as (alpine generalist, broad-montane species, mid-montane, UFL-alpine and alpine specialist). We then calculate absolute and relative richness for each GIFT region and the proportion of each category.

*Vertebrates*: Categorize mammals, birds and reptiles based on their elevational ranges relative the upper forest line within each mountain range as (alpine generalist, broad-montane species, mid-montane, UFL-alpine and alpine specialist). We then calculate absolute and relative richness for each mountain range and the proportion of each category

*Mountains*: Here, we calculate the lapse rates for each mountain range and for each GIFT region as *lm(temperature \~ elevation)*. We then calculate how the temperature difference of x degree translate in elevation meteres in each mountain range and gift region as *Elevation Change = Delta Temp/Lapse rate x 1000*

### Figures and tables

Contains all code to reproduce figures shown in the manuscript and the supplements

### Funding
This research has been developed for the PPF-Alpine project and is funded by the Trond Mohn Stiftelse and University of Bergen


### Contact
If you have any questions regarding the project, this repository or the publication, please do not hesitate to contact us! E-mail: lotta.schultz@outlook.com