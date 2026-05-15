# General info

This repo contains all R code used to process and analyse data for the outputs of the article "Contrasting patterns of alpine biodiversity across mountains and taxa" (Schultz et. al, under review). This work has been developed for the [Past, Present and Future of Alpine Biomes Worldwide (PPF) project](https://mountainsinmotion.w.uib.no/)! 🏔🌎

## Main structure of the Repo

This repo follows a template structure for R projects developed by [Ondrej Mottl](<https://ondrejmottl.github.io/>) and [Suzette Flantua](<https://sflantua.github.io/>)
-   R / 01_Data_processing: Folder for sourcing, restructuring and joining data to create the species checklists
-   R / 02_Main_analyses: Folder where main (richness) analyses of the paper are done
-   R / 03_Figures_and_tables: Folder where figures and tables for the paper are produced
-   R / Functions: Scripts with R functions.
-   00_Config_file: file to define variables (e.g., data storage path)
-   Init_project.R: first file to run when setting up the project. Installs packages

## Data availability

To run the code download data folder from [figshare](https://figshare.com/s/3d4dcca576f6a73668b8) and set your data storage path in the 00_Config_file to the downloaded data folder.

The raw data was obtained from the following sources: 

#### Mammal Data
Distribution data: Mammal Diversity Database (MDD; version 1.11; accessed May, 2023)
Mammal Diversity Database. (2023). Mammal Diversity Database (1.11) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.7830771 
Marsh, C. J., Sica, Y. V., Burgin, C. J., Dorman, W. A., Anderson, R. C., et al. (2022). Expert range maps of global mammal distributions harmonised to three taxonomic authorities. Journal of Biogeography, 49, 979–992. https://doi.org/10.1111/jbi.14330
Elevation data: Elevational data were compiled from the Handbook of the Mammals of the World series (Wilson & Mittermeier, 2009–2019) using the machine-readable extraction provided by J. H. Poelen (accessed June, 2023): jhpoelen/hmw GitHub repository

#### Reptile Data
Distribution data: 
Global Assessment of Reptile Distributions (GARD; version 1.7, accessed June, 2023)
Roll, Uri; Meiri, Shai (2022). GARD 1.7 - updated global distributions for all terrestrial reptiles [Dataset]. Dryad. https://doi.org/10.5061/dryad.9cnp5hqmb 
Roll, U., Feldman, A., Novosolov, M., Allison, A., Bauer, A. M., Bernard, R., Böhm, M., Castro-Herrera, F., Chirio, L., Collen, B., Colli, G. R., et al. (2017). The global distribution of tetrapods reveals a need for targeted reptile conservation. Nature Ecology & Evolution, 1, 1677–1682. https://doi.org/10.1038/s41559-017-0332-2 
de Oliveira Caetano, G. H., et al. (2022). Automated assessment reveals that the extinction risk of reptiles is widely underestimated across space and phylogeny. PLOS Biology, 20(5), e3001544. https://doi.org/10.1371/journal.pbio.3001544 
Elevation data: 
SquamBase (version 1.0, accessed June, 2023 (provided by S. Meiri prior to publication and accessed June 2023); Meiri, 2024)
Meiri, Shai (2024). SquamBase 1.0 – a dataset of squamate species traits [Dataset]. Dryad. https://doi.org/10.5061/dryad.76hdr7t3b 
Meiri, S. (2024). SquamBase—A database of squamate (Reptilia: Squamata) traits. Global Ecology and Biogeography, 33(4), e13812. https://doi.org/10.1111/geb.13812 

#### Bird Data
Distribution data: 
BirdLife International Data Zone (version 2022.2, accessed May 2023)
BirdLife International and Handbook of the Birds of the World (2022) Bird species distribution maps of the world. Version 2022.2. Available at http://datazone.birdlife.org/species/requestdis.

Elevation data: Quintero, I., Jetz, W. Global elevational diversity and diversification of birds. Nature 555, 246–250 (2018). https://doi.org/10.1038/nature25794 (Supplementary Table 1 version of record, 2018, accessed June 2023)

#### Plant Data
Regional plant checklists: Global Inventory of Floras and Traits (GIFT) database (version 3.0, access provided by P. Denelle prior to publication and accessed April 2023)
Weigelt, P., König, C. & Kreft, H. (2020) GIFT - A Global Inventory of Floras and Traits for macroecology and biogeography. Journal of Biogeography, 47, 16-43. DOI: 10.1111/jbi.13623 

#### Mountain Data
Mountain inventory: GMBA Mountain Inventory (version 2, accessed April 2023)
Snethlage, M.A., Geschke, J., Spehn, E.M., Ranipeta, A., Yoccoz, N.G., Körner, Ch., Jetz, W., Fischer, M. & Urbach, D. (2022). A hierarchical inventory of the world’s mountains for global comparative mountain science. Nature Scientific Data. https://doi.org/10.1038/s41597-022-01256-y 
Snethlage, M.A., Geschke, J., Spehn, E.M., Ranipeta, A., Yoccoz, N.G., Körner, Ch., Jetz, W., Fischer, M. & Urbach, D. (2022) GMBA Mountain Inventory v2. GMBA-EarthEnv. https://doi.org/10.48601/earthenv-t9k2-1407



## Information on R/Folders

### Data_processing

*mammals, birds and reptiles*: Here, species distribution data is sourced. Then distribution ranges are overlapped with mountain ranges. Elevational ranges are sourced from the respective literature sources and approximated using DEM. Eventually, data is reshaped to prepare for manual expert validation process.

*Plants*: All plant data is sourced from the GIFT database. See detailed tutorials on how to use GIFT in R [here](<https://biogeomacro.github.io/GIFT/articles/GIFT.html>) and [here](<https://biogeomacro.github.io/GIFT/articles/GIFT_advanced_users.html>) We first sourced GIFT regions that contain alpine area and then joined elevational data for the species in these regions.

GIFT checklist data includes species checklists for GIFT regions (geo_entity), elevational data and polygons that delineate GIFT regions

### Main_analyses

*post_validation_processing*: compile files that have been validated by experts.

*Plants*: Here, we categorize plants based on their elevational ranges relative to the upper forest line within each GIFT region as alpine generalist, broad-montane species, mid-montane, UFL-alpine and alpine specialist. We then calculate absolute and relative richness for each GIFT region and the proportion of each category.

*Vertebrates*: Categorize mammals, birds and reptiles based on their elevational ranges relative the upper forest line within each mountain range as alpine generalist, broad-montane species, mid-montane, UFL-alpine and alpine specialist. We then calculate absolute and relative richness for each mountain range and the proportion of each category

*Mountains*: Here, we calculate the lapse rates for each mountain range and for each GIFT region as *lm(temperature \~ elevation)*. We then calculate how the temperature difference of x degree translate in elevation meteres in each mountain range and gift region as *Elevation Change = Delta Temp/Lapse rate x 1000*

### Figures and tables

Contains all code to reproduce figures shown in the manuscript and the supplements

### Funding
This research has been developed for the PPF-Alpine project and is funded by the [Trond Mohn Stiftelse](https://mohnfoundation.no/) and University of Bergen, Norway. 


### Contact
If you have any questions regarding the project, this repository or the publication, please do not hesitate to contact us! E-mail: lotta.schultz@outlook.com
