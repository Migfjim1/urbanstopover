# Urban Stopover Analysis Using NEXRAD

This repository hosts the workflow used to carry out the spatial analysis for the study 'Divergent Effects of Urbanization and Social Landscapes on Continental Migratory Bird Stopover'. 

Forward any questions regarding this project and code base to Mikko Jimenez (miguel.jimenez@colostate.edu)

## Description of scripts

**setup.R** - This script is used to load (and install, if necessary) packages used throughout the project. It is called at the beginning of all subsequent scripts. Expected run time <5 seconds. 

**hotspot_percents.R** - This script was used to calculate resource selection ratios for the use of urban areas in migratory stopover across varying definitions of 'urban.' Expected run time ~45 seconds. 

**stopoverXurban_mods.R** - This script was used to fit and summarize models estimating the relationship between urbanization levels and migratory bird stopover densities at the county level. Expected run time ~5 minutes.

**stopoverXincome_mods.R** - This script was used to fit and summarize models estimating the relationship between the proportion of high-income residents surrounding a park and its stopover density at the city- and continentual U.S.-levels. Expected run time ~7 minutes. 

*All scripts have been test in R Versions 2024.04.2+764 (2024.04.2+764) and 2025.05.1+513 (2025.05.1+513)

## Data availability
Data for this analysis can be found at: https://doi.org/10.5061/dryad.1jwstqk68
