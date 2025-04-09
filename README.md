# Urban Stopover Analysis Using NEXRAD

This is the working repository for my dissertation chapter, which uses NEXRAD data to quantify migratory bird stopover density across the biggest cities in the continental U.S. All code was adapted from Maria Belotti's analysis on Atlanta Georgia, which can be found at: https://gitlab.com/mariabelotti/prjct_georgia_on_my_mind

This repository hosts the workflow used to carry out the spatial analysis, from data retrieval to producing the final dataset that is hosted (here). Below are further details on the repository structure, how to use the code base, and data sources.

Forward any questions regarding this project and code base to Mikko Jimenez (miguel.jimenez@colostate.edu)

## Description of scripts

**setup.R** - This script is used to load (and install, if necessary) packages used throughout the project. It is called at the beginning of all subsequent scripts.

**hotspot_percents.R** - This script was used to calculate resource selection ratios for the use of urban areas in migratory stopover across varying definitions of 'urban.' 

**stopoverXurban_mods.R** - This script was used to fit and summarize models estimating the relationship between urbanization levels and migratory bird stopover densities at the county level. 

**stopoverXincome_mods.R** - This script was used to fit and summarize models estimating the relationship between the proportion of high-income residents surrounding a park and its stopover density at the city- and continentual U.S.-levels. 
