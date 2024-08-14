### setup script - call in other scripts to install/load necessary packages

## define the packageLoad function
packageLoad <-
  function(x) {
    for (i in 1:length(x)) {
      if (!x[i] %in% installed.packages()) {
        install.packages(x[i])
      }
      library(x[i], character.only = TRUE)
    }
  }

# create a string of package names 
packages <- c('tidyverse',
              'tidyr',
              'dplyr',
              'sf',
              'lubridate',
              'terra',
              'stringr',
              'daymetr',
              'tictoc',
              'data.table',
              'terra',
              'sf',
              'ggspatial',
              'scales',
              'cowplot',
              'ggplot2',
              'GGally',
              'stringr',
              'caret',
              'tictoc',
              'viridis')

# run packageLoad function on packages string
packageLoad(packages)
