#### load packages
packages <-  c("tidyverse", "vegan", "NPSdataverse", "here")
lapply(packages, library, character.only = T)


####
projections <- read.csv("Daily_streamflow.csv", row.names = 1)
