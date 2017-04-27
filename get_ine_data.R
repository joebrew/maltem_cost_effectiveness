library(tidyverse)
library(memisc)

ine <- as.data.set(spss.system.file('data/ine/Censo_Database.sav'))
save(ine,
     file = 'data/ine/Censo_Database.RData')