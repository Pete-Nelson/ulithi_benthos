# Peter Nelson
# Institute for Marine Science
# University of California Santa Cruz

# purpose: read in raw benthic data, basic data manipulations

library(tidyverse)
library(readxl)

# read in raw .xlsx data
temp <- read_excel("data/BenthicData_all yrs compiled_2012-19_MASTER_missing 2015_2018_20220502.xlsx", 
                   sheet = "raw quadrat data yrs combined") %>% 
  mutate(across(c(1:4,6:8, 12:17, 22), factor), # convert some data to factors
         depth_ft = as.numeric(depth_ft), # depth to numeric
         across(c(23:26), as.character)) # notes to character
warnings() # check columns V, U, Z

# observe data structure
glimpse(temp)

levels(temp$site)  # check site names
levels(temp$sitecode) # check sitecodes; no ULAR4, ULAR5
levels(temp$benthic_surveyor)
levels(temp$fish_surveyor) # May AND Eric?
levels(temp$depth_zone)
summary(temp$depth_ft)
range(temp$depth_ft, na.rm = T)
